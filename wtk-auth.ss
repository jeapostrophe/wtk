(module wtk-auth mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "response-structs.ss" "web-server")
           (lib "list.ss"))
  (provide (all-defined))
  
  ; login : string string string
  (define-struct login (ip username password))
  
  ; require-authentication : (login -> boolean) string (login -> (listof string)) (login -> a) -> request -> a
  (define (require-authentication authorized? realm fail-content-f success-f)
    (lambda (request)
      (let* ([login-info (extract-user-pass (request-headers request))]
             [login (if login-info
                        (make-login (request-client-ip request)
                                    (bytes->string/utf-8 (car login-info))
                                    (bytes->string/utf-8 (cdr login-info)))
                        #f)]
             [is-authorized? (if login
                                 (authorized? login)
                                 #f)])
        (if is-authorized?
            (success-f login)
            (let ([fail-content (fail-content-f login)])
              (send/finish
               (make-response/full
                401 "Password Required" (current-seconds) (first fail-content)
                `((WWW-Authenticate . ,(format "Basic realm=\"~a\"" realm)))
                (rest fail-content)))))))))
