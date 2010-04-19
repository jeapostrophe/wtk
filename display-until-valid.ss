(module display-until-valid mzscheme
  (require (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "list.ss")
           (lib "servlet.ss" "web-server"))
  (require (planet "number.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "maybe.ss" ("jaymccarthy" "mmss.plt" 1)))
  (provide interface-version
           timeout
           start)
  
  ; data
  (define-struct (exn:fail:contract:form exn:fail:contract) ())
  (define (raise-form-error msg)
    (raise (make-exn:fail:contract:form msg (current-continuation-marks))))
  
  ; field : a * (request -> a)
  (define-struct field (init extract))
  
  ; test
  (define interface-version 'v1)
  (define timeout 60)
  (define (start init-request)
    (match (form/macro)
      [(list name age)
       (send/back
        `(html (body (h1 "Success")
                     (h2 ,name)
                     (h3 ,(number->string age)))))]))
  
  (define (form/macro)
    (send/suspend/dispatch
     (lambda (embed/url)
       `(html 
         (body
          ,(display-until-valid
            embed/url request errors
            embed/url/form
            ([name "Init"
                   (match (extract-binding/single
                           'name
                           (request-bindings request))
                     [(regexp "^[a-zA-Z0-9]+$" (list s))
                      s]
                     [_
                      (raise-form-error "Error")])]
             [age 18
                  (with-handlers ([exn:fail? (lambda _ (raise-form-error "Error"))])
                    (string->number*
                     (extract-binding/single 
                      'age 
                      (request-bindings request))))])
            `(form ([method "POST"]
                    [action 
                     ,(embed/url/form
                       (lambda ()
                         (list name age)))])
                   ,@(map (match-lambda
                            [(struct just (err))
                             `(h2 ,err)])
                          (filter just? errors))
                   (input ([name "name"]
                           [type "text"]
                           [value ,name]))
                   (input ([name "age"]
                           [type "text"]
                           [value ,(number->string age)]))
                   (input ([type "submit"])))))))))
  
  (define (form)
    (send/suspend/dispatch
     (lambda (embed/url)
       (define name (make-field 
                     ""
                     (lambda (req)
                       (match (extract-binding/single
                               'name
                               (request-bindings req))
                         [(regexp "^[a-zA-Z0-9]+$" (list s))
                          s]
                         [_
                          (raise-form-error "Error")]))))
       (define age (make-field 
                    18
                    (lambda (req)
                      (with-handlers ([exn:fail? 
                                       (lambda _ 
                                         (raise-form-error "Error"))])
                        (string->number*
                         (extract-binding/single 
                          'age 
                          (request-bindings req)))))))
       `(html 
         (body
          ,(display-until-valid*
            embed/url
            (list name age)
            (match-lambda*
              [(list embed/url errors (list name age))
               `(form ([method "POST"]
                       [action 
                        ,(embed/url
                          (match-lambda*
                            [(list request (list name age))
                             (list name age)]))])
                      ,@(map (match-lambda
                               [(struct just (err))
                                `(h2 ,err)])
                             (filter just? errors))
                      (input ([name "name"]
                              [type "text"]
                              [value ,name]))
                      (input ([name "age"]
                              [type "text"]
                              [value ,(number->string age)]))
                      (input ([type "submit"])))])))))))
  
  (define-syntax display-until-valid
    (syntax-rules ()
      [(_ embed/url request errors
          embed/url/form
          ([field init extract]
           ...)
          form)
       (let ([field (make-field 
                     init
                     (lambda (request)
                       extract))]
             ...)
         (display-until-valid*
          embed/url
          (list field ...)
          (lambda (basic-embed/url errors the-values)
            (define-syntax (embed/url/form stx)
              (syntax-case stx (lambda)
                [(e/u/f (lambda () body (... ...)))
                 (with-syntax ([field (datum->syntax-object (syntax e/u/f) 'field)]
                               ...)
                   #`(basic-embed/url
                      (lambda (request the-new-values)
                        (let-values ([(field ...) (apply values the-new-values)])
                          body (... ...)))))]))
            (let-values ([(field ...) (apply values the-values)])
              form))))]))
  
  ; extractv a = Okay a | Fail msg
  (define-struct extractv ())
  (define-struct (extractv:okay extractv) (value))
  (define-struct (extractv:fail extractv) (msg))
  
  ; display-until-valid* : embed/url (list (field a^)) ((request (list a^)) -> alpha) -> alpha
  (define display-until-valid*
    (match-lambda*
      [(list embed/url 
             (list (struct field (init extract)) ...)
             gen-form)       
       (let/cc k
         (let loop ([errors empty]
                    [values init])
           (define (new-embed/url proc)
             (embed/url
              (lambda (request)
                (redirect/get)
                (let* ([extracts
                        (map (lambda (extract)
                               (with-handlers ([exn:fail:contract:form?
                                                (lambda (exn)
                                                  (make-extractv:fail (exn-message exn)))])
                                 (make-extractv:okay (extract request))))
                             extract)]
                       [okay? 
                        (andmap extractv:okay?
                                extracts)]
                       [errors
                        (map (match-lambda
                               [(struct extractv:okay (v))
                                (make-nothing)]
                               [(struct extractv:fail (msg))
                                (make-just msg)])
                             extracts)]
                       [values
                        (map (match-lambda*
                               [(list (struct extractv:okay (new)) cur)
                                new]
                               [(list (struct extractv:fail (msg)) cur)
                                cur])
                             extracts
                             values)])                  
                  (if okay?
                      (proc request values)
                      (k (loop errors values)))))))
           (gen-form
            new-embed/url
            errors values)))])))