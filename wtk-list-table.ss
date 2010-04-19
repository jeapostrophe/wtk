(module wtk-list-table mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "plt-match.ss")
           (lib "etc.ss")
           (lib "kw.ss")
           (lib "list.ss")
           (lib "struct.ss"))
  (require (planet "list.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "string.ss" ("jaymccarthy" "mmss.plt" 1)))
  (require "wtk-list.ss")
  (provide (all-defined)
           (all-from "wtk-list.ss"))
  
  ; table-column : symbol string string (a -> html) (a a -> boolean)
  (define-struct table-column (id short-name long-name obj->html obj-<=))
  
  ; interleave : number (a -> html)
  (define-struct interleave (every gen))
  
  (define-struct table-ui-state (list-ui-state
                                 objs-per-page current-page
                                 highlight-pred? emphasis-pred?))
  (define (with-table-ui:initial-state initial-sort)
    (make-web-cell:local
     ; XXX: How do we know it's a local cell?
     (make-table-ui-state (with-list-ui:initial-state initial-sort)
                          10 0
                          (lambda (o) #f)
                          (lambda (o) #f))))
  (define table-ui-state->string
    (match-lambda
      [(struct table-ui-state (lus/lc objs current-page _0 _1))
       (write/string (list (list-ui-state->string (web-cell:local-ref lus/lc))
                           objs current-page))]))
  (define (string->table-ui-state s)
    (match (read/string s)
      [(list lus/s objs current-page)
       (make-table-ui-state (make-web-cell:local (string->list-ui-state lus/s))
                            objs current-page
                            (lambda (o) #f)
                            (lambda (o) #f))]))
  
  (define/kw (with-table-ui embed/url 
                            table-ui-state-cell css-prefix
                            columns filters interleaves
                            gen-objs generate-filter-div
                            #:key
                            [stable? #f]
                            [sort mergesort]
                            [and-filters? #t])
    (define the-list-ui 
      (with-list-ui (table-ui-state-list-ui-state
                     (web-cell:local-ref table-ui-state-cell))
                    ; filters
                    filters
                    ; sorters
                    (map (lambda (a-column)
                           (make-list-sort (table-column-id a-column) (table-column-obj-<= a-column)
                                           (table-column-id a-column)))
                         columns)
                    gen-objs
                    #:stable? stable?
                    #:sort sort
                    #:and-filters? and-filters?))
    (define (change-page/cell new-page)
      (web-cell:local-mask table-ui-state-cell
                           (copy-struct table-ui-state (web-cell:local-ref table-ui-state-cell)
                                        [table-ui-state-current-page new-page])))
    (define (change-page-size/cell new-size)
      (define old-size (table-ui-state-objs-per-page (web-cell:local-ref table-ui-state-cell)))
      (define old-page (table-ui-state-current-page (web-cell:local-ref table-ui-state-cell)))
      (define displayed-paper (* old-size old-page))
      (define new-page (floor (/ displayed-paper new-size)))
      (web-cell:local-mask table-ui-state-cell
                           (copy-struct table-ui-state (web-cell:local-ref table-ui-state-cell)
                                        [table-ui-state-current-page new-page]
                                        [table-ui-state-objs-per-page new-size])))
    (let/cc k
      (define ((change-page new-page) request)
        (change-page/cell new-page)
        (redirect/get)
        (generate))
      (define ((change-page-size new-size) request)
        (change-page-size/cell new-size)
        (redirect/get)
        (generate))
      (define (generate)  
        (define (css-class x) (format "~a-~a" css-prefix x))
        (define (css-column-class a-column) (format "~a" (table-column-id a-column)))
        (define objs:count (length (list-ui-objects the-list-ui)))
        (define itus (web-cell:local-ref table-ui-state-cell))
        (define last-page (sub1 (ceiling (/ objs:count (table-ui-state-objs-per-page itus)))))
        (define per-page (table-ui-state-objs-per-page itus))
        (define maybe-current-page (table-ui-state-current-page itus))
        (define current-page
          (if ((* per-page maybe-current-page) . > . objs:count)
              (let ([new-page (floor (/ objs:count per-page))])
                (web-cell:local-mask table-ui-state-cell
                                     (copy-struct table-ui-state (web-cell:local-ref table-ui-state-cell)
                                                  [table-ui-state-current-page new-page]))
                (redirect/get)
                new-page)
              maybe-current-page))
        (define obj-start
          (* per-page current-page))
        (define limited-objs
          (list-splice (list-ui-objects the-list-ui)
                       obj-start
                       (+ obj-start per-page)))
        (define movement-div
          `(div ([class ,(css-class "movement")])
                "Page: "
                (span ([class ,(css-class "movement-prev")])
                      ,(if (> current-page 0)
                           `(a ([href ,(embed/url (change-page (sub1 current-page)))])
                               larr nbsp "Previous")
                           `(span larr nbsp "Previous")))
                nbsp nbsp
                (span ([class ,(css-class "pages")])
                      ,@(map (lambda (page)
                               `(span
                                 ,(if (not (= page current-page))
                                      `(a ([href ,(embed/url (change-page page))])
                                          ,(number->string (add1 page)))
                                      `(span ([class "current"]) ,(number->string (add1 page))))
                                 " "))
                             (build-list (add1 last-page)
                                         (lambda (x) x))))
                nbsp nbsp
                (span ([class ,(css-class "movement-next")])
                      ,(if (< current-page last-page)
                           `(a ([href ,(embed/url (change-page (add1 current-page)))])
                               "Next" nbsp rarr)
                           `(span "Next" nbsp rarr)))))
        (define (empty-row content)
          `(tr (td ([class "blank"]) nbsp)
               (td ([class "blank-line"] [colspan ,(number->string (sub1 (length columns)))])
                   ,content)))
        (k `(div
             ,(generate-filter-div the-list-ui)
             ,movement-div
             (div ([class ,(css-class "count")])
                  "Show "
                  ,@(map (lambda (n)
                           `(span 
                             ,(if (eq? n (table-ui-state-objs-per-page itus))
                                  `(span ([class ,(css-class "count-current")])
                                         ,(number->string n))
                                  `(a ([href ,(embed/url (change-page-size n))]
                                       [title ,(format "Show ~a rows at a time" n)])
                                      ,(number->string n)))
                             nbsp))
                         ; XXX: `all' and put in interface
                         `(5 10 25 50 75 100 200 300))
                  "of "
                  ,(number->string objs:count))
             (table ([class ,(css-class "table")])
                    (thead
                     ,@(map (match-lambda
                              [(? table-column-obj-<= a-column)
                               (define sorted? ((list-ui-sorted-by? the-list-ui) (table-column-id a-column)))
                               (define reversed? ((list-ui-sort-reversed? the-list-ui)))
                               (define title (if sorted?                                 
                                                 (format "Reverse sort by ~a" (table-column-long-name a-column))
                                                 (format "Sort by ~a" (table-column-long-name a-column))))
                               (define proc ((list-ui-sort-by the-list-ui) (table-column-id a-column)))
                               `(th ([class ,(css-column-class a-column)]
                                     [style ,(if ((list-ui-sorted-by? the-list-ui) a-column)
                                                 "text-decoration: underline;"
                                                 "")]
                                     [scope "column"])
                                    (a ([class "k-url"] [href ,(embed/url proc)] [title ,title])
                                       ,(table-column-short-name a-column))
                                    nbsp
                                    ,@(if (not sorted?)
                                          empty
                                          `((span ([class ,(css-class "column-arrow")])
                                                  (a ([class "k-url"] [href ,(embed/url proc)] [title ,title])
                                                     ,(if reversed?
                                                          'uarr
                                                          'darr))))))]
                              [a-column
                               `(th ([class ,(css-column-class a-column)]
                                     [scope "column"])
                                    ,(table-column-short-name a-column))])
                            columns))
                    (tbody
                     ,@(apply append
                              (map (lambda (obj i)
                                     (define ze-interleaves
                                       (apply append
                                              (map (match-lambda
                                                     [(struct interleave (every fun))
                                                      (cond
                                                        [(or (and (number? every) (= 0 (modulo i every)))
                                                             (and (symbol? every) (eq? 'every every)))
                                                         (list (fun obj))]
                                                        [else
                                                         (list)])])
                                                   interleaves)))
                                     `((tr ([class ,(css-class "row")]
                                            [style ,(string-append
                                                     (if ((table-ui-state-emphasis-pred? itus) obj)
                                                         "font-weight:bold;" "")
                                                     (if ((table-ui-state-highlight-pred? itus) obj)
                                                         "background-color:#ff6;" ""))])
                                           ,@(map (lambda (a-column)
                                                    `(td ([valign "top"]
                                                          [class ,(css-column-class a-column)])
                                                         ,((table-column-obj->html a-column) obj)))
                                                  columns))
                                       ,@(if (empty? ze-interleaves)
                                             (list (empty-row 'nbsp))
                                             ze-interleaves)))
                                   limited-objs
                                   (build-list (length limited-objs) add1)))))
             ,movement-div)))
      (generate)))
  
  (define with-table-ui/embed/url with-table-ui))