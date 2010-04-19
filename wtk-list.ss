(module wtk-list mzscheme
  (require (lib "servlet.ss" "web-server")
           (lib "struct.ss")
           (lib "kw.ss")
           (lib "plt-match.ss")
           (lib "list.ss"))
  (require (planet "list.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "string.ss" ("jaymccarthy" "mmss.plt" 1))
           (planet "sort.ss" ("jaymccarthy" "mmss.plt" 1)))
  
  (provide with-list-ui
           with-list-ui:initial-state
           list-ui-state->string
           string->list-ui-state
           list-sort?
           make-list-sort
           list-filter?
           make-list-filter)
  (provide list-ui?
           list-ui-objects
           list-ui-map-filters list-ui-clear-filters list-ui-filter-by list-ui-filtered-by?
           list-ui-map-sorts list-ui-sort-by list-ui-sorted-by? list-ui-sort-reversed?)
  
  ; sort : symbol (a a -> boolean) b
  (define-struct list-sort (id <= tag))
  ; filter : symbol (a -> boolean) b
  (define-struct list-filter (id pred? tag))
  
  (define-struct list-ui-state (sort-id reverse-sort? filters))
  (define (with-list-ui:initial-state initial-sort)
    (make-web-cell:local (make-list-ui-state initial-sort #f (list))))
  (define list-ui-state->string
    (match-lambda
      [(struct list-ui-state (current reverse? filters))
       (write/string (list current reverse? filters))]))
  (define (string->list-ui-state s)
    (match (read/string s)
      [(list current reverse? filters)
       (make-list-ui-state current reverse? filters)]))    
  
  ; list-ui
  (define-struct list-ui (objects
                          map-filters clear-filters filter-by filtered-by?
                          map-sorts sort-by sorted-by? sort-reversed?))
  
  ; with-list-ui : (local-cell list-ui-state) (list (filter a b)) (list (sort a)) (-> (list a)) -> list-ui
  (define/kw (with-list-ui state-cell filters sorts gen-objects
                           #:key
                           [stable? #f]
                           [sort mergesort]
                           [and-filters? #t])
    (define the-sort
      (if stable?
          (unstable->stable sort)
          sort))
    (let/cc k
      (letrec ([gen-filter-test
                (lambda ()
                  (define preds 
                    (map (lambda (a-filter-id) 
                           (define list-filter ((assoc/proj/cmp list-filter-id eq? filters) a-filter-id))
                           (if list-filter
                               (list-filter-pred? list-filter)
                               (begin 
                                 ; This means the filter no longer exists
                                 ; XXX: A better behaviour is to remove it from the list.
                                 (lambda (x) #t))))
                         (list-ui-state-filters (web-cell:local-ref state-cell))))                  
                  (lambda (x)
                    (if (empty? preds)
                        #t
                        (if and-filters?
                            (foldl (lambda (pred? acc)
                                     (and acc (pred? x)))
                                   #t
                                   preds)
                            (foldl (lambda (pred? acc)
                                     (or acc (pred? x)))
                                   #f
                                   preds)))))]
               [clear-filters
                (lambda (request)
                  (web-cell:local-mask
                   state-cell
                   (copy-struct list-ui-state (web-cell:local-ref state-cell)
                                [list-ui-state-filters (list)]))
                  (redirect/get)
                  (generate))]
               [filtered-by?
                (lambda (the-filter-id)
                  (memq the-filter-id (list-ui-state-filters (web-cell:local-ref state-cell))))]
               [filter-by
                (lambda (the-filter-id)
                  (lambda (request)
                    (define state (web-cell:local-ref state-cell))
                    (web-cell:local-mask
                     state-cell
                     (copy-struct list-ui-state state
                                  [list-ui-state-filters 
                                   (if (filtered-by? the-filter-id)
                                       (filter (lambda (a-filter-id)
                                                 (not (eq? a-filter-id the-filter-id)))
                                               (list-ui-state-filters state))
                                       (list* the-filter-id
                                              (list-ui-state-filters state)))]))
                    (redirect/get)
                    (generate)))]
               [map-filters
                ; (special filtered?/boolean filter-f -> alpha) -> list-of alpha
                (lambda (f)
                  (map (lambda (a-filter)
                         (f (list-filter-tag a-filter)
                            (filtered-by? (list-filter-id a-filter))
                            (filter-by (list-filter-id a-filter))))
                       filters))]
               ; sorts
               [sorted-by?
                (lambda (the-sort-id)
                  (eq? (list-ui-state-sort-id (web-cell:local-ref state-cell))
                       the-sort-id))]
               [sort-reversed?
                (lambda ()
                  (list-ui-state-reverse-sort? (web-cell:local-ref state-cell)))]
               [sort-by
                (lambda (the-sort-id)
                  (lambda (request)
                    (web-cell:local-mask state-cell
                                         (copy-struct list-ui-state
                                                      (web-cell:local-ref state-cell)
                                                      [list-ui-state-reverse-sort?
                                                       (if (sorted-by? the-sort-id)
                                                           ; Switch the not state if it's equal
                                                           (not (sort-reversed?))
                                                           ; Default to normal order
                                                           #f)]
                                                      [list-ui-state-sort-id the-sort-id]))
                    (redirect/get)
                    (generate)))]
               [map-sorts
                (lambda (f)
                  (map (lambda (a-sort)
                         (f (list-sort-tag a-sort)
                            (sorted-by? (list-sort-id a-sort))
                            (sort-by (list-sort-id a-sort))))
                       sorts))]
               ; generate
               [generate 
                (lambda ()
                  (define filtered-objs (filter (gen-filter-test) (gen-objects)))
                  (define sorted-objects
                    (foldl (lambda (a-sort objs)
                             (if (sorted-by? (list-sort-id a-sort))
                                 (the-sort objs
                                           (if (sort-reversed?)
                                               (lambda (a b) (not ((list-sort-<= a-sort) a b)))
                                               (list-sort-<= a-sort)))
                                 objs))
                           filtered-objs
                           sorts))
                  (k (make-list-ui
                      sorted-objects
                      map-filters clear-filters filter-by filtered-by?
                      map-sorts sort-by sorted-by? sort-reversed?)))])
        (generate)))))