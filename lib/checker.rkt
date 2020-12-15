#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    threading
    racket/function
    syntax/parse
    syntax/strip-context
    "scope.rkt"
    (prefix-in meta/ "meta.rkt")))

(provide begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (replace-context stx ((checker stx))))

(begin-for-syntax
  (define (check-all lst)
    (map (λ (f) (f)) lst))

  (define current-entity-name (make-parameter #f))

  (define (checker stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       (define body^ (with-scope
                       (~>> (attribute body)
                            (map add-scope)
                            (map checker))))
       (thunk
         #`(begin
             #,@(check-all body^)))]

      [:stx/entity
       ; An entity does not introduce a new scope.
       ; Its ports are collected into a dictionary for later use.
       (bind! #'name (meta/make-entity
                       (for/hash ([p (in-list (attribute port))])
                         (define/syntax-parse q:stx/port p)
                         (values #'q.name (meta/port (syntax->datum #'q.mode))))))
       (thunk stx)]

      [:stx/architecture
       (bind! #'name (meta/architecture #'ent-name))
       (define body^ (with-scope
                       (~>> (attribute body)
                            (map add-scope)
                            (map checker))))
       (thunk/in-scope
         ; Check that ent-name refers to an entity.
         (lookup #'ent-name meta/entity?)
         ; Check the architecture body, providing the current entity name
         ; as a parameter, for name resolution in port references.
         (parameterize ([current-entity-name #'ent-name])
           #`(architecture name ent-name
               #,@(check-all body^))))]

      [:stx/instance
       (bind! #'name (meta/instance #'arch-name))
       (thunk/in-scope
         ; Check that arch-name refers to an architecture.
         (lookup #'arch-name meta/architecture?)
         stx)]

      [:stx/assignment
       (define target^ (checker #'target))
       (define expr^   (checker #'expr))
       (thunk
         #`(assign #,(target^) #,(expr^)))]

      [:stx/operation
       (define arg^ (map checker (attribute arg)))
       (thunk
         #`(op #,@(check-all arg^)))]

      [(inst-name:id port-name:id)
       (thunk/in-scope
         (define/syntax-parse ent-name
           (~> #'inst-name
               ; Check that inst-name refers to an instance.
               (lookup meta/instance?)
               ; Check that the architecture name for that instance refers to an architecture.
               (meta/instance-arch-name)
               (lookup meta/architecture?)
               ; Get the entity name for that architecture
               (meta/architecture-ent-name)))
         (~> #'ent-name
             ; Check that the entity name for that architecture refers to an entity.
             (lookup meta/entity?)
             ; Check that port-name is the name of a port in that entity.
             (meta/entity-port-ref #'port-name))
         ; Return a fully-resolved port-ref form.
         #'(port-ref ent-name port-name inst-name))]

      [port-name:id
       (thunk/in-scope
         ; Check that port-name refers to a port in the entity of the current architecture.
         (define/syntax-parse ent-name (current-entity-name))
         (~> #'ent-name
             ; We already know that the current entity name refers to an entity.
             (lookup)
             ; Check that port-name is the name of a port in that entity.
             (meta/entity-port-ref #'port-name))
         ; Return a fully-resolved port-ref form.
         #'(port-ref ent-name port-name))]

      [_ (thunk stx)])))
