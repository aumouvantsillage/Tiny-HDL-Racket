#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    racket
    threading
    racket/function
    syntax/parse
    syntax/strip-context
    "scope.rkt"
    (prefix-in meta/ "meta.rkt")))

(provide begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  #`(begin
      #,(replace-context stx ((checker stx)))
      (require
        tiny-hdl/lib/scope
        (prefix-in meta/ tiny-hdl/lib/meta))
      (provide #%tiny-hdl-export)
      (define (#%tiny-hdl-export)
        #,@(make-export-body))))

(begin-for-syntax
  (define (check-all lst)
    (map (λ (f) (f)) lst))

  (define current-entity-name (make-parameter #f))
  (define assignment-targets  (make-parameter #f))

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

      [:stx/use
       (define path-str (syntax->datum #'path))
       (define import   (dynamic-require path-str '#%tiny-hdl-export))
       (import)
       (thunk stx)]

      [e:stx/entity
       ; An entity does not introduce a new scope.
       ; Its ports are collected into a dictionary for later use.
       (bind/export! #'e.name (meta/make-entity
                                (for/hash ([p (in-list (attribute e.port))])
                                  (define/syntax-parse q:stx/port p)
                                  (values #'q.name (meta/port (syntax->datum #'q.mode))))))
       (thunk stx)]

      [a:stx/architecture
       (bind! #'a.name (meta/architecture #'a.ent-name))
       (define body^ (with-scope
                       (~>> (attribute a.body)
                            (map add-scope)
                            (map checker))))
       (thunk/in-scope
         ; Check that ent-name refers to an entity.
         (lookup #'a.ent-name meta/entity?)
         ; Check the architecture body, providing the current entity name
         ; as a parameter, for name resolution in port references.
         (parameterize ([current-entity-name #'a.ent-name]
                        [assignment-targets (collect-assignment-targets (attribute a.body))])
           ; Check that all output ports of the entity are assigned
           ; in the current architecture.
           (check-all-assigned stx)
           #`(architecture a.name a.ent-name
               #,@(check-all body^))))]

      [i:stx/instance
       (bind! #'i.name (meta/instance #'i.arch-name))
       (thunk/in-scope
         ; Check that arch-name refers to an architecture.
         (define arch (lookup #'i.arch-name meta/architecture?))
         ; Check that all input ports of the entity for that architecture
         ; are assigned in the current architecture.
         (check-all-assigned stx #'i.name (meta/architecture-ent-name arch))
         stx)]

      [a:stx/assignment
       (define target^ (checker #'a.target))
       (define expr^   (checker #'a.expr))
       (thunk/in-scope
         ; Check that the target port of an assignment has the appropriate mode:
         ; * output: in an assignment to a port of the current architecture,
         ; * input:  in an assignment to a port of an instance.
         (define target* (target^))
         (define/syntax-parse (_ ent-name port-name (~optional inst-name)) target*)
         (define expected-mode (if (attribute inst-name) 'input 'output))
         (define actual-mode (~> #'ent-name
                                 (lookup)
                                 (meta/entity-port-ref #'port-name)
                                 (meta/port-mode)))
         (unless (eq? expected-mode actual-mode)
           (raise-syntax-error (syntax->datum #'port-name) "Invalid target for assignment" stx))
         #`(assign #,target* #,(expr^)))]

      [o:stx/operation
       (define arg^ (map checker (attribute o.arg)))
       (thunk
         #`(o.op #,@(check-all arg^)))]

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

      [_ (thunk stx)]))

  ; Collect the assignment targets in a given architecture, as a set.
  ; This function also checks that the same port is not assigned twice.
  (define (collect-assignment-targets stmt-lst)
    (for/fold ([acc  (set)])
              ([stmt (in-list stmt-lst)])
      (syntax-parse stmt
        [:stx/assignment
         (define port-id (syntax->datum #'target))
         (when (set-member? acc port-id)
           (define port-name (if (list? port-id) (second port-id) port-id))
           (raise-syntax-error port-name "Port is assigned more than one time" #'target))
         (set-add acc port-id)]

        [_ acc])))

  ; Check that all output ports of the current architecture,
  ; or all input ports of a given instance, are assigned.
  (define (check-all-assigned ctx [inst-name #f] [ent-name (current-entity-name)])
    (define mode (if inst-name 'input 'output))
    (for ([(port-name port) (in-dict (meta/entity-ports (lookup ent-name meta/entity?)))]
          #:when (eq? mode (meta/port-mode port)))
      (define port-name^ (syntax->datum port-name))
      (define port-id (if inst-name
                        (list (syntax->datum inst-name) port-name^)
                        port-name^))
      (unless (set-member? (assignment-targets) port-id)
        (raise-syntax-error port-name^ "Port is never assigned" ctx))))

  (define (make-export-body)
    (for/list ([(name obj) (in-dict exports-table)])
      #`(bind/import! #'#,name
          #,(match obj
              [(meta/entity ports)
               #`(meta/make-entity (list #,@(for/list ([(n p) (in-dict ports)])
                                              #`(cons #'#,n (meta/port '#,(meta/port-mode p))))))]
              [(meta/architecture ent-name)
               #`(meta/architecture #'#,ent-name)])))))
