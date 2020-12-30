#lang racket

(require
  syntax/parse/define
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

(define-syntax-parser begin-tiny-hdl
  [(_ body ...)
   #'(begin
       (module-level-bindings body) ...
       (check body ...))])

(define-syntax-parser module-level-bindings
  [(_ u:stx/use)
   #'(require u.path)]

  [(_ e:stx/entity)
   #'(begin
       (provide e.name)
       (define-syntax e.name (meta/make-entity
                               (for/hash ([p (in-list '(e.port ...))])
                                 (define/syntax-parse q:stx/port p)
                                 (values #'q.name (meta/port (syntax->datum #'q.mode)))))))]

  [(_ a:stx/architecture)
   #'(begin
       (provide a.name)
       (define-syntax a.name (meta/architecture #'a.ent-name)))]

  [_
   #'(begin)])

(define-syntax (check stx)
  ((checker stx)))

(begin-for-syntax
  (define (check-all lst)
    (map (λ (f) (f)) lst))

  (define current-entity-name        (make-parameter #f))
  (define current-assignment-targets (make-parameter #f))

  (define (checker stx)
    (syntax-parse stx
      #:literals [check]

      [(check body ...)
       (define body^ (with-scope
                       (~>> (attribute body)
                            (map add-scope)
                            (map checker))))
       (thunk
         #`(begin
             #,@(check-all body^)))]

      [:stx/use
       (thunk #'(begin))]

      [a:stx/architecture
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
                        [current-assignment-targets (collect-assignment-targets (attribute a.body))])
           ; Check that all output ports of the entity are assigned
           ; in the current architecture.
           (check-all-assigned stx)
           #`(architecture a.name a.ent-name
               #,@(check-all body^))))]

      [i:stx/instance
       (bind! #'i.name (meta/instance #'i.arch-name))
       (thunk/in-scope
         (~> #'i.arch-name
             ; Check that arch-name refers to an architecture.
             (lookup meta/architecture?)
             ; Get the entity name for that architecture
             (meta/architecture-ent-name)
             ; Check that all input ports of the entity for that architecture
             ; are assigned in the current architecture.
             (check-all-assigned stx #'i.name _))
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
        [a:stx/assignment
         (define target-id (syntax->datum #'a.target))
         (when (set-member? acc target-id)
           (define port-name (if (list? target-id) (second target-id) target-id))
           (raise-syntax-error port-name "Port is assigned more than one time" #'a.target))
         (set-add acc target-id)]

        [_ acc])))

  ; Check that all output ports of the current architecture,
  ; or all input ports of a given instance, are assigned.
  (define (check-all-assigned ctx [inst-name #f] [ent-name (current-entity-name)])
    (define mode (if inst-name 'input 'output))
    (for ([(port-name port) (in-dict (meta/entity-ports (lookup ent-name meta/entity?)))]
          #:when (eq? mode (meta/port-mode port)))
      (define port-name^ (syntax->datum port-name))
      (define target-id (if inst-name
                          (list (syntax->datum inst-name) port-name^)
                          port-name^))
      (unless (set-member? (current-assignment-targets) target-id)
        (raise-syntax-error port-name^ "Port is never assigned" ctx)))))
