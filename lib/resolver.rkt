#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    racket
    syntax/parse
    "scope.rkt"
    (prefix-in stx/ "syntax.rkt")
    (prefix-in meta/ "meta.rkt")))

(provide begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (resolve (decorate stx)))

(begin-for-syntax
  (define (decorate stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       (with-scope
         #`(begin-tiny-hdl
             #,@(map decorate (attribute body))))]

      [:stx/entity
       #:with name^ (add-scope #'name)
       (define-values (port^ sc)
         (with-scope*
           (map decorate (attribute port))))
       (bind! #'name (meta/entity #'name^ (scope-table sc)))
       #`(entity name^ #,port^)]

      [:stx/port
       #:with name^ (add-scope #'name)
       (bind! #'name (meta/port #'name^ (syntax->datum #'mode)))
       #`(mode name^)]

      [:stx/architecture
       #:with name^     (add-scope #'name)
       #:with ent-name^ (add-scope #'ent-name)
       (define-values (body^ sc)
         (with-scope*
           (map decorate (attribute body))))
       (bind! #'name (meta/architecture #'name^ #'ent-name^ (scope-table sc)))
       #`(architecture name^ ent-name^ #,@body^)]

      [:stx/instance
       #:with name^      (add-scope #'name)
       #:with arch-name^ (add-scope #'arch-name)
       (bind! #'name (meta/instance #'name^ #'arch-name^))
       #'(instance name^ arch-name^)]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #`(#,(add-scope #'inst-name) port-name)]

      [_ stx]))

  (define current-entity     (make-parameter #f))
  (define assignment-targets (make-parameter #f))

  (define (resolve stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map resolve (attribute body)))]

      [:stx/architecture
       (parameterize ([current-entity (lookup #'ent-name meta/entity?)]
                      [assignment-targets (collect-assignment-targets (attribute body))])
          (check-all-assigned stx (assignment-targets) #'ent-name 'output)
          #`(architecture name ent-name
              #,@(map resolve (attribute body))))]

      [:stx/instance
       #:with ent-name (meta/architecture-ent-name (lookup #'arch-name meta/architecture?))
       (check-all-assigned stx (assignment-targets) #'ent-name 'input #'name)
       stx]

      [:stx/assignment
       (define target^ (resolve #'target))
       (check-target-mode target^)
       #`(assign #,target^ #,(resolve #'expr))]

      [:stx/operation
       #`(op #,@(map resolve (attribute arg)))]

      [(inst-name:id port-name:id)
       (define inst     (lookup #'inst-name meta/instance?))
       (define arch     (lookup (meta/instance-arch-name inst) meta/architecture?))
       (define ent-name (meta/architecture-ent-name arch))
       (define ent      (lookup ent-name meta/entity?))
       (unless (dict-has-key? (meta/entity-ports ent) #'port-name)
         (raise-syntax-error #f (format "Port not found in entity ~a" (syntax->datum ent-name)) #'port-name))
       #`(port-ref #,ent-name port-name inst-name)]

      [port-name:id
       (define ent (current-entity))
       (define ent-name (meta/entity-name ent))
       (unless (dict-has-key? (meta/entity-ports ent) #'port-name)
         (raise-syntax-error #f (format "Port not found in entity ~a" (syntax->datum ent-name)) #'port-name))
       #`(port-ref #,ent-name port-name)]

      [_ stx]))

  ; Check that the target port of an assignment has the appropriate port:
  ; * output in an assignment to a port of the current architecture,
  ; * input  in an assignment to a port of an instance.
  (define (check-target-mode stx)
    (define/syntax-parse (_ ent-name port-name (~optional inst-name)) stx)
    (define port (dict-ref (meta/entity-ports (lookup #'ent-name)) #'port-name))
    (define expected-mode (if (attribute inst-name) 'input 'output))
    (unless (eq? expected-mode (meta/port-mode port))
      (raise-syntax-error (syntax->datum #'port-name) "Invalid target for assignment" stx)))

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

  ; Check that all output ports of the current architecture, or all input ports
  ; of a given instance, are assigned.
  (define (check-all-assigned ctx targets ent-name mode [inst-name #f])
    (for ([(port-name port) (in-dict (meta/entity-ports (lookup ent-name meta/entity?)))]
          #:when (eq? mode (meta/port-mode port)))
      (define port-name^ (syntax->datum port-name))
      (define inst-name^ (and inst-name (syntax->datum inst-name)))
      (define port-id (if inst-name (list inst-name^ port-name^) port-name^))
      (unless (set-member? targets port-id)
        (raise-syntax-error port-name^ "Port is never assigned" ctx)))))
