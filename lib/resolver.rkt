#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    racket
    syntax/parse
    "helpers.rkt"
    "scope.rkt"
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
       (define-values (port^ sc)
         (with-scope*
           (map decorate (attribute port))))
       (bind! #'name (meta/entity (scope-table sc)))
       (quasisyntax/loc stx
         (entity name #,port^))]

      [:stx/port
       (bind! #'name (meta/port (syntax->datum #'mode)))
       (syntax/loc stx
         (mode name))]

      [:stx/architecture
       #:with ent-name^ (add-scope #'ent-name)
       (define-values (body^ sc)
         (with-scope*
           (map decorate (attribute body))))
       (bind! #'name (meta/architecture #'ent-name^ (scope-table sc)))
       (quasisyntax/loc stx
         (architecture name ent-name^ #,@body^))]

      [:stx/instance
       #:with arch-name^ (add-scope #'arch-name)
       (bind! #'name (meta/instance #'arch-name^))
       (syntax/loc stx
         (instance name arch-name^))]

      [:stx/assignment
       (quasisyntax/loc stx
         (assign #,(decorate #'target) #,(decorate #'expr)))]

      [:stx/operation
       (quasisyntax/loc stx
         (op #,@(map decorate (attribute arg))))]

      [(inst-name:id port-name:id)
       #:with inst-name^ (add-scope #'inst-name)
       (quasisyntax/loc stx
         (inst-name^ port-name))]

      [_ stx]))

  (define current-entity-name (make-parameter #f))
  (define assignment-targets  (make-parameter #f))

  (define (resolve stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map resolve (attribute body)))]

      [:stx/architecture
       (parameterize ([current-entity-name #'ent-name]
                      [assignment-targets (collect-assignment-targets (attribute body))])
          (lookup #'ent-name meta/entity?)
          (check-all-assigned stx (assignment-targets) #'ent-name 'output)
          #`(architecture name ent-name
              #,@(map resolve (attribute body))))]

      [:stx/instance
       #:with ent-name (meta/architecture-ent-name (lookup #'arch-name meta/architecture?))
       (check-all-assigned stx (assignment-targets) #'ent-name 'input #'name)
       stx]

      [:stx/assignment
       #:with target^ (resolve #'target)
       #:with (_ ent-name port-name (~optional inst-name)) #'target^
       ; Check that the target port of an assignment has the appropriate port:
       ; * output in an assignment to a port of the current architecture,
       ; * input  in an assignment to a port of an instance.
       (define* mode (if (attribute inst-name) 'input 'output)
                port (dict-ref (meta/entity-ports (lookup #'ent-name)) #'port-name))
       (unless (eq? mode (meta/port-mode port))
         (raise-syntax-error (syntax->datum #'port-name) "Invalid target for assignment" stx))
       #`(assign target^ #,(resolve #'expr))]

      [:stx/operation
       #`(op #,@(map resolve (attribute arg)))]

      [(inst-name:id port-name:id)
       (define* inst     (lookup #'inst-name meta/instance?)
                arch     (lookup (meta/instance-arch-name inst) meta/architecture?)
                ent-name (meta/architecture-ent-name arch)
                ent      (lookup ent-name meta/entity?))
       (unless (dict-has-key? (meta/entity-ports ent) #'port-name)
         (raise-syntax-error #f (format "Port not found in entity ~a" (syntax->datum ent-name)) #'port-name))
       #`(port-ref #,ent-name port-name inst-name)]

      [port-name:id
       (define* ent-name (current-entity-name)
                ent      (lookup ent-name))
       (unless (dict-has-key? (meta/entity-ports ent) #'port-name)
         (raise-syntax-error #f (format "Port not found in entity ~a" (syntax->datum ent-name)) #'port-name))
       #`(port-ref #,ent-name port-name)]

      [_ stx]))

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
      (define* port-name^ (syntax->datum port-name)
               inst-name^ (and inst-name (syntax->datum inst-name))
               port-id (if inst-name (list inst-name^ port-name^) port-name^))
      (unless (set-member? targets port-id)
        (raise-syntax-error port-name^ "Port is never assigned" ctx)))))
