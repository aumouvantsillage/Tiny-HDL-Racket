#lang racket

(require
  syntax/parse/define
  "expander.rkt"
  (for-syntax
    racket
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
       (define-values (port^ sc)
         (with-scope*
           (map decorate (attribute port))))
       (bind! #'name (meta/entity #'name (scope-table sc)))
       #`(entity name #,port^)]

      [:stx/port
       (bind! #'name (meta/port #'name (syntax->datum #'mode)))
       stx]

      [:stx/architecture
       #:with ent-name^ (add-scope #'ent-name)
       (define-values (body^ sc)
         (with-scope*
           (map decorate (attribute body))))
       (bind! #'name (meta/architecture #'name #'ent-name^ (scope-table sc)))
       #`(architecture name ent-name^ #,@body^)]

      [:stx/instance
       #:with arch-name^ (add-scope #'arch-name)
       (bind! #'name (meta/instance #'name #'arch-name^))
       #'(instance name arch-name^)]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #`(#,(add-scope #'inst-name) port-name)]

      [_ stx]))

  (define current-entity (make-parameter #f))

  (define (resolve stx)
    (with-lookup-cache
      (syntax-parse stx
        #:literals [begin-tiny-hdl]

        [(begin-tiny-hdl body ...)
         #`(begin
             #,@(map resolve (attribute body)))]

        [:stx/architecture
         (parameterize ([current-entity (lookup #'ent-name meta/entity?)])
            #`(architecture name ent-name
                #,@(map resolve (attribute body))))]

        [:stx/instance
         (lookup #'arch-name meta/architecture?)
         stx]

        [:stx/assignment
         #`(assign #,(resolve #'target) #,(resolve #'expr))]

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

        [_ stx]))))
