#lang racket

(require
  syntax/parse/define
  "expander.rkt"
  (for-syntax
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
       (bind! #'name (meta/entity #'name))
       (with-scope
         #`(entity name #,(map decorate (attribute port))))]

      [:stx/port
       (bind! #'name (meta/port #'name (syntax->datum #'mode)))
       stx]

      [:stx/architecture
       #:with ent-name^ (add-scope #'ent-name)
       (bind! #'name (meta/architecture #'name #'ent-name^))
       (with-scope
         #`(architecture name ent-name^
             #,@(map decorate (attribute body))))]

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
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map resolve (attribute body)))]

      [:stx/architecture
       (parameterize ([current-entity (lookup #'ent-name meta/entity?)])
          #`(architecture name ent-name
              #,@(map resolve (attribute body))))]

      [:stx/assignment
       #`(assign #,(resolve #'target) #,(resolve #'expr))]

      [:stx/operation
       #`(op #,@(map resolve (attribute arg)))]

      [(inst-name:id port-name:id)
       (define inst     (lookup #'inst-name meta/instance?))
       (define arch     (lookup (meta/instance-arch-name inst) meta/architecture?))
       (define ent-name (meta/architecture-ent-name arch))
       ; TODO check that the current entity has a port named #'port-name
       #`(port-ref #,ent-name port-name inst-name)]

      [port-name:id
       ; TODO check that the current entity has a port named #'port-name
       #`(port-ref #,(meta/entity-name (current-entity)) port-name)]

      [_ stx])))
