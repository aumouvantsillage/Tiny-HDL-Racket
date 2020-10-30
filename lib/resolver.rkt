#lang racket

(require
  syntax/parse/define
  "expander.rkt"
  (for-syntax
    "scope.rkt"
    (prefix-in stx/  "syntax.rkt")
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
         #`(begin-tiny-hdl #,@(map decorate (attribute body))))]

      [:stx/entity
       (define ports (for/list ([it (in-list (attribute port))])
                       (define/syntax-parse p:stx/port it)
                       (meta/port #'p.name (syntax->datum #'p.mode))))
       (bind! #'name (meta/make-entity #'name ports))
       stx]

      [:stx/architecture
       #:with ent-name^ (add-scope #'ent-name)
       (bind! #'name (meta/architecture #'name #'ent-name^))
       (with-scope
         #`(architecture name ent-name^ #,@(map decorate (attribute body))))]

      [:stx/instance
       #:with arch-name^ (add-scope #'arch-name)
       (bind! #'name (meta/instance #'name #'arch-name^))
       #'(instance name arch-name^)]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #:with inst-name^ (add-scope #'inst-name)
       #'(inst-name^ port-name)]

      [_ stx]))

  (define current-entity (make-parameter #f))

  (define (resolve stx)
    (with-lookup-cache
      (syntax-parse stx
        #:literals [begin-tiny-hdl architecture assign not and or xor]

        [(begin-tiny-hdl body ...)
         #`(begin #,@(map resolve (attribute body)))]

        [:stx/architecture
         (parameterize ([current-entity (lookup #'ent-name meta/entity?)])
           #`(architecture name ent-name #,@(map resolve (attribute body))))]

        [:stx/assignment
         #`(assign #,(resolve #'target) #,(resolve #'expr))]

        [:stx/operation
         #`(op #,@(map resolve (attribute arg)))]

        [(inst-name:id port-name:id)
         (define inst (lookup #'inst-name                       meta/instance?))
         (define arch (lookup (meta/instance-arch-name inst)    meta/architecture?))
         (define ent  (lookup (meta/architecture-ent-name arch) meta/entity?))
         (define port (meta/entity-port ent #'port-name))
         #`(port-ref #,(meta/entity-name ent) port-name inst-name)]

        [port-name:id
         (define ent  (current-entity))
         (define port (meta/entity-port ent #'port-name))
         #`(port-ref #,(meta/entity-name ent) port-name)]

        [_ stx]))))
