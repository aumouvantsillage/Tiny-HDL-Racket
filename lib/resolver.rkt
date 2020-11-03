#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    syntax/parse
    "scope.rkt"))

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
       (bind! #'name
         (with-scope
           #`(entity name #,(map decorate (attribute port)))))]

      [:stx/port
       (bind! #'name stx)]

      [:stx/architecture
       (bind! #'name
         (with-scope
           #`(architecture name #,(add-scope #'ent-name)
               #,@(map decorate (attribute body)))))]

      [:stx/instance
       (bind! #'name #`(instance name #,(add-scope #'arch-name)))]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #`(#,(add-scope #'inst-name) port-name)]

      [_ stx]))

  (define current-entity-name (make-parameter #f))

  (define (resolve stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map resolve (attribute body)))]

      [:stx/architecture
       (parameterize ([current-entity-name #'ent-name])
          #`(architecture name ent-name
              #,@(map resolve (attribute body))))]

      [:stx/assignment
       #`(assign #,(resolve #'target) #,(resolve #'expr))]

      [:stx/operation
       #`(op #,@(map resolve (attribute arg)))]

      [(inst-name:id port-name:id)
       (define/syntax-parse inst:stx/instance     (lookup #'inst-name))
       (define/syntax-parse arch:stx/architecture (lookup #'inst.arch-name))
       #`(port-ref arch.ent-name port-name inst-name)]

      [port-name:id
       #`(port-ref #,(current-entity-name) port-name)]

      [_ stx])))
