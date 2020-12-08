#lang racket

(require
  "expander.rkt"
  (prefix-in stx/ "syntax.rkt")
  (for-syntax
    syntax/parse
    "scope.rkt"))

(provide begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (check (decorate stx)))

(begin-for-syntax
  (define (decorate stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       (with-scope
         #`(begin-tiny-hdl
             #,@(map decorate (attribute body))))]

      [:stx/entity
       #:with port^ (with-scope (map decorate (attribute port)))
       (bind! #'name
         #'(entity name port^))]

      [:stx/port
       (bind! #'name stx)]

      [:stx/architecture
       #:with ent-name^   (add-scope #'ent-name)
       #:with (body^ ...) (with-scope (map decorate (attribute body)))
       (bind! #'name
         #`(architecture name ent-name^ body^ ...))]

      [:stx/instance
       #:with arch-name^ (add-scope #'arch-name)
       (bind! #'name
         #'(instance name arch-name^))]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #:with inst-name^ (add-scope #'inst-name)
       #'(inst-name^ port-name)]

      [_ stx]))

  (define current-entity-name (make-parameter #f))

  (define (check stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       #`(begin
           #,@(map check (attribute body)))]

      [:stx/architecture
       (parameterize ([current-entity-name #'ent-name])
          #`(architecture name ent-name
              #,@(map check (attribute body))))]

      [:stx/assignment
       #`(assign #,(check #'target) #,(check #'expr))]

      [:stx/operation
       #`(op #,@(map check (attribute arg)))]

      [(inst-name:id port-name:id)
       (define/syntax-parse inst:stx/instance     (lookup #'inst-name))
       (define/syntax-parse arch:stx/architecture (lookup #'inst.arch-name))
       #`(port-ref arch.ent-name port-name inst-name)]

      [port-name:id
       #`(port-ref #,(current-entity-name) port-name)]

      [_ stx])))
