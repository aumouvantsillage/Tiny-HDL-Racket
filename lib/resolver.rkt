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
  (define (bind!/meta thunk stx . args)
    (define meta (apply thunk stx args))
    (define/syntax-parse :stx/named-elt stx)
    (bind! #'name meta)
    (syntax-property stx 'meta meta))

  (define (decorate stx)
    (syntax-parse stx
      #:literals [begin-tiny-hdl]

      [(begin-tiny-hdl body ...)
       (with-scope
         #`(begin-tiny-hdl #,@(map decorate (attribute body))))]

      [:stx/entity
       (bind!/meta meta/make-entity
         (with-scope
           #`(entity name #,(map decorate (attribute port)))))]

      [:stx/port
       (bind!/meta meta/port stx (syntax->datum #'mode))]

      [:stx/architecture
       (bind!/meta meta/make-architecture
         (with-scope
           #`(architecture name #,(add-scope #'ent-name) #,@(map decorate (attribute body)))))]

      [:stx/instance
       (bind!/meta meta/instance
         #`(instance name #,(add-scope #'arch-name)))]

      [:stx/assignment
       #`(assign #,(decorate #'target) #,(decorate #'expr))]

      [:stx/operation
       #`(op #,@(map decorate (attribute arg)))]

      [(inst-name:id port-name:id)
       #`(#,(add-scope #'inst-name) port-name)]

      [_ stx]))

  (define current-entity-name (make-parameter #f))

  (define (resolve stx)
    (with-lookup-cache
      (syntax-parse stx
        #:literals [begin-tiny-hdl architecture assign not and or xor]

        [(begin-tiny-hdl body ...)
         #`(begin #,@(map resolve (attribute body)))]

        [:stx/architecture
         (parameterize ([current-entity-name #'ent-name])
           #`(architecture name ent-name #,@(map resolve (attribute body))))]

        [:stx/assignment
         #`(assign #,(resolve #'target) #,(resolve #'expr))]

        [:stx/operation
         #`(op #,@(map resolve (attribute arg)))]

        [(inst-name:id port-name:id)
         #:with i:stx/instance     (meta/metadata-stx (lookup #'inst-name   meta/instance?))
         #:with a:stx/architecture (meta/metadata-stx (lookup #'i.arch-name meta/architecture?))
         #'(port-ref a.ent-name port-name inst-name)]

        [port-name:id
         #`(port-ref #,(current-entity-name) port-name)]

        [_ stx]))))
