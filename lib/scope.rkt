#lang racket

(require
  syntax/id-table
  racket/function)

(provide
  exports-table
  with-scope
  thunk/in-scope
  add-scope
  bind!
  bind/export!
  bind/import!
  lookup)

(define current-scope (make-parameter #f))
(define exports-table (make-free-id-table))
(define imports-table (make-free-id-table))

(define-syntax-rule (with-scope* sc body ...)
  (parameterize ([current-scope sc])
    body ...))

(define-syntax-rule (with-scope body ...)
  (with-scope* (syntax-local-make-definition-context (current-scope))
    body ...))

(define-syntax-rule (thunk/in-scope body ...)
  (let ([sc (current-scope)])
    (thunk (with-scope* sc body ...))))

(define (add-scope stx)
  (internal-definition-context-introduce (current-scope) stx 'add))

(define (bind! name data)
  (syntax-local-bind-syntaxes (list name) #`'#,data (current-scope)))

(define (bind/export! name data)
  (bind! name data)
  (dict-set! exports-table name data))

(define (bind/import! name data)
  (dict-set! imports-table name data))

(define (lookup name [pred (λ (x) #t)])
  (define res (syntax-local-value name
                (thunk (dict-ref imports-table name
                         (thunk (raise-syntax-error #f "No declaration found for this identifier" name))))
                (current-scope)))
  (unless (pred res)
    (raise-syntax-error #f "Cannot be used in this context" name))
  res)
