#lang racket

(require syntax/id-table)

(provide
  with-scope
  bind!
  add-scope
  lookup)

; A scope has an optional parent scope
; and a table that maps ids to custom data.
(struct scope (parent table))

(define current-scope (make-parameter #f))

; Create a new scope with the given parent and an empty table.
(define (make-scope [parent (current-scope)])
  (scope parent (make-free-id-table)))

; Introduce a new scope for the given body.
(define-syntax-rule (with-scope body ...)
  (parameterize ([current-scope (make-scope)])
    body ...))

; Associate the given data to the given name in a scope.
; Return the data unchanged.
(define (bind! name data [sc (current-scope)])
  (dict-set! (scope-table sc) name data)
  data)

; Add scope information to a syntax object.
(define (add-scope stx [sc (current-scope)])
  (syntax-property stx 'scope sc))

; Lookup a name in a scope chain. If no scope is specified,
; start at the scope attached to the name.
; Returns the corresponding data, or raise an error if not found.
(define (lookup name [sc (syntax-property name 'scope)])
  (unless sc
    (raise-syntax-error #f "No declaration found for identifier" name))
  (dict-ref (scope-table sc) name
    (λ () (lookup name (scope-parent sc)))))
