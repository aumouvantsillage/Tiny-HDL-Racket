#lang racket

(require syntax/id-table)

(provide
  with-scope
  add-scope
  bind!
  lookup
  with-lookup-cache)

; A scope has an optional parent scope
; and a table that maps ids to custom data.
(struct scope (parent table))

(define current-scope (make-parameter #f))

(define-syntax-rule (with-scope body ...)
  (parameterize ([current-scope (make-scope)])
    body ...))

; Create a new scope with the given parent and an empty table.
(define (make-scope [parent (current-scope)])
  (scope parent (make-free-id-table)))

; Add scope information to a syntax object.
(define (add-scope stx [sc (current-scope)])
  (syntax-property stx 'parent-scope sc))

; Associate the given data to the given name in a scope.
; Return the data unchanged.
(define (bind! name data [sc (current-scope)])
  (dict-set! (scope-table sc) name data)
  data)

(define lookup-cache (make-parameter (make-hasheq)))

(define-syntax-rule (with-lookup-cache body ...)
  (parameterize ([lookup-cache (make-hasheq)])
    body ...))

; Lookup a name in a scope chain. If no scope is specified,
; start at the scope attached to the name.
; Returns the corresponding data, or raise an error if not found.
(define (lookup name [pred (λ (x) #t)] [sc (syntax-property name 'parent-scope)])
  ; Find the given name in the lookup cache.
  (dict-ref (lookup-cache) name
    (λ ()
      ; If not found, find it in the given scope,
      ; looking up recursively in the parent scope chain.
      (unless sc
        (raise-syntax-error #f "No declaration found for identifier" name))
      (define res (dict-ref (scope-table sc) name
                    (λ () (lookup name pred (scope-parent sc)))))
      ; Check that the result satisfies the given predicate.
      (unless (pred res)
        (raise-syntax-error #f "Invalid target" name))
      ; Add the result to the lookup cache and return.
      (dict-set! (lookup-cache) name res)
      res)))
