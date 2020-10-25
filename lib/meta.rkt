#lang racket

(require
  syntax/parse
  syntax/id-table
  (prefix-in stx/ "syntax.rkt"))

(provide (all-defined-out))

(struct metadata (stx) #:transparent)

(define (make-local-scope lst [sc (make-immutable-free-id-table)])
  (for/fold ([acc sc])
            ([i (in-list lst)])
    (syntax-parse i
      [:stx/named-elt
       (define meta (syntax-property i 'meta))
       (unless meta
         (raise-syntax-error #f "No metadata attached to name" #'name))
       (dict-set acc #'name meta)]
      [_ acc])))

(struct entity metadata (local-scope))

(define (make-entity stx)
  (define/syntax-parse :stx/entity stx)
  (entity stx (make-local-scope (attribute port))))

(struct architecture metadata (local-scope))

(define (make-architecture stx)
  (define/syntax-parse :stx/architecture stx)
  (architecture stx (make-local-scope (attribute body))))

(struct port metadata (mode))

(struct instance metadata ())
