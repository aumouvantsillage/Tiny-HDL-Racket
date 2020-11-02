#lang racket

(require
  "lib/expander.rkt"
  "lib/resolver.rkt"
  "lib/checker.rkt"
  (for-syntax "lib/scope.rkt"))

(provide
  (all-from-out "lib/expander.rkt")
  begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (with-lookup-cache
    (check (resolve (decorate stx)))))
