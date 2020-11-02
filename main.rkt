#lang racket

(require
  "lib/expander.rkt"
  "lib/resolver.rkt"
  "lib/checker.rkt")

(provide
  (all-from-out "lib/expander.rkt")
  begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (check (resolve (decorate stx))))
