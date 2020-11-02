#lang racket

(require
  "lib/expander.rkt"
  "lib/resolver.rkt")

(provide
  (all-from-out "lib/expander.rkt")
  begin-tiny-hdl)

(define-syntax (begin-tiny-hdl stx)
  (resolve (decorate stx)))
