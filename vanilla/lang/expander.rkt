#lang racket

(require tiny-hdl)

(provide
  (all-from-out tiny-hdl)
  (rename-out [tiny-hdl-module-begin #%module-begin])
  xor and or not)

(define-syntax-rule (tiny-hdl-module-begin body ...)
  (#%module-begin
    (begin-tiny-hdl body ...)))
