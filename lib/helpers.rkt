#lang racket

(require syntax/parse/define)

(provide (all-defined-out))

(define-syntax-parser define*
  [(_ (~seq v:id e:expr) ...)
   #'(begin
       (define v e) ...)])
