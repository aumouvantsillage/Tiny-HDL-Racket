#lang s-exp syntax/module-reader
tiny-hdl/spicy/lang/expander
#:read                tiny-hdl-read
#:read-syntax         tiny-hdl-read-syntax
#:whole-body-readers? #t

(require "lexer.rkt" "grammar.rkt")

(define (tiny-hdl-read in)
  (syntax->datum (tiny-hdl-read-syntax #f in)))

(define (tiny-hdl-read-syntax src ip)
  (list (parse src (tokenize ip))))
