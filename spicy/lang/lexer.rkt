#lang racket

(require brag/support)

(provide tokenize)

(define (tokenize ip)
  (port-count-lines! ip)
  (define tiny-hdl-lexer
    (lexer-src-pos
      [(:or "use" "entity" "input" "output"
            "architecture" "of" "end"
            "and" "or" "not" "xor"
            "<=" ":" "." "(" ")")
       (token lexeme (string->symbol lexeme))]
      [(:or "false" "true")
       (token 'BOOLEAN (eq? lexeme "true"))]
      [(:seq alphabetic (:* (:or alphabetic numeric "_" "-")))
       (token 'ID (string->symbol lexeme))]
      [(from/to "\"" "\"")
       (token 'STRING (trim-ends "\"" lexeme "\""))]
      [whitespace
       (token 'WHITESPACE lexeme #:skip? #t)]
      [(eof)
       (void)]))
  (λ () (tiny-hdl-lexer ip)))
