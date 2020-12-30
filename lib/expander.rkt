#lang racket

(require
  syntax/parse/define
  racket/stxparam
  (for-syntax
    racket/syntax))

(provide
  use
  entity
  architecture
  instance
  assign
  port-ref)

(define-syntax (use stx)
  (raise-syntax-error #f "should not be used outside of begin-tiny-hdl" stx))

(define-for-syntax (constructor-name name)
  (format-id name "make-~a" name))

(define-simple-macro (entity ent-name ([_ port-name] ...))
  #:with ent-struct-name (generate-temporary #'ent-name)
  #:with ent-ctor-name   (constructor-name   #'ent-name)
  (begin
    (provide (struct-out ent-struct-name))
    (struct ent-name ([port-name #:auto] ...)
      #:mutable
      #:name             ent-struct-name
      #:constructor-name ent-ctor-name)))

; Inside an architecture, current-instance will contain an instance of the current entity.
(define-syntax-parameter current-instance
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside an architecture")))

(define-simple-macro (architecture arch-name ent-name body ...)
  #:with arch-ctor-name (constructor-name #'arch-name)
  #:with ent-ctor-name  (constructor-name #'ent-name)
  (begin
    (provide arch-ctor-name)
    (define (arch-ctor-name)
      (define self (ent-ctor-name))
      (syntax-parameterize ([current-instance (make-rename-transformer #'self)])
        body ...)
      self)))

(define-simple-macro (instance inst-name arch-name)
  #:with arch-ctor-name (constructor-name #'arch-name)
  (define inst-name (arch-ctor-name)))

(define-simple-macro (assign ((~literal port-ref) ent-name port-name (~optional inst-name)) expr)
  #:with setter-name (format-id #'port-name "set-~a-~a!" #'ent-name #'port-name)
  #:with arg-name (if (attribute inst-name) #'inst-name #'current-instance)
  (setter-name arg-name (λ () expr)))

(define-simple-macro (port-ref ent-name port-name (~optional inst-name))
  #:with getter-name (format-id #'port-name "~a-~a" #'ent-name #'port-name)
  #:with arg-name (if (attribute inst-name) #'inst-name #'current-instance)
  ((getter-name arg-name)))
