#lang racket

(require
  syntax/parse/define
  racket/stxparam
  (for-syntax
    racket/syntax))

(provide
  entity
  architecture
  instance
  assign
  port-ref)

(define-simple-macro (entity ent-name ([_ port-name] ...))
  (begin
    (provide (struct-out ent-name))
    (struct ent-name ([port-name #:auto] ...) #:mutable)))

; Inside an architecture, this-instance will contain an instance of the current entity.
(define-syntax-parameter current-instance
  (λ (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside an architecture")))

(define-simple-macro (architecture arch-name ent-name body ...)
  (begin
    (provide arch-name)
    (define (arch-name)
      (define self (ent-name))
      (syntax-parameterize ([current-instance (make-rename-transformer #'self)])
        body ...)
      self)))

(define-simple-macro (instance inst-name arch-name)
  (define inst-name (arch-name)))

(define-simple-macro (assign ((~literal port-ref) ent-name port-name (~optional inst-name)) expr)
   #:with setter-name (format-id #'port-name "set-~a-~a!" #'ent-name #'port-name)
   #:with arg-name (if (attribute inst-name) #'inst-name #'current-instance)
   (setter-name arg-name (λ () expr)))

(define-simple-macro (port-ref ent-name port-name (~optional inst-name))
   #:with getter-name (format-id #'port-name "~a-~a" #'ent-name #'port-name)
   #:with arg-name (if (attribute inst-name) #'inst-name #'current-instance)
   ((getter-name arg-name)))
