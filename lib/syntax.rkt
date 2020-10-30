#lang racket

(require syntax/parse)

(provide (all-defined-out))

(define-syntax-class entity
  #:datum-literals [entity]
  (pattern (entity name:id (port:port ...))))

(define-syntax-class port
  #:datum-literals [input output]
  (pattern (mode:input  name:id))
  (pattern (mode:output name:id)))

(define-syntax-class architecture
  #:datum-literals [architecture]
  (pattern (architecture name:id ent-name:id body:statement ...)))

(define-syntax-class statement
  (pattern :instance)
  (pattern :assignment))

(define-syntax-class instance
  #:datum-literals [instance]
  (pattern (instance name:id arch-name:id)))

(define-syntax-class assignment
  #:datum-literals [assign]
  (pattern (assign target:port-ref expr:expression)))

(define-syntax-class expression
  (pattern :port-ref)
  (pattern :operation)
  (pattern :boolean))

(define-syntax-class port-ref
  (pattern port-name:id)
  (pattern (inst-name:id port-name:id)))

(define-syntax-class operation
  #:datum-literals [not and or xor]
  (pattern (op:not a:expression)
    #:attr (arg 1) (list #'a))
  (pattern (op:xor a:expression b:expression)
    #:attr (arg 1) (list #'a #'b))
  (pattern (op:and arg:expression ...))
  (pattern (op:or arg:expression ...)))
