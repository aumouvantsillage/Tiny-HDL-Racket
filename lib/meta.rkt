#lang racket

(require syntax/id-table)

(provide (all-defined-out))

(struct entity (ports))

(define (make-entity ports)
  (entity (make-immutable-free-id-table ports)))

(define (entity-port-ref ent name)
  (dict-ref (entity-ports ent) name
    (λ () (raise-syntax-error #f "No port declaration found for this identifier" name))))

(struct port (mode))

(struct architecture (ent-name))

(struct instance (arch-name))
