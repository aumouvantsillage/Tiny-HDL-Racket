#lang racket

(require
  syntax/id-table)

(provide (all-defined-out))

(struct entity (name ports))

(define (make-entity name ports-lst)
  (entity name
          (for/fold ([acc (make-immutable-free-id-table)])
                    ([it  (in-list ports-lst)])
            (dict-set acc (port-name it) it))))

(define (entity-port ent name)
  (dict-ref (entity-ports ent) name))

(struct architecture (name ent-name))

(struct port (name mode))

(struct instance (name arch-name))
