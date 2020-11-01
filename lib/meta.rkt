#lang racket

(provide (all-defined-out))

(struct entity (name ports))

(struct port (name mode))

(struct architecture (name ent-name instances))

(struct instance (name arch-name))
