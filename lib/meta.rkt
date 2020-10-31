#lang racket

(provide (all-defined-out))

(struct entity (name))

(struct port (name mode))

(struct architecture (name ent-name))

(struct instance (name arch-name))
