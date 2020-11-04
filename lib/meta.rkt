#lang racket

(provide (all-defined-out))

(struct entity (ports))

(struct port (mode))

(struct architecture (ent-name instances))

(struct instance (arch-name))
