#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e
    (instance i a)
    (assign (i x) x)
    (assign x (i y)))) ; <-- x: Invalid target for assignment
