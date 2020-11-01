#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b a ; <-- a: Invalid target
    (instance i a)
    (assign (i x) x)
    (assign y (i y))))
