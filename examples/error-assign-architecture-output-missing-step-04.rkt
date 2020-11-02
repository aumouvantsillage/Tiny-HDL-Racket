#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e ; <-- y: Port is never assigned
    (instance i a)
    (assign (i x) x)))
