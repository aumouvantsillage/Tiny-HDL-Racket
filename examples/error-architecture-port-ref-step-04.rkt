#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e
    (instance i a)
    (assign (i x) x)
    (assign z (i y)) ; <-- z: Port not found in entity e
    (assign y (i y))))
