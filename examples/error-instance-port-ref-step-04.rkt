#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e
    (instance i a)
    (assign (i z) x) ; <-- z: Port not found in entity e
    (assign (i x) x)
    (assign y (i y))))
