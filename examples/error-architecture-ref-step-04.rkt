#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e
    (instance i e) ; <-- e: Invalid target
    (assign (i x) x)
    (assign y (i y))))
