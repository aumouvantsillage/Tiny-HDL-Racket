#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture b e
    (instance i a)
    ; In this example, architecture a is checked via the first assignment statement.
    (assign (i x) x)
    (assign y (i y)))
  (architecture a b ; <-- b: Invalid target
    (assign y x)))
