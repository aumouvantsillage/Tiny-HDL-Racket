#lang racket

(require tiny-hdl)

(begin-tiny-hdl
  (entity e ([input x] [output y]))
  (architecture a e
    (assign y x))
  (architecture b e
    ; In this example, the instance is checked via the first assignment statement.
    (assign (i x) x)
    (assign y (i y))
    (instance i e))) ; <-- e: Invalid target
