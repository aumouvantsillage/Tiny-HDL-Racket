#lang racket

(require tiny-hdl)

(entity half-adder ([input a] [input b] [output s] [output co]))

(entity full-adder ([input a] [input b] [input ci] [output s] [output co]))

(architecture half-adder-arch half-adder
  (assign s  (xor a b))
  (assign co (and a b)))

(architecture full-adder-arch full-adder
  (instance h1 half-adder-arch)
  (instance h2 half-adder-arch)
  (assign (h1 a) a)
  (assign (h1 b) b)
  (assign (h2 a) (h1 s))
  (assign (h2 b) ci)
  (assign s      (h2 s))
  (assign co     (or (h1 co) (h2 co))))
