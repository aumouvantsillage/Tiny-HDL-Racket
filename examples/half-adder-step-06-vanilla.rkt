#lang tiny-hdl/vanilla

(entity half-adder ([input a] [input b] [output s] [output co]))

(architecture half-adder-arch half-adder
  (assign s  (xor a b))
  (assign co (and a b)))
