#lang racket

(require tiny-hdl)

(entity half-adder ([input a] [input b] [output s] [output co]))

(entity full-adder ([input a] [input b] [input ci] [output s] [output co]))

(architecture half-adder-arch half-adder
  (assign (port-ref half-adder s)  (xor (port-ref half-adder a) (port-ref half-adder b)))
  (assign (port-ref half-adder co) (and (port-ref half-adder a) (port-ref half-adder b))))

(architecture full-adder-arch full-adder
  (instance h1 half-adder-arch)
  (instance h2 half-adder-arch)
  (assign (port-ref half-adder a h1) (port-ref full-adder a))
  (assign (port-ref half-adder b h1) (port-ref full-adder b))
  (assign (port-ref half-adder a h2) (port-ref half-adder s h1))
  (assign (port-ref half-adder b h2) (port-ref full-adder ci))
  (assign (port-ref full-adder s)    (port-ref half-adder s h2))
  (assign (port-ref full-adder co)   (or (port-ref half-adder co h1) (port-ref half-adder co h2))))
