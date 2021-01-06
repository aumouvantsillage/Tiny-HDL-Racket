#lang tiny-hdl/spicy

use "half-adder-step-06-spicy.rkt"

entity full-adder
  input a
  input b
  input ci
  output s
  output co
end

architecture full-adder-arch of full-adder
  h1 : half-adder-arch
  h2 : half-adder-arch
  h1.a <= a
  h1.b <= b
  h2.a <= h1.s
  h2.b <= ci
  s    <= h2.s
  co   <= h1.co or h2.co
end
