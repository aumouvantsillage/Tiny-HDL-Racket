#lang tiny-hdl/spicy

entity half-adder
  input a
  input b
  output s
  output co
end

architecture half-adder-arch of half-adder
  s  <= a xor b
  co <= a and b
end
