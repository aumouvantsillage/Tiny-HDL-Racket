#lang brag

begin-tiny-hdl: (use | entity | architecture)*

use: /"use" STRING

entity: /"entity" ID port-list /"end"

/port-list: port*

/port: ("input" | "output") ID

architecture: /"architecture" ID /"of" ID statement* /"end"

@statement: instance | assign

instance: ID /":" ID

assign: port-ref /"<=" expression

@expression: or | or-term

or:       (or-term /"or")+ or-term
@or-term: xor | xor-term

xor:       xor-term /"xor" xor-term
@xor-term: and | and-term

and:       (and-term /"and")+ and-term
@and-term: not | not-term

not: "not" not-term
@not-term: BOOLEAN
         | port-ref
         | /"(" expression /")"

@port-ref: ID | instance-port-ref

/instance-port-ref: ID /"." ID
