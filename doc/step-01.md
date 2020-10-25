# Step 1: manual implementation of an example in Racket

Before attempting to create a complete language implementation, let's figure
out how each Tiny-HDL construct will be translated into Racket code.

* An entity will be implemented as a struct type with one port per field.
* An architecture will be implemented as a function that returns a struct instance.

This function will play the role of a *constructor* that will be used to create
instances of an architecture.
As such, it does not perform any computation, but assigns each output port with
an expression that will be evaluated later.
For this reason, the fields of a struct instance will contain functions that
return the actual value of each port.

The complete implementation of the full-adder example is available in file
`examples/full-adder-manual.rkt`.
We also provide a test program in file `examples/full-adder-common-test.rkt`
that prints the complete truth table of a full-adder.
The same test program will be used throughout this experiment.
You can run the example by typing this command:

```
racket examples/full-adder-manual-test.rkt
```

The following sections will highlight some key aspects of this implementation.

## Entities

An entity is implemented as a Racket struct with mutable fields
that are set to `#f` by default.

```racket
(struct full-adder ([a  #:auto]
                    [b  #:auto]
                    [ci #:auto]
                    [s  #:auto]
                    [co #:auto]) #:mutable)
```

## Architectures

An architecture is implemented as a function that returns an instance of
its entity.
The skeleton of the architecture `full-adder-arch` is:

```racket
(define (full-adder-arch)
  (define self (full-adder))
  ...
  self)
```

## Instantiation statements

An instantiation statement is implemented as a call to the function that
implements the chosen architecture.
The returned instance is assigned to a variable:

```racket
(define h1 (half-adder-arch))
```

## Expressions

Reading a port consists in calling the function stored in the corresponding
field of a struct instance.

In the body of `full-adder-arch`, this expression reads the current value
of port `a` of the entity `full-adder`:

```racket
((full-adder-a self))
```

And this expression reads port `s` of the entity `half-adder`,
instantiated as `h1`:

```racket
((half-adder-s h1))
```

Operators `not`, `and`, `or`, and `xor` are directly provided by Racket.

## Assignments

An assignment statement to a given port is implemented by wrapping an
expression in a lambda, and then assigning it to the corresponding struct
field.
For instance, in `half-adder-arch`, port `s` is assigned like this:

```racket
(set-half-adder-s! self (λ () (xor ((half-adder-a self)) ((half-adder-b self)))))
```
