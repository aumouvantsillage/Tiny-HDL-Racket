# Step 0: Tiny-HDL concepts and syntax

Like in VHDL, Tiny-HDL separates the interface and the implementation of a circuit
into an *entity* and an *architecture*.

* An entity has `input` and `output` ports that transport boolean values:

```
(entity half-adder ([input a] [input b] [output s] [output co]))

(entity full-adder ([input a] [input b] [input ci] [output s] [output co]))
```

* An architecture provides an implementation for a given entity:

```
(architecture half-adder-arch half-adder
    ...
)

(architecture full-adder-arch full-adder
    ...
)
```

The body of an architecture is composed of concurrent statements.
Two kinds of statements are supported:

* An instantiation statement creates an instance of a given architecture.
* An assignment statement assigns the result of an expression to an output port of the current architecture, or to an input port of an instance.

In this example, we create an instance `h1` of the architecture `half-adder-arch`.
Then we assign the port `a` of the current architecture to the port `a` of `h1`.

```
(architecture full-adder-arch full-adder
  (instance h1 half-adder-arch)
  ...
  (assign (h1 a) a)
)
```

In the right-hand side of an assignment, Tiny-HDL also supports boolean operations using the following syntax:

* `(not expr)`
* `(xor expr expr)`
* `(or expr ...)`
* `(and expr ...)`

## Example

Here is the complete full-adder description using two instances of a half-adder entity:

```
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
  (assign s (h2 s))
  (assign co (or (h1 co) (h2 co))))
```
