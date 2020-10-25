# Step 2: code generation using macros

In this step, we implement a simple code generator that converts an intermediate
form of the Tiny-HDL language into Racket code.
As you will see when looking at the example, the language is not exactly Tiny-HDL yet:
we have introduced a `port-ref` construct that will be explained in the next section.

Code generation is performed by macros implemented in `lib/expander.rkt`.

The complete implementation of the full-adder example is available in file
`examples/full-adder-step-02.rkt`.

Before running the example for the first time, you will need to register this
project as a Racket collection so that we can `(require tiny-hdl)` when needed.

```
raco link -n tiny-hdl .
```

Then you can run the example by typing this command:

```
racket examples/full-adder-step-02-test.rkt
```

## The `port-ref` expression

In the generated Racket code, reading or writing a port is performed by
calling an accessor provided by a struct type.
For a struct `S` and a field `F`, the accessor name has the form `S-F` or `set-S-F!`,
which means that you need to know the name of the struct to compose the accessor name.

For this reason, we have introduced the `port-ref` expression that
can take two forms:

```racket
; When the port belongs to the current architecture:
(port-ref entity-name port-name)
; When the port belongs to an instance:
(port-ref entity-name port-name inst-name)
```

Now we can write macros that generate the appropriate accessor names.
In step 3, we will implement a name resolver that will generate these
`port-ref` expressions for us.

Note: as a workaround, we could have used hash tables instead of structs,
but it would have been less fun!
