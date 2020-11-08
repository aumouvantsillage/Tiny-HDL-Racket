# Step 6: promoting Tiny-HDL as a language

So far, Tiny-HDL source code was embedded in Racket modules.
In this final step, we make Tiny-HDL a *standalone* language.
Compared to the previous step, a Tiny-HDL source file will have:

* a `#lang tiny-hdl` directive instead of `#lang racket`,
* no explicit use of the `begin-tiny-hdl` macro,
* no possibility to mix Racket and Tiny-HDL code in the same file.

These changes have been applied to the full-adder example in files
`half-adder-step-06.rkt` and `full-adder-step-06.rkt`.

You can run this example by typing this command:

```
racket examples/full-adder-step-06-test.rkt
```

## Adding a reader

I have added a `lang` folder containing two files:

* `reader.rkt` sets up an S-expression parser for Tiny-HDL.
* `expander.rkt` provides a `#%module-begin` macro that expands our to `begin-tiny-hdl` macro.
