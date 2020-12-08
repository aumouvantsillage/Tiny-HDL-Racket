# Step 4: semantic checking

If the current state of the language, some errors in a Tiny-HDL source will be
reported against the generated Racket code.
For instance, suppose we use the wrong entity name in an architecture:

```
(architecture full-adder-arch half-adder
    ...
    (assign (h2 b) ci))
```

The error message will be:

```
half-adder-ci: unbound identifier
```

The error message describes a consequence of the wrong entity name:
since `half-adder` does not define a port `ci`, no accessor named `half-adder-ci`
exists after macro expansion.
A better error message would be: "Port `ci` not found in entity `half-adder`".

Here is a list of rules that we want to check:

* In an architecture, the entity name must refer to an existing entity in the current scope.
* In an instantiation statement, the architecture name must refer to an existing architecture in the current scope.
* In a port reference, the (optional) instance name must refer to an existing instance in the current scope.
* In a port reference, the port name must refer to an existing port.
* All the output ports of an architecture must be the assigned one, and only one time in this architecture.
* All the input ports of an instance must be assigned one, and only one time in the enclosing architecture.
* An input port of an architecture cannot be assigned.
* An output port of an instance cannot be assigned.

The first four rules are natural additions to the current name resolution process.
The others are specific to the domain of digital circuit modeling, where all inputs
of a circuit must have a known value in order to compute an output deterministically.

We provide a series of examples that each violate a specific rule.
Use this command to run one of these examples:

```
racket examples/error-...-step-04.rkt
```

You can also verify that the full-adder example from step 3 passes all the checks:

```
racket examples/full-adder-step-03-test.rkt
```

## Changes in the name resolution process

In `lib/scope.rkt`:

* The `lookup` function has been modified to accept an optional predicate
  to check against the retrieved data.
* The new macro `with-scope*` is a variant of `with-scope` that also returns
  the new scope object. Having a reference to the scope's table will be useful
  when we want to inspect the ports of a given entity or the instances in a
  given architecture.
* A parameter `lookup-cache` stores the result of each lookup operation in
  a hash map. A new map is created with the macro `with-lookup-cache`.

File `lib/meta.rkt` defines `struct` types for all the named element types
in a Tiny-HDL source.

In `lib/checker.rkt`:
* Function `decorate` has been changed to create and bind `struct` instances instead of binding syntax objects.
* Function `check` uses predicates when calling `lookup`.

Some checks may seem redundant, but they prove useful when an instance or
assignment appears before the elements that it references
(see the examples `error-...-reversed-step-04.rkt`).
Redundant lookup operations have a low impact on performance thanks to the
use of a lookup cache in macro `begin-tiny-hdl`.

All semantic rule checks have been inserted in the `check` function.
As a consequence, the result of `check` is a semantically correct syntax
object where names have been fully resolved.
