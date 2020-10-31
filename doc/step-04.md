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
exists.
A better error message would be: "Port `ci` not found in entity `half-adder`".

Here is a list of rules that we want to check:

* In an architecture, the entity name must refer to an existing entity in the current scope.
* In an instantiation statement, the architecture name must refer to an existing architecture in the current scope.
* In a port reference, the (optional) instance name must refer to an existing instance in the current scope.
* In a port reference, the port name must refer to an existing port.
* All the output ports of an architecture must be the assigned in this architecture.
* All the input ports of an instance must be assigned in the enclosing architecture.
* An input port of an architecture cannot be assigned.
* An output port of an instance cannot be assigned.

The first four rules are closely related to the name resolution stage.
In this step we have modified the function `resolve` accordingly.

The other rules are implemented in file `lib/checker.rkt`.
