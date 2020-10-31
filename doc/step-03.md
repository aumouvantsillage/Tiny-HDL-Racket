Step 3: name resolution

In this step, we add scoping information to the syntax object that represents
a Tiny-HDL source code, and we use these information to infer the `port-ref`
constructs.
Compared with step 2, the example `full-adder-step-03.rkt` has the following
noticeable differences:

* The Tiny-HDL source if wrapped in a `begin-tiny-hdl` construct.
* In the assignment statements, references to ports no longer contain the entity name.

You can run the example by typing this command:

```
racket examples/full-adder-step-03-test.rkt
```

## Syntax classes

In `lib/resolver.rkt`, we define two functions that transform syntax objects.
These functions rely on pattern matching to parse Tiny-HDL constructs.
Since we use the same syntax patterns several times, we have decided to
abstract them as syntax classes defined in a separate file: `lib/syntax.rkt`.

In this file, you will see that we have defined a complete *grammar* for Tiny-HDL
in terms of syntax patterns.

## Scoping

Our goal is to retrieve the name of the entity where a port is defined,
considering that we only now the name of the port, and possibly also the name of an instance.

In architecture `full-adder-arch`, the expression `co` will be transformed into
`(port-ref full-adder co)`.
This is quite straightforward to achieve with a Racket parameter `current-entity-name`
that stores the name of the entity for the current architecture.

Difficulties arise when we want to transform the expression `(h1 a)` into `(port-ref half-adder a h1)`,
as the name `half-adder` is not directly related to any element in the current context.
This is a situation where an explicit scoping system needs to be introduced.

In `lib/resolver.rkt`, the `decorate` function performs the following operations:

* Create a hierarchy of nested scopes.
* In each scope, register the association between a name and the corresponding syntax object.
* Keep track of the scope where each symbol is used.

These facilities are provided by the `lib/scope.rkt` module.

### Creating a hierarchy of scopes

A scope is composed of a table that maps names to corresponding data,
and a reference to its parent (outer) scope.
Scopes are created using the `with-scope` macro.
Internally, this construct makes use of a Racket parameter to keep the current
scope implicit and manage the nesting between scopes.

The purpose of the `begin-tiny-hdl` macro is to create the root scope.

In the full-adder example, we get the following hierarchy:

* Root scope (`begin-tiny-hdl`)
    * Scope for entity `half-adder`.
    * Scope for entity `full-adder`.
    * Scope for architecture `half-adder-arch`.
    * Scope for architecture `full-adder-arch`.

N.B: Creating scopes for entities is not strictly necessary.
It will mostly be useful to enforce distinct port names in a given entity.

### Registering named associations

The `bind!` function attaches some data to a name in a given scope.
In our case, we will use this function to associate names to their
corresponding syntax objects: entities, architectures, instances.

* Root scope: `half-adder`, `full-adder`, `half-adder-arch`, `full-adder-arch`.
    * Scope for entity `half-adder`: `a`, `b`, `s`, `co`.
    * Scope for entity `full-adder`: `a`, `b`, `ci`, `s`, `co`.
    * Scope for architecture `half-adder-arch`: empty.
    * Scope for architecture `full-adder-arch`: `h1`, `h2`.

### Decorate named references with their current scope

The `add-scope` function uses Racket syntax properties to add scope information
to named references.

In architecture `full-adder-arch`, all names `h1` and `h2` that appear in
assignment targets or expressions are decorated with the current scope
(the scope for architecture `full-adder-arch`).

### Name resolution

In function `resolve`, we use the `lookup` function to get the syntax object
that is associated to a given name in the current scope or one of its parents.
It will allow us to retrieve the name of the entity that corresponds to a given instance name.

For instance, let's resolve the expression `(h1 a)` in the scope of architecture `full-adder-arch`:

1. `(lookup #'h1)` will find `h1` in the current scope and return `#'(instance h1 half-adder-arch)`.
2. `(lookup #'half-adder-arch)` will find `half-adder-arch` in the root scope and return `#'(architecture half-adder-arch half-adder ...)`
3. Now, we now that the entity name is `hald-adder`.

## Disclaimers and credits

In this step, I have decided to bind names to their corresponding syntax objects.
My concern was to avoid introducing too many new concepts at the same time.
This approach is sufficient for the current purpose, but may prove cumbersome in more
complex languages.

As far as I could see, the common practice is to create `struct` types for the
data that need to be stored in scopes.
These `struct` types will provide a more abstract view of the syntax tree and
will be easier to manipulate in the semantic checking phase.

The reader may find it strange that I had to implement my own scope management
functions.
As a language-oriented programming platform, you would expect Racket to provide
facilities for managing scopes in user-defined languages.

For some reasons, while Racket provides scope-related APIs, I could not
find a way to re-use them in Tiny-HDL.
The API defined in `lib/scope.rkt` takes inspiration from the work of
Michael Ballantyne and Matthias Felleisen.
