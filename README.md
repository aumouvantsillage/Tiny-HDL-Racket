# Racket-Tiny-HDL

Tiny-HDL is a minimal Hardware Description Language that borrows concepts from
VHDL and Verilog.

The goal of this project is to experiment with some techniques for language
implementation in Racket.
The concerns that are addressed in this project are:

* abstract syntax representation and exploration,
* scoping and name resolution,
* semantic checking.

For these reasons, there is no plan to add features that would make the language
suitable for real circuit design: code generation for synthesis tools,
data types, syntax and model of computation for sequential circuits, etc.

## Structure of this repository

The development of this language is decomposed into several steps.
Each step corresponds to a branch of this repository.

Check the `doc` folder for details.
