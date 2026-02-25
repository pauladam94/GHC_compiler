# GHC_compiler

Toy implementation of GHC compiler for haskell.

The test system have been remade in Ocaml to handle specific exception for
each type of test.

## Task 1

This rewrote permits to see that the two tests `illtyping2.f` and `illtyping8.f` are wrongly named.
Indeed they use `join` and `jump` operator, however since the test should not passed and the makefile
wait for the test to crash. All people in the class have a passing test even if the test is crashing because
of parsing and not of typecking.
