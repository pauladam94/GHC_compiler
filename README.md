# GHC_compiler

Toy implementation of GHC compiler for haskell including :
- lexer / parser
- typechecking
- some optimization of the program with rewrite rule from the literature
- no backend
- no interpreter

## Rewrite of the tests

The test system have been remade in Ocaml to handle specific exception for
each type of test. The complete test runnner is in the file `tester.ml`.

This rewrite has permit to see some issue within the current test and some quality
of life improvment namely :

1. task 1 tried to test file with `join` and `jump` named `illtyping2.f` and `illtyping8.f`
since at this stage of the project `join` and `jump` directive would crash (because not parsed)
which would make the test passed because the makefile only check if the run crashed or not. 

Indeed in our implementation we pick for each test which exception are considered fails and which
one are considered 

2. task 3b has this line in the makefile which test multiple times the same things :
```  
test3a: $(TYPECHECKJ) $(TYPEFAILJ) $(TYPECHECK) $(TYPECHECK)
```
(TYPECHECK is present 2 times)

Here is the correction line (according my reading this seems to be the right line) :
```  
test3a: $(TYPECHECKJ) $(TYPEFAILJ) $(TYPECHECK) $(TYPEFAIL)
```

3. better output for diff when testing optimization
4. Test only output the print during execution if the test fail
5. If the test fail a precise error is returned thanks to the exception raised buy
raising again the exception we can also get a stack trace in debug mode
6. Each test prints a [SUCESS] or [FAILED] information to the command line


## Task Completion

### Task 1 (typechecking)

task 1 has been completed.

### Task 2 (optimization case, beta, beta_tau, inline)

task 2 has been completed.

except for 2 tests that are related to match case optimization.

### Task 3a (syntax addition)

task 3a has been completed.

### Task 3b (optimization caseofcase)

task 3b has not been partially completed.

A few tests are passing.
The other tests are not passin because of real semantic difference between
the expected file and the one computed (not pretty printer issue).
