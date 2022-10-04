# The REPL

## The REPL

A scheme interpreter is a REPL (Read-Eval-Print-Loop). It takes from the standard input a string, parse it to transform it into an SExpr (or LValue in our case) and then evaluates it in an environment.

An environment contains bindings between symbols and LValues. Interpreting an expression in Lisp can either returns a LValue or an LError
