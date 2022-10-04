# Module IO
This module contains functions to load lisp code from file, write to files and print either on the console or in the log.

## Functions

- `print`: print a LValue either in the console or in the interpreter log.
```lisp
>> (print test)
test
```
- `__read__`: reads a string from a file. Prefer using the macro **read**.
```lisp
>> (__read__ <path-to-file>/<name-file>.lisp)
```
- `write`: write a LValue to a file.
```lisp
>> (write <path-to-file>/<name-file>.lisp)
```

## Macros

- `read` : macro used to read, parse and eval a lisp file.

```lisp
(read test.lisp)
=> (eval (parse (__read__ test.lisp)))
```