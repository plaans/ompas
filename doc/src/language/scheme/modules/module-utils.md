---
description: Module providing utilitary functions useful when programming. It also provides lambdas and macros.
---
# Module Utils

## Macros
- `and` : conjunction of boolean expressions
```lisp
(and (> x 3) (< y 4))
=> (if (> x 3)
         (< y 4)
         nil)
```
- `or` : disjunction of boolean expressions
(or (> x 3) (< y 4))
=> (if (> x 3)
         true
         (< y 4))
- `caar` : takes the first element of the first element of a list
```lisp 
(caar x) => (car (car x))
```
- `cadr` : takes the first element of the rest of a list
```lisp 
(cadr x) => (car (cdr x))
```
- `cdar`, `cddr`, `cadar`, `cadar`, `caddr`, `cdadr`, `caadr`, `cadadr`, `cadaddr`, similar to `caar` and `cadr`.
- `!=`, `neq` : not equal
```lisp
(!= x y) => (! (= x y))
```
- `await-async` await-async block to eval asynchronously while waiting directly for the result
```lisp
(await-async (+ 3 4))
=> (await (async (+ 3 4)))
```

- `apply` apply a function to a list
```lisp
(apply + (3 4))
=> (cons + (3 4))
```
Here is an example
``` lisp
>> (apply + (10 6))
LI>> 16
```

- `cond`: equivalent to successive `if ..  else if .. else`

```lisp
>> (define weather 
(lambda (x)
    (cond ((< x 10) '(It is cold!))
        ((< x 20) '(It is cool!))
        ((< x 30) '(It is warm!))
        (else '(It is hot!)))))
>> (weather 10)
LI>> (It is cool!)
>> (weather 9)
LI>> (It is cold!)
>> (weather 25)
LI>> (It is warm!)
>> (weather 36)
LI>> (It is hot!)
```
Here is an example of how to use it.
```
>> (let ((x 3)
         (y 4))
       (+ x y))
LI>> 7
>> (let* ((x 3)
          (y (x +1)))
        (+ x y))
LI>> 7
```

- `loop` : loop the evaluation of an expression
```lisp
(loop (print 'test))
=> (begin
    (define __loop__
        (lambda nil
            (begin
                (print 'test)
                (__loop__))))
    (__loop__))
```
- `let` and `let*` Used to evaluate a code with locally bound variables. The difference lies in the possibility to bind variables in function of previous bindings with `let*`.
```lisp
(let ((x 3)
         (y 4))
       (+ x y))
=> ((lambda (x y) (+ x y)) 3 4)

(let* ((x 3)
          (y (+ x 1)))
        (+ x y))
=> ((lambda (x) 
        ((lambda (y) (+ x y)) (+ x 1))) 3)
```
- `combine` : *todo*


  

## Lambdas

- `zip` zips elements of two lists in pairs

```lisp
>> (zip '(1 2 3 4) '(5 6 7 8))
LI>> ((1 5) (2 6) (3 7) (4 8))
```
- `unzip` undo a zipped list.

```lisp
>> (define zipped (zip '(1 2 3 4) '(5 6 7 8)))
>> zipped
LI>> ((1 5) (2 6) (3 7) (4 8))
>> (unzip zipped)
LI>> ((1 2 3 4) (5 6 7 8))
```
- `mapf`: maps a function to a list of argument
```lisp
>> (mapf weather '(9 15 25 36))
;defined in example of cond
LI>> ((It is cold!) (It is cool!) (It is warm!) (It is hot!))
```

- `par` evaluates in parallel a list of expressions and awaits on all their result.
```lisp
>> (par (+ 1 2) (+ 3 4))
LI >> (3 7)
```
- `repeat` evaluates n time an expression and returns the result of the last evaluation.
```lisp
>> (repeat '(print 1) 4)
1
1
1
1
LI>> nil
```
- `retry-once` evaluates an expression, and evaluates it again if the result is an err.

  
## Functions

Provides a bunch of utility functions:

- `rand-element` pick a random element from a list

```lisp
>> (rand-element '(10 2 6))
LI>> 2
>> (rand-element '(10 2 6))
LI>> 10
>> (rand-element '(10 2 6))
LI>> 2
>> (rand-element '(10 2 6))
LI>> 6
```
- `enumerate` enumerates all possible sets of combination from several lists.

```lisp
>> (enumerate '(1 2) '(3 4) '(5 6))
LI>> ((1 3 5) (1 3 6) (1 4 5) (1 4 6) (2 3 5) (2 3 6) (2 4 5) (2 4 6))
```

- `contains` returns true if a list contains an element
```lisp
>> (contains '(1 2 3 4 5) 1)
LI>> true
>> (contains '(1 2 3 4 5) 6)
LI>> nil
>> (contains '(1 (2 3) 4 5) '(2 3))
LI>> true
```
- `sublist` : returns a sublist of a list
```lisp
>> (sublist '(a b c d e f) 1)
LI>> (b c d e f)
>> (sublist '(a b c d e f) 1 3)
LI>> (b c)
```
- `transform-in-singleton-list` : transforms a list of arguments in a list of singleton
```lisp
>> (transform-in-singleton-list 1 2 3 4)
LI>> ((1) (2) (3) (4))
```
- `quote-list` : transform a list in list quoting each element
```lisp
>> (quote-list '(1 2 3 4))
LI>> ((quote 1) (quote 2) (quote 3) (quote 4))
```
- `sleep` awaits that the defined time in second is elapsed to resume the evaluation of the next expressions.