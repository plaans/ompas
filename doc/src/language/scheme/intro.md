# Scheme

Scheme is derived from the functional language Lisp.
Peter Norvig made a tutorial to implement a basic version in Python available at [https://norvig.com/lispy.html](https://norvig.com/lispy.html) and [https://norvig.com/lispy2.html](https://norvig.com/lispy2.html).
The Rust implementation is inspired by Lis.py and Lispy.py (the second version integrates advanced features) but differs on several aspects.

## Introduction to Scheme.

Scheme is a Lisp dialect that is based on the recursive evaluation of expressions that we call *LValue*. An *LValue* is either an Atom (symbol, boolean, number, function) or a list of expressions.
A particularity of the language is that the boolean *false* is represented by the *LValue* *Nil* that represents also the empty list.
An expression is evaluated in an environment, in which some symbols can be bound to LValues.
Each expression is a new scope, which means that a new binding only lives in the scope it has been defined in, which makes the language way more simple.

## Core language
The core of the Scheme implementation has been both simplified, and extended for concurrency.
The core language is composed of the following functions:
- `begin` evaluates a list of expressions and returns the result of the last expression.
- `define` defines in the current scope a new binding for a symbol.
- `if` takes as argument an evaluation that returns a boolean *b*.
In function of the value of *b*, either the second expression of the third will be executed.
The result of the *if* expression is the result of the evaluated expression.
- `lambda` creates a closure.
A closure is defined by parameters and a body.
The valid kind of parameters for a lambda are *nil* for no parameters, a simple symbol for an arbitrary number of parameters or a list of symbol for a defined number of parameters.
- `quote` or `'` avoids the evaluation of an expression and returns the raw expression as result.
- `quasiquote` (or &#96;) and `unquote` (or `,`) are used to partially evaluate an expression. Those functions can be viewed as macros for the formatting of expressions using *quote* and *cons*.
- `eval` evaluates an expression.
- `parse` takes as input a string and returns the parsed and expanded expression.
- `expand` expanses an expression.
- `enr` evaluates an expression without evaluating the sub expressions.
```lisp
>> (eval (cons zip (list (list 2 3) (list 4 5))))
LI>> error: In eval, 2: Got int, expected fn
>> (enr (cons zip (list (list 2 3) (list 4 5))))
LI>> ((2 4) (3 5))
```

### Errors

A specific *Err* *LValue* as been added in the current implementation for special cases were an err as result would escape the evaluation of an expression.

- `do` similar to begin, but stops at the first err returned.
The result of the expression is *err*.
- `check` takes as argument a boolean.
It returns true if the boolean is true, an *err* otherwise

From those two functions, some interesting constructs can be programmed for code that should not return errors.
### Concurrency and interruption

To handle concurrency, we implemented an extension to Scheme inspired by asynchronous code from C++ and rust, using schedulers and futures.
In addition, we define a new *LValue* named *handle* that represents an asynchronous evaluation.
A handle can be manipulated with the following functions: 

- `async` takes as argument an expression, starts the evaluation in a new asynchronous process and returns a handle.
- `await` takes as parameter a handle and returns the result of the expression evaluated in the asynchronous process.
- `interrupt` takes as argument a handle, and sends an interruption signal to the asynchronous evaluation.
The interruption signal is propagated recursively to all sub-expressions.

In addition to those primitives, it is possible to define the interruptibility of an expression, by avoiding the interruption of some expressions with the keyword *uninterruptible*.
Here is an example of such code:
```lisp
(begin
    (define h
    (async 
        (begin
        (uninterruptible (begin
            (exec pick ?r ?o)
            (exec move ?r ?l)
            (exec drop ?r ?o)))
        (exec inspect ?r ?o))))
    (race 
        (await h)
        (begin
            (sleep 10)
            (interrupt h))))
```

## Standard library

### Functions to get informations on the environment

- `help` returns the list of functions defined in the environment along their documentation.
An extended documentation can be given by giving as parameter a specific function to `help`.
```lisp
>> (help)
LI>> ¤ * : Takes 2+ arguments. Ret...
¤ + : Takes 2+ arguments. Return t...
¤ - : Takes 2 arguments. Return th...

>> (help write)
LI>> Write a LValue to a file
Takes two arguments: the name of the file and the LValue
Note: The path of the file is relative to the path of the executable

```
- `get_keys` returns the list of all elements defined in the environment.
- `get_macros` returns the list of macros.
- `get_macro` returns the expression of a given macro.
- `get_contexts` returns the list of all contexts defined in the environment.

### Get and set functions

- `get` returns an element either from a list or a map. You can also use directly `get-list` and `get-map`.
```lisp
>> (get (list 1 2 3) 1)
LI>> 2
>> (get (map '((ten 10) (twenty 20))) twenty)
LI>> 20
```

- `set`: function to set an element in a list or a map
```lisp
>> (set (list 1 2 3) 1 1)
LI>> 2
>> (set (map '((ten 10) (twenty 20))) '(twenty twenty))
LI>> twenty: twenty
ten: 10
```
### Functions on list

- `list`: constructs a list.
```lisp
>>(list 1 2 3)
LI>>(1 2 3)
```
- `car`: returns the first element of a list, nil if the list is empty.

```lisp
>>(car '(3 4 5))
LI>> 3
```

- `cdr`: returns list without the first element, nil if the list has one element.

```lisp
>>(cdr '(3 4 5))
LI>>(4 5)
```

- `first`: returns the first element of a list (same as **car**)

```lisp
>>(first (list 1 2 3))
LI>> 1
```

- `second`: returns the second element of a list
```lisp
>>(second (list 1 2 3))
LI>> 2
```
- `third`: returns the third element of a list
```lisp
>>(second (list 1 2 3))
LI>> 3
```
- `rest`: returns the rest of a list (same as cdr)
```lisp
>> (rest '(1 2 3))
LI>> (2 3)
```
- `last`: returns the last element of a list.
```lisp
>>(last '(3 4 5))
LI>>5
```
- `cons`: creates a cons of two elements

```lisp
>> (cons 3 nil)
LI>> (3)
>> (cons nil 3)
LI>> (nil 3)
>> (cons 4 '(3))
LI>> (4 3)
>> (cons '(4) '(3))
LI>> ((4) 3)
```
- `len`: returns the length of a list of a map
```lisp
>>(len '(1 2 3))
LI>> 3
```
- `empty`: returns true is a list or a map is empty
```lisp
>> (empty '())
LI>> true
```
- `get-list` : get the element of the list at the given index (starts at 0). The first argument is a list, and the second the index
```lisp
>> (get-list (list 1 2 3) 1)
LI>> 2
```
- `set-list` : set the element of the list at the given index (starts at 0).
The first argument is a list, the second the value, and the third the index.
```lisp
>> (set (list 1 2 3) 1 1)
LI>> 2
```
- `append` : append two lists together
```lisp
>> (append (list 1 2 3) '(4 5))
LI>> (1 2 3 4 5)
```
- `member` : It takes two arguments of which the second must be a list,
if the first argument is a member of the second argument, and then it returns the remainder of the list beginning with the first argument.
```lisp
>> (member 3 '(1 2 3 4 5))
LI>> (3 4 5)
```
- `reverse` : reverse a list
```lisp
>> (reverse '(1 2 3 4 5))
LI>> (5 4 3 2 1)
```
- `intersection` returns a list containing all elements present in all the lists passed as arguments.
```lisp
>> (intersection '(1 2 3 4 5) '(1 2 3) '(3 4 2))
LI>> (2 3)
```

### Functions on map

- `map`: constructs a map.
```lisp
>> (map '((ten 10) (twenty 20)))
LI>> twenty: 20
ten: 10
```
- `get-map`: get the value inside a map

```lisp
>> (define m1 (map '((test . 10))))
>> (get m1)
LI>> test: 10
>> (get-map m1 test)
LI>> 10
```
- `set-map`: returns a map with the new value inserted.
```lisp
>> (define m1 (set-map m1 '(twenty . 20)))
>> (get m1)
LI>> twenty: 20
test: 10
```

- `union-map` : unifies two maps
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> map1
LI>> ten: 10
twenty: 20

>> (define map2 (map '((thirty 30) (fourty 40))))
LI>> nil
>> map2
LI>> thirty: 30
fourty: 40

>> (define map3 (union-map map1 map2))
LI>> nil
>> map3
LI>> thirty: 30
twenty: 20
fourty: 40
ten: 10
```
- ̀`remove-map` : remove from the map an entry. The first argument is a map, and the second the key.
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> (define map2 (map '((thirty 30) (fourty 40))))
LI>> nil
>> (define map3 (union-map map1 map2))
LI>> nil
>> (define map4 (remove-map map3 twenty))
LI>> nil
>> map4
LI>> thirty: 30
fourty: 40
ten: 10
```
- `remove-key-value-map` : remove a key-value binding if it exists.
```lisp
>> (define map1 (map '((ten 10) (twenty 20))))
LI>> nil
>> (define map2 (remove-key-value-map map1 '(twenty 20)))
LI>> nil
>> map2
LI>> ten: 10
>> (define map2 (remove-key-value-map map1 '(ten 1)))
LI>> error: In remove-key-value-map, map does not have key value (ten:1)
```

### Mathematical functions

- basic math functions:
  - not (!)
  - add (+)
  - sub (-)
  - div (/)
  - mul (\*)
  - eq (=)
  - gt (>)
  - ge (>=)
  - lt (<)
  - le (<=)

### Type verification
  - `lambda?` : returns true if the LValue is a lambda.
  - `integer?`: returns true if the LValue is an integer.
  - `float?`: returns true if the LValue is a float.
  - `nil?`: returns true if the LValue is nil.
  - `bool?`: returns true if the LValue is a boolean.
  - `symbol?`: returns true if the LValue is a symbol.
  - `fn?`: returns true if the LValue is a function.
  - `mut-fn?`: returns true if the LValue is a mutable function.
  - `quote?`: returns true if the LValue is a quote.
  - `map?`: returns true if the LValue is a map.
  - `list?`: returns true if the LValue is a list.
  - `pair?`: returns true if the LValue is a list with at least one element.

All above functions are defined in the _root_ environment.

## Error

- `err` construct an err value from a *LValue*
```lisp
>> (err test)
LI>> (err (test))
```

- `err?` returns true if the *LValue* is a (Err \<lvalue>), false if it is a (Ok \<lvalue>)
```lisp
>> (err? (err test))
LI>> true
```

- `interrupted?` returns true is the *LValue* is an err coming from an interruption. 