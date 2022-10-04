---
description: Core functions of scheme
---

# Core Operator

Core operators are the very basic functions present directly in the **eval** function:

* **Define** : binds a symbol to a LValue

```lisp
(define x 10); the symbol will be bound to the value x
```

****

* **Set**: modify the value of a symbol already defined in the environment.

```lisp
(set! x 12); x previously bound the value 10 will now have the value 12
```

* **If** : Basic conditional structure. Evaluates an expression that returns a boolean, executes two differents branches in function of the result.

```lisp
>> (if (> 3 10) (+ 3 3)  (* 10 5))
LI>> 50
```

* **Begin**: Evaluates a list of expressions and return the result of the last.

```lisp
>> (begin (define x 10) (define y 20) (+ x y))
LI>> 30
```

* **Eval**: Explicitely evaluates an expression.

```lisp
'(* 3 3)
LI>> (* 3 3)
(eval '(* 3 3))
=> 9
```

* **DefLamda**: creates a lambda expression.

```lisp
>> (define square (lambda (x) (* x x)))
; defines a new lambda in the environment binded to symbol 'square' 
>> (square 5)
LI >> 25
```

There is two ways to define the arguments of a lambda. `(lambda args body)` will consider _args_ as a list of arbitrary length when `(lambda (x y) body)` will wait for two arguments that will be bound to _x_ and _y_.

* **DefMacro**: defines a lambda that format lisp code

```lisp
(defmacro cadr (lambda (x) `(car (cdr ,x))))
(cadr '(3 4)); <=> (car (cdr '(3 3)))
LI >> 4
```

* **Async** & **await**: Evaluates asynchronously an expression. ~~Returns a pid (usize)~~ Returns a future. The keyword **await** wait on the future and return the LValue if the result is Ok, or propagate the error otherwise.

```lisp
>>(await (async (*3 3)))
LI>> 9
;we can split await and async to launch several async blocks and await on the results later
>> (let ((x (async (* 3 3))); x is the pid of the async block
       (y (async (* 4 5)))); y is the pid of the async block
      (+ (await x) (await y)))
LI>> 29
```

* **Quote**: Prevent evaluation of the expression

```lisp
>>(quote (*3 3))
LI>> (* 3 3)
```

* **QuasiQuote** & **UnQuote**: Creates a block in which everything is quotted except unquotted expressions:

```lisp
>> (quasiquote (3 (+ 6 7) (unquote (- 2 1)))))
LI>> (3 (+ 6 7) 1); the unquotted expression has been evaluated
```

* **Eval :** Evaluates an expression:&#x20;

```lisp
>>(eval (cons + (list 3 3))
LI>> 6
```

* **Parse :** Parse a string into an LValue

```lisp
>>(parse "(+ 3 3)")
LI>>(+ 3 3)
```

* **Expand :**  Expand an expression

```lisp
>> (expand (parse "(and (= 3 3) (< 4 5))"))
LI >> (if (= 3 3) (< 4 5) nil)
```
