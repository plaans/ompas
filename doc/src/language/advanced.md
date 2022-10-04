# Advanced acting functions

- `wait-for` takes a dynamic expression and awaits that the evaluation of the expression returns a **true** value.
```lisp
(wait-for `(= (robot.battery ,?r) 1)))))
```
- `monitor` opposite of wait-for
```lisp
(monitor `(> (robot.battery ,?r) 0.4)))))
```
- `run-monitoring` evaluates an expression while a dynamic expression is true.
```lisp
(run-monitoring `(move ,?r) `(> (robot.battery ,?r) 0.4))
```