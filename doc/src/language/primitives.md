# Acting primitives

The acting language is built on top of the new implementation of Scheme with the following functions and lambdas:

- `exec-command` sends an execution request to the platform a command and returns an execution handle.
```lisp
(exec-command pick r1) -> handle
```
- `exec-task` executes a task thanks to the RAE algorithm.
- `read-state` returns the current value of a state variable.
```lisp
(read-state at r1) -> bedroom
```
- `arbitrary` : arbitrarily takes an element of a list using a function that can be passed as argument, the default behavior being to take the first element of the list.
```lisp
>>(arbitrary '(3 4 5))
LI>> 3
>>(arbitrary '(3 4 5) last)
LI>> 5
```
- `+>/assert` add/update a fact in the state.
<> - **->/retract:** Delete a fact in the state.
- `get-state` returns a map of the perceived state.
- `get-facts` returns a map with the perceived state, the resources, and the instances.
- `instance` is used to reason on the presence of objects, and their types.
    - If no argument is passed, the function will return a map with all the objects of each type.
    - If one argument is passed, it should be a type, and the function will return all elements of the type, including elements of subtypes.
    - If two arguments are passed, it should be an object and a type.
    The function returns true if the object is of the given type.


