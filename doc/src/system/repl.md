# Interacting with the REPL

REPL stands for **Read Evaluate Print Loop**, which means raw expressions are sent to an interpreter which it will execute in real-time without prior compilation.
Such systems are born with Lisp, and the most common language using this feature is Python.
The present system proposes a number of primitives to interact with the system.

--- 

## Monitor

### Principal functions

- `(launch)` launches RAE. It will also launch the platform if defined.
- `(stop)` stops RAE.

- `(configure-platform e1...en)` A number of parameters passed to the platform when launched.
- `(set-select <select-mode>)` configure the algorithm that will be used to select a method during the refinement process.
The available select-mode are *greedy*, *planning*, *aries*, *aries-opt*, *rae-plan*, *c-choice*, *upom* (not yet implemented).
- `(trigger-task <label> <params>)` trigger a task with instantiated parameters. Launch before triggering tasks.
- `(add-task-to-execute <label> <params)` define a task to execute before the launch of RAE. The task will be passed to RAE once the acting system is launched.

### Monitoring
- `(get-state)` returns the whole state of RAE. Contains only initial facts if RAE is not launched yet.
- `(get-resources)` returns the list of resources with some useful information, as the number of waiter for each resource.
- `(get-monitors)` returns the list of dynamic expressions actually monitored.
- `(get-env)` returns the environment of RAE.
- `(get-methods)` returns the list of methods defined.
- `(get-state-functions)` returns the list of state-function.
- `(get-tasks)` returns the list of tasks.
- `(get-config-platform)` returns the config of the platform.
- `(get-select)` returns the select mode used by RAE.
- `(get-agenda <type> <status>)` returns the status of all tasks and commands executed in RAE. The list can be sorted by **type** (task, command) or/and **status** (pending, running, done, failure).
- `(get-task-network)` returns the network of tasks and commands.
- `(get-type-hierarchy)` returns the type hierarchy.
- `(get-stats)` returns a bunch of statistics on the current RAE run.
- `(export-stats <csv-file>)` exports the stats in a *csv* file.

### Operational model extraction

Those functions are used to test and debug the extraction of planning models in the form of chronicles from the automated analysis of the method operational models and the command models. The planner used is Aries @@godetChroniclesRepresentingHierarchical2022, the project is available here : [https://github.com/plaans/aries](https://github.com/plaans/aries)

- `(convert-expr e)` converts the expression *e* into a chronicle and prints it.
- `(convert-domain)` converts the domain and prints it.
- `(plan-task t p1...pn)` converts the domain and calls the planner *aries* 

---
## Domain definition

### **Commands**

Commands are defined with the macro `def-command` that takes a symbol, and a list of typed parameters.

```lisp
(def-command pick (:params (?obj ball) (?room room) (?gripper gripper)))
```

A model can be provided and defined with the macro `def-command-pddl-model`. Here is an example for the command *pick*. The pre-conditions takes a list of dynamic expressions (expressions which result will depend on the current values of state-variables).

```lisp
(def-command-pddl-model pick
    (:params (?obj ball) (?room room) (?gripper gripper))
    (:pre-conditions
        (= (at ?obj) ?room)
        (= (at-robby) ?room)
        (= (carry ?gripper) no_ball))
    (:effects
        (begin
            (assert `(carry ,?gripper) ?obj)
            (assert `(at ,?obj) no_place))))
```


<!--- This function creates a lambda that will be stored in the environment with this form:

```lisp
(lambda (?r) (rae-exec-command (quote pick) ?r))
```
-->

### **Tasks**

A task is defined with the macro `def-task` that takes a label, and a list of typed parameters.

```lisp
(def-task pick-and-drop (:params (?ball ball) (?room room)))
```

### **Methods**

A method is defined with the macro `def-method` that takes a label, the task it refines, a list of typed parameters, the first ones inherited from the task, a score to sort the method for the basic select of RAE, and the body that is the program executed by the platform is the method is selected.

```lisp
(def-method m1
    (:task pick-and-drop)
    (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
    (:pre-conditions 
        (= (at ?ball) (at-robby))
        (= (carry ?gripper) no_ball)
        (= ?departure (at-robby)))
    (:score 0)
    (:body
        (do
            (pick ?ball ?departure ?gripper)
            (move ?departure ?room)
            (drop ?ball ?room ?gripper))))

```

**State-function**

A state function returns the value of state-variable at a given time.
We define a state function with the macro `def-state-function` that takes a label, a list of typed parameters, and a result type.

```lisp
(def-state-function at (:params (?b ball)) (:result room))
```

### **Lambdas**

Lambdas can be defined in the evaluation environment of RAE, which is different from the environment of the REPL. A lambda is defined with the macro `def-lambda` that takes the label of the lambda, and a lambda.

```lisp
(def-lambda go_random
    (lambda (?r ?l ?u)
        (let ((x (rand-int-in-range ?l ?u))
                (y (rand-int-in-range ?l ?u)))
                (navigate_to ?r x y))))
```


### **Types**

The system supports the definition of new types, with a hierarchy of types. A type is defined with the macro `def-types` that takes a list of types.

```lisp
;adding new root types
(def-types room gripper ball)
;adding a new subtype to ball
(def-types (football ball))
```


### **Objects**

Typed objects can be defined in the fact base of the system, in particular when using the system in simulation.
We use the macro `def-objects` that takes lists of objects with their type defined as the last element of the list.

```lisp
 (def-objects
    (bedroom kitchen living_room room)
    (b1 b2 b3 b4 ball))
```
### **Initial-state**

To define an initial-state for some scenarios (for example to use RAE in simulation), we use the macro `def-initial-state` that takes a list of pair <key,value>.

```lisp
(def-initial-state
    (at-robby living_room)
    ((at b1) bedroom)
    ((at b2) bedroom)
    ((at b3) bedroom)
    ((at b4) bedroom)
    ((carry left) no_ball)
    ((carry right) no_ball))
```