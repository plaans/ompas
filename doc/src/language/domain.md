# Describe an acting domain using macros in SOMPAS

## **Commands**

Commands are defined with a label and a list of typed parameters.

```lisp
(def-command pick (:params (?r robot)))
```

## **Tasks**

Tasks are abstract objects that needs to be refined in methods in function of the context of execution. A task is defined by a label and a list of typed parameters.

```lisp
(def-task t_navigate_to (:params (?r robot) (?x int) (?y int)))
```

## **Methods**

A method is defined by a label, the task it refines, a list of typed parameters, a list of pre-conditions, and a body composed of SOMPAS code.

```lisp
;define a method body
(def-method m_navigate_to 
   (:task t_navigate_to)
   (:params ?r ?x ?y)
   (:pre-conditions true)
   (:body
      (do
         (navigate_to ?r ?x ?y)
         (navigate_to ?r (+ ?x 1) (+ ?y 1)))))
```

## **State-function**

A state function returns the value of fact at a given moment.
We define a state function by a label of the state function, a list of typed parameters and a result type.

```lisp
(def-state-function robot.coordinates (:params ?r robot) (:result (tupe int int)))
;defines the state function that will return the coordinates of a robot.
```

## **Lambdas**

Add a function that will be used in the execution environment (for example when a part of the code needs to be used several times).

```lisp
(def-lambda
   go_random 
   (lambda (?r ?l ?u)
      (let ((x (rand-int-in-range ?l ?u))
            (y (rand-int-in-range ?l ?u)))
         (navigate_to ?r x y))))
```
## **Types**
Define some symbols as types.
Support for types hierarchy.
```lisp
;room is a subtype of location
(def-types '(room location) gripper ball)
```

## **Objects**

Objects can be defined in the state of the system. 
Define typed objects in the state.
```lisp
(def-objects
      (left right gripper)
      (no_place room)
      (no_ball ball))
```

## **Initial facts**

Set initial facts in the state of the acting engine.
It is mainly used when the system is for pure simulation usage of OMPAS.
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
