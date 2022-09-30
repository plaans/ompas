# RAE Description(Operationnal?) Language (RAEDL)

## Introduction

This language is built on top of the Scheme language we developped. It is used to define an environment in which RAE can execute tasks and methods. Several elements can be defined:

## Describe a RAE Domain

### **Primitive actions**

&#x20;Primitive actions are defined with a list a symbol. The first symbol is the label of the command, and the rest the labels of the parameters. Parameters are not typed.

```
(defaction pick ?r)
;defines the action pick that take the parameter ?r
```

This function creates a lambda that will be stored in the environment with this form:

```
(lambda (?r) (rae-exec-command (quote pick) ?r))
```

### **Tasks**

&#x20;Tasks are abstract objects that needs to be refined in methods in function of the context of execution. A task is defined by a list of symbols corresponding to the label of the task and the labels of the parameters.

```
(def-task t_navigate_to ?r ?x ?y)
;transformed into a lambda into RAE enviroment.
(lambda (?r ?x ?y) (progress (quote navigate_to) ?r ?x ?y))
```

### **Methods**

Way of refining a task. A method can have the same number or more parameters than the task it refines. If it has more parameters, it needs to define a way to enumerate the list of the set of applicable parameters to the method.

```
;define a method body
(def-method m_navigate_to 
        '((:task t_navigate_to)
          (:params ?r ?x ?y)
          (:pre-conditions true)
          (:effects nil)
          (:parameters-generator nil true)
          (:score-generator 0)
          (:body (begin
                        (rae-await (navigate_to ?r ?x ?y))
                        (rae-await 
                                (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))

;What is defined in RAE environment
-task: t_navigate_to
-pre-conditions: (lambda (?r ?x ?y)
                   true)
-effects: (lambda (?r ?x ?y)
            nil)
-generator: (lambda args
              (begin
                 (define eval_params
                       (lambda args
                          ((lambda (params)
                                (if (null? params)
                                    nil
                                    (if (eval (cons (lambda (?r ?x ?y) true) params))
                                        (cons 
                                           (list 
                                              (cons (quote m_navigate_to) params)
                                                 (eval
                                                    (cons
                                                       (lambda (?r ?x ?y) 0) params)))
                                           (eval_params (cdr args)))
                                        (eval_params (cdr args))))) (car args))))
                 (eval_params (eval (cons enumerate (append args (quote nil)))))))
-body: (lambda (?r ?x ?y)
         (begin
            (rae-await (navigate_to ?r ?x ?y))
            (rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))

```

**State-function**

A state function returns the value of fact at a given time. We define a state function by a list of symbols. The first is the label of the state function, the rest the labels of the parameters.

```
(def-state-function robot.coordinates ?r)
;defines the state function that will return the coordinates of a robot.
(lambda (?r)
    (rae-get-state-variable (quote robot.coordinates) ?r))
```

### **Lambdas**

Add a function that will be used in RAE (for example when a part of the code needs to be used several time)

```
(def-lambda '(go_random (lambda (?r ?l ?u)
                            (let ((x (rand-int-in-range ?l ?u))
                                  (y (rand-int-in-range ?l ?u)))
                                  (rae-await (navigate_to ?r x y))))))
```

### **Initial facts**

Set initial facts in RAE. Those facts will be stored in the inner world part of the state.

```
(def-initial-state '((robots . ())(machines . ())(packages . ())))
;example of initial facts that can be defined.
```

## **RAE** Primitives

* **rae-exec-command:** Exec on the platform a command, and returns the id of the action to monitor it.
* **rae-cancel-command:** Used in synergy with **rae-exec-command** to cancel a command on the platform.
* **rae-await:** Used in synergy with **rae-exec-command** to monitor the status of an action.
* **+>/assert:** Add/update a fact in the state.
* **->/retract:** Delete a fact in the state.
* **rae-get-state:** Return a map of the state.
* **rea-get-state-variable:** Return the value of aa state variable.
* **rae-get-methods:** Return the applicable methods of a task
* **rae-get-best-method:** Return the best applicable method.
* **rae-select:** Takes as input a task and its applicable methods, create a new refinement stack in the agenda and return the best method to apply and a task id.
* **rae-set-success-for-task**: Takes as input a task id, set the task as success and remove it from the agenda.
* **rae-get-next-method:** Takes as input a task id. Update the refinement stack, setting the current method as tried and getting the next applicable method, setting it as _current method_.
