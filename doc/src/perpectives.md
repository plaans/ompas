# Perpectives



### What is still to be done

* **Check proper tail recursion:** A task that is called recursively will provoke in the end stack overflow. In the same way, the function `expand` provokes on windows stack overflow on too large codes.
* **Implement agenda for methods:** For the moment, we execute methods, without retry mechanism. We need to store instances of methods that have been already tried. When a method fails, we need to try other methods while there is still untried.
* **Implement enumeration of applicable methods:** For the moment, we can only define methods with the same parameters as the task. We need to enumerate instances of methods that are applicable and then choose the best one.
* **Create jobshop problem**

### Future work for RAE V2.

* Generation of chronicles/ppdl problems/hddl problems from operationnal models to call a planner online (aries/fape)
* STN network for actions.
