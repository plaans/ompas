---
description: Module to handle definition and test of types
---
# Module type

### **Functions**

* **get-type** : Returns the type of an LValue. If it is a symbol and has a defined type, it returns the type.
* **type-of** : Bind a type to a symbol. The symbol can either be an object of a certain type, a sub-type of a type or a state-function.
* **sub-type** : Define a sub-type.
* **new-sf** : Define a new state-function.
* **new-obj** : Define a new object of a certain type
* **sf?** : Returns true if the symbol is a state function.
* **obj?** : Returns true if the symbol is an object.
* **type?** : Returns true if the symbol is a type.

### Complete Example:
```lisp
;defining a new type
>> (type-of room (sub-type object))
LI>> nil
>> (get-type room)
LI>> subtype of object

;defining a new object
>> (type-of bedroom (new-obj room))
LI>> nil
>> (get-type bedroom)
LI>> room

;defining a new state-function
>> (type-of robot (sub-type object))
LI>> nil
>> (type-of at (new-sf robot room))
LI>> nil
>> (get-type at)
LI>> (robot) = room

;types of symbol tests
>> (sf? at)
LI>> true
>> (sf? bedroom)
LI>> nil
>> (obj? bedroom)
LI>> true
>> (type? bedroom)
LI>> nil
>> (type? room)
LI>> true

;returns the LValue kind when not a symbol with type
>> (get-type 10)
LI>> int
>> (get-type 10.1)
LI>> float
>> (get-type test)
LI>> symbol

```



