---
description : Module to generate errors.
---

# Module error

Define way to return a Result\<O,E> in the Lisp

### **Functions**

* **ok:** construct a ok value from a LValue

```lisp
>> (ok test)
(ok (test))
```

* **err:** construct a err value from a LValue
```lisp
>> (err test)
LI>> (err (test))
```
* **ok?:** returns true if the LValue is a (Ok \<lvalue>), false if it is a (Err \<lvalue>)
```lisp
>> (ok? (ok test))
LI>> true
```
* **err?:** returns true if the LValue is a (Err \<lvalue>), false if it is a (Ok \<lvalue>)
```lisp
>> (err? (err test))
LI>> true
```
