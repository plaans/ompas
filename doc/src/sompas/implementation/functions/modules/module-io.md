---
description : Module to print on terminal, read a lisp file, or write to a file
---

# Module IO

### **Functions**

* **print**: print a LValue
```lisp
>> (print test)
test
```
* **__read__**: reads a string from a file. Prefer using the macro **read**.
```lisp
>> (__read__ <path-to-file>/<name-file>.lisp)
```
* **write**: write a LValue to a file.
```lisp
>> (write <path-to-file>/<name-file>.lisp)
```

### **Macros**

* **read** : macro used to read, parse and eval a lisp file.

```lisp
(read test.lisp)
=> (eval (parse (__read__ test.lisp)))
```