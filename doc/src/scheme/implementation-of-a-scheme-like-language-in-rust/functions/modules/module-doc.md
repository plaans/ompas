---
description : Module used to define a documentation
---

# Module doc

### **Functions**
It provides a unique function **help** that prints the whole help if called without arg. Prints verbose doc of symbol if called with a symbol.

```lisp
>> (help)
LI>> ¤ * : Takes 2+ arguments. Ret...
¤ + : Takes 2+ arguments. Return t...
¤ - : Takes 2 arguments. Return th...

>> (help write)
LI>> Write a LValue to a file
Takes two arguments: the name of the file and the LValue
Note: The path of the file is relative to the path of the executable

```
