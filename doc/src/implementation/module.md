---
description: Description of a Module
---

# Modules

A Module is a structure that load in the lisp environment bindings between symbols and LValues.

A Module is composed of the following attributes:

* **ctx :** a pointer to a struct used by the functions it provides.
* **prelude :** a set of bindings between symbols and LValue.
* **raw\_lisp :** a set of scheme code that will be evaluated when the module is loaded. Most of the time it contains lambda and macros definitions.
* **label :** the name of the module.
