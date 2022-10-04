# Acting Language

The system OMPAS uses a custom acting Language.
It is a Lisp dialect and in particular a Scheme variant, with a limited set of primitives.
On top of the basic Scheme, primitives for concurrency have been added, and interruptions are supported in the core language.
From this reimplementation of Scheme, we propose an acting language that supports information gathering, reasoning on facts, execution and monitoring of commands as well as a specific resource management system.