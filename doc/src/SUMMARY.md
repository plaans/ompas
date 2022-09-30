# Table of contents

- [Operationnal Model Planning and Acting System (OMPAS)](README.md)

# SOMPAS

- [Acting Language]()
  - [Acting primitives]()
  - [Resource management]()
  - [Advanced acting functions]()
  - [Acting domain description](rae/description.md)
- [Scheme]()
  - [The sompas Interpreter](sompas/interpreter.md)
  - [Implementation of a sompas-like language in Rust](sompas/implementation/README.md)
    - [LValue](sompas/implementation/lvalue/README.md)
      - [LValueS](sompas/implementation/lvalue/lvalues.md)
    - [LError](sompas/implementation/lerror.md)
    - [Environment](sompas/implementation/environment.md)
    - [Functions](sompas/implementation/functions/README.md)
      - [Core Operator](sompas/implementation/functions/core-operator.md)
      - [Lambda](sompas/implementation/functions/lambda.md)
      - [Modules](sompas/implementation/functions/modules/README.md)
        - [Module root](sompas/implementation/functions/modules/module-root.md)
        - [Module IO](sompas/implementation/functions/modules/module-io.md)
        - [Module String](sompas/implementation/functions/modules/module-string.md)
        - [Module utils](sompas/implementation/functions/modules/module-utils.md)
        - [Module math](sompas/implementation/functions/modules/module-math.md)
        - [Module error](sompas/implementation/functions/modules/module-error.md)
        - [Module doc](sompas/implementation/functions/modules/module-doc.md)
        - [Module type](sompas/implementation/functions/modules/module-type.md)
        - [Deprecated modules](sompas/implementation/functions/modules/deprecated-modules/README.md)
          - [Module robot](sompas/implementation/functions/modules/deprecated-modules/module-robot.md)
          - [Module counter](sompas/implementation/functions/modules/deprecated-modules/module-counter.md)
  - [Create binaries with the interpreter.](sompas/binaries.md)
  - [The REPL](sompas/repl.md)
  - [How to run the Scheme interpreter](sompas/run.md)
  - [The task handler](sompas/taskhandler.md)


# Acting System

- [Refinement Acting Engine (RAE)](rae/def.md)
  - [Algorithms](rae/algorithms.md)
  - [Define a Platform in RAE](rae/platform-definition.md)
  - [How to run RAE](rae/how-to-run-rae.md)

---

- [Perpectives](perpectives.md)
- [Bibliography](bibliography.md)
