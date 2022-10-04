# Operational Model Planning and Acting System (OMPAS)

_OMPAS_ is a complete supervision solution for robotic platforms developed in the context of the thesis "Planning from operational model for deliberative acting in robotics".
This thesis is led by Jérémy Turi under the supervision of Arthur Bit-Monnot and Daniel Sidobre at LAAS-CNRS in the RIS team.
This official documentation is updated as the project goes, but some shift may appear with the actual implementation.

The system _OMPAS_ is based on the Refinement Acting Engine, that it extends with a native support for tasks concurrency, with a dedicated resource management system. The system uses a Scheme-like language to describe the behavior of a robotic agent as hierarchical operational models.

Several articles have been accepted in conferences and workshops.
Here are the references of the paper:
- [Extending a Refinement Acting Engine for Fleet Management: Concurrency and Resources](https://hal.archives-ouvertes.fr/hal-03792874)(@@turiExtendingRefinementActing2022)
- [Guidance of a Refinement-based Acting Engine with a Hierarchical Temporal Planner](https://hal.archives-ouvertes.fr/hal-03690039) (@@turiGuidanceRefinementbasedActing2022)

---

<!---
## Refinement Acting Engine (RAE)

### Overview

The Refinement Acting Engine is an acting engine first described in _Planning and Acting_ from Ghallab & Co. A first version has been developped by Sunandita in Python. This version integrates a first prototype of RAE working in simulation. The select method to refine a task was using RAEPlan, UPOM or UPOM with learning capabilities. The goal of my thesis is to work from operationnal models to generates plans using planning technics, and then executes the plan. In our scenario, planning is continuously working online in parallel of the execution. Previous works exist combining both approches to maximise long-term utility and prevent deadlock (Propice-Plan, FAPE, others?) The first step has been to look at the state-of-the-art in term of executing engine and their languages. From this preliminary search, the choice has been made to base the RAE Description Language (RAEDL) on Scheme. Once the language has been defined, we used it to build RAEDL on top of it. RAEDL is used to describe domains and the environment (as special functions and initial facts that can be asserted).

### Architecture

The executable of RAE is decomposed in several processus and asyncrhonous tasks. The following diagram gives an overview of the Architecture and how tasks are communicating, what object is shared between tasks and threads etc.

*   The _main_ process is the process that is launched with the command `cargo run`, i.e  _OMPAS_

    At initialization it launches a repl that communicates via a channel with the Scheme Interpreter task.

    This task _await_ on a channel to receive raw (str) Scheme expression, that will be parsed, expanded and evaluated.

    The result is then sent back to the REPL to be printed on stdout. Other processes can call the Scheme Interpreter to Evaluate Expressions.

    However the return of the value is only available for the REPL Task (To be implemented later if future case need this feature...)
* The task RAE is launched via the REPL with the command `(rae-launch)`. It will successively launch _rae\_main_ and the platform. In our case _godot3_ via command line loaded with an hardcoded scenario and options. The platform godot then opens a tcp connection to receive state updates and action status updates.
*   The objects RAEState and Status are shared between the different tasks (using _Arc_).&#x20;

-->