# Refinement Acting Engine (RAE)

## Introduction

The Refinement Acting Engine is an acting engine first described in _Planning and Acting_ from Ghallab & Co. A first version has been developped by Sunandita in Python. This version integrates a first prototype of RAE working in simulation. The select method to refine a task was using RAEPlan, UPOM or UPOM with learning capabilities. The goal of my thesis is to work from operationnal models to generates plans using planning technics, and then executes the plan. In our scenario, planning is continuously working online in parallel of the execution. Previous works exist combining both approches to maximise long-term utility and prevent deadlock (Propice-Plan, FAPE, others?) The first step has been to look at the state-of-the-art in term of executing engine and their languages. From this preliminary search, the choice has been made to base the RAE Description Language (RAEDL) on Scheme. Once the language has been defined, we used it to build RAEDL on top of it. RAEDL is used to describe domains and the environment (as special functions and initial facts that can be asserted).

## Architecture

The executable of RAE is decomposed in several processus and asyncrhonous tasks. The following diagram gives an overview of the Architecture and how tasks are communicating, what object is shared between tasks and threads etc.

*   The _main_ process is the process that is launched with the command `cargo run`, i.e. ~~_unnamed\_supervisor_~~  _ompas._

    At initialisation it launches a repl that communicates via a channel with the Scheme Interpreter task.

    This task _await_ on a channel to receive raw (str) Scheme expression, that will be parsed, expanded and evaluated.

    The result is then sent back to the REPL to be printed on stdout. Other processes can call the Scheme Interpreter to Evaluate Expressions.

    However the return of the value is only available for the REPL Task (To be implemented later if future case need this feature...)
* The task RAE is launched via the REPL with the command `(rae-launch)`. It will successively launch _rae\_main_ and the platform. In our case _godot3_ via command line loaded with an hardcoded scenario and options. The platform godot then opens a tcp connection to receive state updates and action status updates.
*   The objects RAEState and Status are shared between the different tasks (using _Arc_).&#x20;

    ****

## ****



