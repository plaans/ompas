# Operational Model Planning and Acting System (OMPAS)

The Operational Model Planning and Acting System is an acting engine based on the Refinement Acting Engine (RAE).
It is a framework to endow an automated system with deliberation capabilities.
The agent skills are defined in a Lisp dialect called Scheme OMPAS (SOMPAS), which are then executed in OMPAS.
OMPAS is capable of reactively handling new tasks to achieve, while coping with contingency events and execution errors such as unexpected action outcome.
It also features an integrated planner, used to guide the acting decisions of the executor with lookahead.

## Scope of the project
This project is developed within the framework of Jérémy Turi's thesis conducted at LAAS-CNRS in the RIS team, under the supervision of Arthur Bit-Monnot and Daniel Sidobre. 

## Documentation
The documentation is available at this gitbook: https://plaans.github.io/ompas/

# Usage

## Building

The compiling of the project requires the latest version of rust's development suite.
You can find installation guidelines on [rust website](https://www.rust-lang.org/tools/install)
Prior to compiling, you will need to fetch the git submodules source code. You can use the following commands:
- `git submodule init`
- `git submodule fetch`


Now, you can build the project. First, move to the root folder of the project and execute the following command in a command line prompt: `cargo build`.

Note: Some warning might appear, feel free to notify them to the development team in the issue tab.
## Libraries
The project is decomposed into several libraries:
- *scheme*: library containing the source code of the SOMPAS interpreter and all the standard libraries of SOMPAS. The preliminary design of SOMPAS was based on Lispy and Lispy2 by Peter Norvig. You can find them at [norvig.com/lispy](https://norvig.com/lispy.html) and [norvig.com/lispy2](https://norvig.com/lispy2.html).

- *acting*: library containing all the models and executor of OMPAS.
The library is mostly defined in the *core* submodule, which is decomposed as follows:
    - *model*: contains all the data structures to represent an acting system.
    - *ompas*: the acting executor along the Scheme libraries.
    - *planning*: contains all data structures and algorithms to convert operational models into descriptive models and encode the problem for the aries solver.
- *middleware*: structures to handle in a common API all threads of OMPAS and other programs launched by OMPAS.
- *aries*: CSP solver used to plan. The source code comes from [https://github.com/plaans/aries]().
- *benchmark*: executables to benchmark the OMPAS system on several domains and problems. The benchmarks export the time to handle the different problems.
- *gobot-sim*: library and executables to use OMPAS as the actor of the Gobot-Sim simulator. The source code of the simulator is available at the following repository [https://github.com/plaans/gobot-sim]().
- *craft-bots*: library and executables to use OMPAS as the actor of the Craft-bots simulator. The source code of the simulator is available at the following repository [https://github.com/strathclyde-artificial-intelligence/craft-bots]().
- *domains*: Collection of domains defined with SOMPAS. For all domains, a set of example problems have been defined in the *problems* sub folders.
- *resources*: Bunch of source code for playground and deprecated libraries. Contains the lis.py and lispy.py files from Peter Norvig.

## Executables
The project contains several executables; all executables are defined in the apps subfolders of the different libraries. Here is a non-exhaustive list grouped per library.
To run a binary, you can use the following command: `cargo run --bin <name> -- <args>` where `<name>` is the binary you want to execute, and `<args>` a list of arguments
- `scheme`: runs a **repl** for the SOMPAS. It can take the following arguments:
    - `-d, --debug`: output all the steps of the evaluation of an expression.
    - `-l, --log <path>`: change the log directory to `<path>`.
    - `-r, --root`: run a scheme without the standard libraries.
- ompas binaries:
    - `ompas`: runs the actor in simulation given a domain.
        - `-d, --domain <path>`: domain of the platform.
        - `-p, --problem <path>`: optional problem to load.
        - `-l, --log-path <path>`: configure the log directory.
    - `flow_graph`: convert a list of expressions into their flow_graph and chronicle representation. Takes as input a config in the form of a yaml file.
    Here is an example of configuration:
        ```yaml
        input-path: /home/<path>/ompas/acting/flow_graph/examples/
        output-path: /home/<name>/Desktop
        problems:
            - method/m_process_package.lisp
            - atom.lisp
            - expression.lisp
            - nested-expression.lisp
        ```
    Other binaries are defined in the apps submodules, but most of them have been used for debugging during development.
- Gobot-Sim binaries:
    - `ompas-gobot-sim`: runs a repl, in which the interface to gobot-sim has been loaded. The binary takes the following arguments: 
        - `-d, --domain <path>`: optional path to the domain file of the system.
        - `-l, --log-path <path>`: configure the log directory.
    - `ompas-gobot-sim-plan`: runs the actor with a given problem loaded into the engine, and the planning module searches for a plan (optimal or not).
        - `-d, --domain <path>`: optional path to the domain file of the system.
        - `-o, --optimal`: searches for an optimal plan.
    - `ompas-gobot-sim-plan-exec`: same as `ompas-gobot-sim-plan` but also executes the tasks it has been given. Is has the same arguments as `ompas-gobot-sim`.
- Benchmarks: 
    - `bench`: launches a number of benchmarks. Here are the arguments it takes:
        - `-c, --config`: a yaml file containing all necessary information to run benchmarks. Here is an example:
            ```yaml
            type: gobot # type of binary is needs to run
            techniques: # acting strategies
                - fa # first available
                - random # random
                - lrptf # longest remaining processing time first.
                - aries # using the planner
                - aries-opt # using the planner searching for an optimal plan
            view: true
            mail:
                from: xxx@yyy.com
                smtp : smtp.yyy.com
                password: ********
                to: zzz@sss.com
            number: 4 # number of run per problem.
            max-time: 300 # maximum time allowed to finish a problem in seconds.
            problems: # file names of the problems
                - j02.lisp
                - j06.lisp
                - j02_teleport.lisp
            bin_path: <path>/ompas/benchmark/
            log_path: <path>/ompas/benchmark/
            domain_path: <path>/ompas/domains/godot/
            ```
    - `bench-gobot-sim`: launches a benchmark on the `ompas-gobot-sim`.
        - `-d, --domain <path>`: path to the domain directory. 
        - `-p, --problem <path>`: optional path to the problem.
        - `-t, --t <time>`: allocated time in seconds.
        - `-f, --fa`: uses the FA strategy.
        - `-L, --lrptf`: uses the LRPTF strategy.
        - `-r, --random`: uses the random strategy.
        - `-a, --aries`: uses Aries as guidance.
        - `-o, --aries-opt`: uses Aries looking for the optimal plan as guidance.
        - `-v, --view`: activate the windows of the simulator.
        - `-l, --log-path <path>`: configure the log directory.
    - `bench-sim`: launches a benchmark using internal simulation of actions.
        - `-d, --domain <path>`: path to the domain directory. 
        - `-p, --problem <path>`: optional path to the problem.
        - `-t, --t <time>`: allocated time in seconds.
        - `-a, --aries`: uses Aries as guidance.
        - `-o, --aries-opt`: uses Aries looking for the optimal plan as guidance.
        - `-v, --view`: activate the windows of the simulator.
        - `-l, --log-path <path>`: configure the log directory.


## Env variables
- `OMPAS_CHRONICLE_DEBUG={off,on,full}`: print in stdout the steps of the chronicle translation processes.
- `OMPAS_PLAN_OUTPUT=<bool>`: print in stdout the plan formatted for *OMPAS*.
- `OMPAS_DEBUG=<bool>`: print in stdout the debug steps of *OMPAS*.
- `OMPAS_PATH=<path>`: path to the source files of *OMPAS*. Used to automatically find domain and problem files. 
- `OMPAS_LOG=<bool>`: activate the log window.
- `OMPAS_PLAN_ENCODING_OPTIMIZATION=<bool>`: improve the quality of the chronicles generated by *OMPAS*.
- `SOMPAS_DEBUG=<bool>`: print in stdout the evaluation steps of *SOMPAS*.

## Domains
Several domains have been programmed in SOMPAS, and can be found in the *domains* folder. Some of those domains come the previous implementation of RAE. 
- *Grid* : an agent needs to move in a grid from one place to another.
- *Gripper*: An agent needs to move balls from one place to another.
- *Gripper Extended*: An agent needs to move balls from one place to another with some constraints on the allowed displacement between rooms.
- **Gobot-Sim**: Collection of domain file for the Gobot-Sim platform interface with OMPAS.
- *Spring doors* : Similar to Gripper Extended, but robots needs to help each other to pass doors by holding them open if necessary.
- *Chargeable robot*: Robot need to displace balls while monitoring their battery and charge themselves.
- *RAE test*: very simple domain to test the implementation of RAE and OMPAS.


