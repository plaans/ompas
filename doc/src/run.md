# How to run the system

The system can be used in different manners.
The project provides an API to define custom binaries linked to specific robotic platforms described in [link to how to create binaries].
The actual project provides a simple binary to use OMPAS in simulation, by loading a domain with the description of the commands model.
In simulation, OMPAS executes a command by executing the corresponding Scheme model.
The project also provide an integration with _Gobot-Sim_ a benchmark for acting and planning system, providing job shop scheduling problems. 

## OMPAS in simulation
```bash
# if needed go in the root folder of the project
cd rae
cargo run --bin ompas -- -d <path-to-domain> -p <path-to-problem>
```

## OMPAS and Gobot-sim
```bash
# if needed go in the root folder of the project
cd gobot-sim
cargo run
```

## Basic commands in OMPAS
Running one of those binary will automatically launch a REPL.
Here is a list of basic commands that you can use to interact with the system.
The complete list is available [here](rae/repl.md) :
- `(launch)` launches RAE with and starts the platform.
- `(get-state)` returns a map with all the state of RAE, perceived and internal.
- `(trigger-task task p1 ... pn)` triggers a task with the parameters *(p1...pn)*
- `(get-env <element>)` returns the domain, or a specific element.