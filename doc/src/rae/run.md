# How to run RAE

To launch RAE, you need to use the project [https://github.com/Yirmandias/FactBase](https://github.com/Yirmandias/FactBase). Clone the project and run `cargo run`It will automatically build and run the right target. Once the **REPL** is launched, you can use the following commands to monitor, launch rae and trigger tasks.

* `(rae-get-env)`:returns the environment of RAE.
* `(rae-get-state)`: returns the whole state of RAE. Contains only initial facts if rae is not launched yet.
* `(rae-get-status)`: returns the list of status of all actions that have been triggered.
* `(rae-launch)`: launch rae. It will also launch the platform.
* `(rae-trigger-task <label> <params>)`: trigger a task with instantiated parameters. Launch before triggering tasks.
* `(rae-trigger-event)`: trigger an event. Not yet fully defined.
