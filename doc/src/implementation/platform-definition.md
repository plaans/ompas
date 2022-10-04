# Define a Platform in RAE

_RAE_ needs a platform to execute commands and get the new state resulting from the action. To define a Platform, the _trait_ **RAEInterface** needs to be implemented. This _trait_ is composed of the following methods:

* `init(&mut self, state: RAEState, status: ActionsProgress)`: Called during initialisation steps of RAE. In gives _Arc_ references to the platform, so it can use it directly to update state and action status. (Note: This function may change)
* `exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError>` : Executes a command on the platform. It receives the keys of the command via the `args`, and the local `command_id`.
* `cancel_command(&self, args: &[LValue]) -> Result<LValue, LError>` : Cancel the command wich `command_id` is supposed to be wrapped inside `args`. The `command_id` is supposed to be the internal one (In godot, two command\_id exists. One that is used by RAE, and the other one inside Godot Engine. Be careful while using `command_id`. Assure yourself to work with the right ones.) (Note: Not yet used, supposed to work in godot at least )
* `get_state(&self, args: &[LValue]) -> Result<LValue, LError>`: Get the whole state of the platform. In godot implementation, args are used to get only part of the state. You can pass the arguments `static` or `dynamic`. (Note: The field _inner\_world_ is supposed to be used only by RAE)
*   `get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError>`: Get the value of a state variable. The key is contained inside `args`. Here is an example

    ```
    (rae-get-state-variable robot.velocity robot0)
    ;return the velocity of the robot as a float
    ```
* `get_status(&self, args: &[LValue]) -> Result<LValue, LError>`: Return the status list of all the actions if `args` is empty, or a specific status if args contains the id of an action.
* `launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>`: Launch the platform, i.e starts the process and open the communication with the process.
* `start_platform(&self, args: &[LValue]) -> Result<LValue, LError>`: Start the platform. The implementation of this method suppose that it will start the process and it will be fully operationnal to then open a communication with RAE.
* `open_com(&mut self, args: &[LValue]) -> Result<LValue, LError>`: Open the communication with the platform. In the godot implementation of this trait, it opens a tcp connection.
* `get_action_status(&self, action_id: &usize) -> Status`: Get the status of an action.
* `set_status(&self, action_id: usize, status: Status)`: Set the status of an action.
* `domain(&self) -> &'static str`: Provides the a lisp expression that enable RAE to load the domain. We propose two ways.
  * The first one is to declare the domain as an str that will be compile in the binary.
  * The other one is to write the domain in a file, and load it with the command `(read <name-file.lisp>).`
