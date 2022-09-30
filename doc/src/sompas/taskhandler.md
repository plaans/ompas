---
description: >-
  Description of the task handler used to end the different asynchronous tasks
  spawned by the executable.
---

# The TaskHandler

## **TaskHandler**

The TaskHandler is a simple way that I found to stop all tokio tasks spawned that are looping during the whole process lifetime. In the REPL, when it receives a _CTRL-C_ entry, it calls `task_handler::end_all()`. Every task can get the end signal by calling `task_handler::subscribe_new_task`. It returns a broadcast receiver. Here is one example of how to use it. We recommand to use `tokio::select!` has it awaits concurently several futures and executes the code of the first received future. In this example it loops and awaits on two futures given by `receiver.recv()` that awaits on messages to send to godot, and `end_receiver.recv()` that awaits on a message from the task\_handler. If the task\_handler broadcast the end message, the task breaks it main loop and end the task process by ending the function.

```rust
let mut end_receiver = task_handler::subscribe_new_task();
loop {
  tokio::select! {
      command = receiver.recv() => {
          let command = match command {
              None => break,
              Some(s) => s
          };
              //println!("new command to send: {}", command);
          let size = u32_to_u8_array(command.len() as u32);
          let msg: &[u8] = &[&size[0..4], &command.as_bytes()].concat();
          match stream.write_all(msg).await {
              Ok(_) => {}
              Err(_) => panic!("error sending via socket"),
          }
      }
      _ = end_receiver.recv() => {
          println!("godot sender task ended");
          break;
      }
  }
}
```
