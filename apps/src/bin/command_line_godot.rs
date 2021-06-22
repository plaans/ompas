use std::process::Command;
use std::thread;

fn main() {
    println!("hello world");
    let join_handle = thread::spawn(|| {
        let output = Command::new("godot3")
            .arg("--path")
            .arg("/home/jeremy/godot/Simulation-Factory-Godot/simu")
            .output()
            .expect("failed to execute process");
        output
    });

    let _result = join_handle.join();
}
