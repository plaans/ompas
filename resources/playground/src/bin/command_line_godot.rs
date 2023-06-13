use ompas_core::ompas_path;
use std::process::Command;
use std::thread;

fn main() {
    println!("hello world");
    let path = format!("{}/ompas-gobot-sim/gobot-sim/simu", ompas_path());
    let join_handle = thread::spawn(move || {
        let output = Command::new("godot3")
            .arg("--path")
            .arg(path.as_str())
            .output()
            .expect("failed to execute process");
        output
    });

    let _result = join_handle.join();
}
