use ompas_core::OMPAS_PATH;
use std::process::Command;
use std::thread;

fn main() {
    println!("hello world");
    let path = format!("{}/ompas-gobot-sim/gobot-sim/simu", OMPAS_PATH.get_ref());
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
