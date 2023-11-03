use std::time::Duration;
use tokio::process::Command;

#[tokio::main]
pub async fn main() {
    println!("Hello, I test commands");

    // let mut command = Command::new("timeout");
    // command.arg(format!("40s").as_str());
    // command.arg("ompas");
    //
    let mut command = Command::new("ompas");
    command.args(["-d","/home/jeremy/ompas_output/benchmark/gripper/problems/gripper_medium_0_random_satisfactory.lisp"]);
    command.env("OMPAS_WORKING_DIR", "/home/jeremy/ompas_output/benchmark/gripper");

    if let Ok(mut c) = command.spawn() {
        tokio::select! {
            _ = tokio::time::sleep(Duration::from_secs(40)) => {
                c.kill().await;
                println!("command killed!")
            }
            r = c.wait() => {
                println!("command ok!")
            }
        }
    } else {
        panic!("panic");
    }

}