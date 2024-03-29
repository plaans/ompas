use std::io::Write;
use std::process::{Command, Stdio};
use std::thread;
use std::time::Duration;

fn main() {
    println!("hello, world!");

    let terminal = Command::new("gnome-terminal")
        .arg("echo test")
        .stdin(Stdio::piped())
        .spawn()
        .expect("could not spawn terminal");

    let mut stdin = terminal.stdin.expect("no stdin");
    for _ in 1..5 {
        stdin.write_all("echo test ".as_bytes()).expect("");
        thread::sleep(Duration::from_millis(500));
    }
}
