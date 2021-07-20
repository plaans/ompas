use chrono::{DateTime, Utc};
//use ompas_utils::log;
use std::fs;
use std::fs::OpenOptions;
use std::io::Write;
use std::process::Command;
use std::time::Duration;

#[tokio::main]
async fn main() {
    println!("TEST RAE LOGGER:");

    /*let mut terminal = Command::new("gnome-terminal")
    .args(&["--", "python3", "utils/src/log/logger.py", "&"])
    .spawn()
    .expect("could not spawn terminal");*/

    /*tokio::time::sleep(Duration::from_secs(2)).await;
    //terminal.kill();

    for i in 1..5 {
        let str = format!("test {}", i);
        //println!("sending: {}", str);
        log::send(str);
        tokio::time::sleep(Duration::from_secs(1)).await;
    }

    log::send(log::END_MSG.to_string())*/

    logger_with_file().await;
}

async fn logger_with_file() {
    let date: DateTime<Utc> = Utc::now();
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    fs::create_dir_all("rae_logs").expect("could not create rae logs directory");
    let name_file = format!("rae_logs/rae_{}", string_date);
    let mut file = OpenOptions::new()
        .read(true)
        .write(true)
        .create(true)
        .open(name_file.clone())
        .expect("error creating log file");

    file.write_all("RAE LOG\n\n".as_bytes())
        .expect("could not write to RAE log file.");

    Command::new("gnome-terminal")
        .args(&["--", "tail", "-f", name_file.as_str()])
        .spawn()
        .expect("could not spawn terminal");

    for i in 1..5 {
        let str = format!("[rae] test {}\n", i);
        //println!("sending: {}", str);
        file.write_all(str.as_bytes())
            .expect("could not write to RAE log file.");
        //log::send(str);
        tokio::time::sleep(Duration::from_secs(1)).await;
    }
}
