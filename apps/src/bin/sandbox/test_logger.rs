use ompas_utils::log;
use std::time::Duration;

#[tokio::main]
async fn main() {
    println!("TEST RAE LOGGER:");

    /*let mut terminal = Command::new("gnome-terminal")
    .args(&["--", "python3", "utils/src/log/logger.py", "&"])
    .spawn()
    .expect("could not spawn terminal");*/

    tokio::time::sleep(Duration::from_secs(2)).await;
    //terminal.kill();

    for i in 1..5 {
        let str = format!("test {}", i);
        //println!("sending: {}", str);
        log::send(str);
        tokio::time::sleep(Duration::from_secs(1)).await;
    }

    log::send(log::END_MSG.to_string())
}
