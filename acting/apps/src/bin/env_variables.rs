use std::env;

const OMPAS_PATH: &str = "OMPAS_PATH";

fn main() {
    //println!("Hello, world!");
    match env::var(OMPAS_PATH) {
        Ok(s) => println!("{OMPAS_PATH} : {}", s),
        Err(e) => println!("{e}"),
    }
}
