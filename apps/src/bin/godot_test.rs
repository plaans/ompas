use serde_json::from_str;
use tokio::io::{AsyncReadExt, BufReader};
use tokio::net::TcpStream;
use ompas_godot_simulation_client::serde::GodotState;

//const MESSAGE_TO_SEND: &str = "ACK";
const SIZE_BUFFER: usize = 65_536; //65KB should be enough for the moment
#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("test of the tcp connection with godot");

    let stream = TcpStream::connect("127.0.0.1:10000").await?;
    println!("receiver successfully bound to godot");

    let mut buf_reader = BufReader::new(stream);

    //let mut buf = String::new();
    //let mut buf = Vec::new();
    let mut count: usize = 0;
    loop {
        let mut buf = [0; SIZE_BUFFER];
        buf_reader.read(&mut buf).await?;
        let mut size = [0; 4];
        size.clone_from_slice(&buf[0..4]);
        let size = u32::from_le_bytes(size);
        let msg_slice = &buf[4..4 + size as usize];
        let msg = String::from_utf8_lossy(&msg_slice).to_string();
        println!("message_size: {:?}", size);
        println!("string length: {}", msg.len());
        println!("{}: {}", count, msg);
        let godot_state: GodotState = from_str(&msg).expect("error while deserializing");

        println!("deserialized:\n {}", godot_state);
        count += 1;
        if count == 2 {
            break;
        }
    }
    Ok(())
}
/*

use std::net::TcpStream;
use std::io::{BufReader, Read};

fn main() -> std::io::Result<()> {
    println!("Welcome in test connection with godot classic");
    let mut stream = TcpStream::connect("127.0.0.1:10000")?;
    let mut buf_reader = BufReader::new(stream);
    println!("Succesfully connects with godot");
    //stream.write(&[1])?;
    let mut count = 1;
    loop {
        let mut buf = [0; SIZE_BUFFER];
        buf_reader.read(&mut buf);
        let mut size = [0; 4];
        size.clone_from_slice(&buf[0..4]);
        let size = u32::from_le_bytes(size);
        let msg_slice = &buf[4..4 + size as usize];
        let msg = String::from_utf8_lossy(&msg_slice).to_string();
        println!("message_size: {}", size);
        println!("{}: {}", count, msg);
        let godot_state: GodotState = from_str(&msg).expect("error while deserializing");
        println!("deserialized:\n {}", godot_state);
        count += 1;
        if count == 3 {
            break;
        }
    }
    Ok(())
} // the stream is closed here*/
