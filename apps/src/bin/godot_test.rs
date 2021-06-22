use ompas_godot_simulation_client::tcp::BUFFER_SIZE;
use tokio::io::BufReader;
use tokio::net::TcpStream;

//const MESSAGE_TO_SEND: &str = "ACK";

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("tests of the tcp connection with godot");

    let stream = TcpStream::connect("127.0.0.1:10000").await?;
    println!("receiver successfully bound to godot");

    let mut _buf_reader = BufReader::new(stream);

    //let mut buf = String::new();
    //let mut buf = Vec::new();
    let mut _count: usize = 0;
    let mut _buf = [0; BUFFER_SIZE];
    /*let mut next_type_msg = TypeMessage::Unknown;
    let mut size: usize = 0;
    loop {
        let mut msg = String::new();
        println!("message {} ", count);
        let size_read = buf_reader.read(&mut buf).await?;
        match next_type_msg {
            TypeMessage::Unknown => match size_read {
                0..=3 => {
                    println!("error in the message")
                }
                4 => {
                    println!("it's only the size");
                    next_type_msg = TypeMessage::Content;
                    size = read_size_from_buf(&buf);
                }
                _ => {
                    println!("it's a message with it's size prepended");
                    size = read_size_from_buf(&buf);
                    msg = read_msg_from_buf(&buf[4..], size);
                }
            },
            TypeMessage::Content => {
                msg = read_msg_from_buf(&buf, size);
                next_type_msg = TypeMessage::Unknown;
            }
            _ => {}
        }

        println!("\tsize read: {}", size_read);
        println!("\tmessage_size: {}", size);
        //println!("\tcontent: {}", msg);

        if !msg.is_empty() {
            /*let godot_state: Result<GodotState, _> = serde_json::from_str(&msg);
            if let Ok(gs) = godot_state {
                println!("deserialized:\n {:?}", gs);
                if let Ok(lisp) = gs.transform_data_into_lisp() {
                    println!(">>>>>>lisp: {}", lisp);
                }
            };*/
        }

        //println!("deserialized:\n {}", godot_state);
        count += 1;
        if count == 20 {
            break;
        }
    }*/

    Ok(())
}

/*

use std::net::TcpStream;
use std::io::{BufReader, Read};

fn main() -> std::io::Result<()> {
    println!("Welcome in tests connection with godot classic");
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
