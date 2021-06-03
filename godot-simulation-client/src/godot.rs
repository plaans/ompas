use tokio::io::{self, ReadHalf, BufReader, AsyncReadExt, WriteHalf, AsyncWriteExt};
use tokio::net::TcpStream;
use tokio::sync::mpsc::{Sender, Receiver};
use std::net::SocketAddr;
use crate::serde::GodotMessageSerde;
use std::convert::TryInto;
use crate::state::{GodotState, LState};
use std::sync::Arc;
use tokio::sync::Mutex;

pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment

pub async fn task_tcp_connection(
    socket_addr: &SocketAddr,
    receiver: Receiver<String>,
    sender: Sender<String>,
    state: Arc<Mutex<GodotState>>
) {
    let stream = TcpStream::connect(socket_addr).await.unwrap();

    let (rd, wr) = io::split(stream);

    tokio::spawn(async move { async_read_socket(rd, sender, state).await });

    tokio::spawn(async move { async_send_socket(wr, receiver).await });
}

async fn async_send_socket(mut stream: WriteHalf<TcpStream>, mut receiver: Receiver<String>) {
    loop {
        let command = receiver.recv().await.unwrap();
        let size = u32_to_u8_array(command.len() as u32);
        let msg: &[u8] = &[&size[0..4], &command.as_bytes()].concat();
        match stream.write_all(msg).await {
            Ok(_) => {}
            Err(_) => panic!("error sending via socket"),
        }
    }
}

fn u32_to_u8_array(x: u32) -> [u8; 4] {
    let b1: u8 = ((x >> 24) & 0xff) as u8;
    let b2: u8 = ((x >> 16) & 0xff) as u8;
    let b3: u8 = ((x >> 8) & 0xff) as u8;
    let b4: u8 = (x & 0xff) as u8;

    [b4, b3, b2, b1]
}

async fn async_read_socket(stream: ReadHalf<TcpStream>, sender: Sender<String>, state: Arc<Mutex<GodotState>>) {
    let mut buf_reader = BufReader::new(stream);

    let mut buf = [0; BUFFER_SIZE];
    let mut size_buf = [0; 4];

    loop {
        match buf_reader.read_exact(&mut size_buf).await {
            Ok(_) => {}
            Err(_) => panic!("Error while reading buffer"),
        };
        let size = read_size_from_buf(&size_buf);
        match buf_reader.read_exact(&mut buf[0..size]).await {
            Ok(_) => {}
            Err(_) => panic!("Error while reading buffer"),
        };

        let msg = read_msg_from_buf(&buf, size);

        if !msg.is_empty() {
            let message : GodotMessageSerde = serde_json::from_str(&msg).unwrap();
            let temp_state: LState = message.try_into().unwrap();
            state.lock().await.set_state(temp_state);


            /*let sender_temp = sender.clone();
            tokio::spawn(async move {
                sender_temp
                    .send(format!("(update-state {} {})", state_type, lisp))
                    .await
                    .expect("could not send via channel");
            });*/
        }
    }
}

pub fn read_size_from_buf(buf: &[u8]) -> usize {
    let mut size = [0; 4];
    size.clone_from_slice(&buf[0..4]);
    u32::from_le_bytes(size) as usize
}

pub fn read_msg_from_buf(buf: &[u8], size: usize) -> String {
    String::from_utf8_lossy(&buf[0..size]).to_string()
}

