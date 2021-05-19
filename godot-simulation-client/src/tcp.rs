pub const BUFFER_SIZE: usize = 65_536; //65KB should be enough for the moment
#[derive(PartialEq, Eq, Clone)]
pub enum TypeMessage {
    Unknown,
    Size,
    Content,
}

pub fn read_size_from_buf(buf: &[u8]) -> usize {
    let mut size = [0; 4];
    size.clone_from_slice(&buf[0..4]);
    u32::from_le_bytes(size) as usize
}

pub fn read_msg_from_buf(buf: &[u8], size: usize) -> String {
    String::from_utf8_lossy(&buf[0..size]).to_string()
}
