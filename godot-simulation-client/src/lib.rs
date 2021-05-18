use std::net::SocketAddrV4;
use ompas_lisp::structs::{Module, GetModule};
use ompas_modules::doc::{Documentation, LHelp};

//modules of the crate
pub mod serde;





pub struct SocketInfo {
    addr: SocketAddrV4,
    port: usize,
}

pub struct CtxGodot {
    pub socket_info: SocketInfo
}

impl GetModule for CtxGodot {
    fn get_module(self) -> Module {
        todo!()
    }
}

impl Documentation for CtxGodot {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}