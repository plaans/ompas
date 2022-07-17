use crate::lfuture::LFuture;
use crate::lruntimeerror::LResult;
use crate::lswitch::InterruptionSender;
use crate::lvalue::LValue;
use futures::future::Shared;
use futures::Future;
use futures::FutureExt;
use std::pin::Pin;

#[derive(Clone, Debug)]
pub struct LAsyncHandler {
    future: LFuture,
    switch: InterruptionSender,
}

impl LAsyncHandler {
    pub fn new(future: LFuture, switch: InterruptionSender) -> Self {
        Self { future, switch }
    }
}

impl From<LAsyncHandler> for LValue {
    fn from(lah: LAsyncHandler) -> Self {
        Self::Handler(lah)
    }
}
