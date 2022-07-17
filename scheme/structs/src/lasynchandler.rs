use crate::lfuture::LFuture;
use crate::lruntimeerror::LResult;
use crate::lswitch::InterruptionSender;
use crate::lvalue::LValue;

#[derive(Clone, Debug)]
pub struct LAsyncHandler {
    future: LFuture,
    switch: InterruptionSender,
}

impl LAsyncHandler {
    pub fn new(future: LFuture, switch: InterruptionSender) -> Self {
        Self { future, switch }
    }

    pub async fn interrupt(&mut self) -> LResult {
        todo!()
    }

    pub fn get_future(&self) -> LFuture {
        self.future.clone()
    }
}

impl From<LAsyncHandler> for LValue {
    fn from(lah: LAsyncHandler) -> Self {
        Self::Handler(lah)
    }
}
