use crate::lfuture::LFuture;
use crate::lruntimeerror::LResult;
use crate::lswitch::InterruptionSender;
use crate::lvalue::LValue;

#[derive(Clone, Debug)]
pub struct LAsyncHandle {
    future: LFuture,
    switch: InterruptionSender,
}

impl LAsyncHandle {
    pub fn new(future: LFuture, switch: InterruptionSender) -> Self {
        Self { future, switch }
    }

    pub async fn interrupt(&mut self) -> LResult {
        self.switch.interrupt().await;
        self.get_future().await
    }

    pub fn get_future(&self) -> LFuture {
        self.future.clone()
    }
}

impl From<LAsyncHandle> for LValue {
    fn from(lah: LAsyncHandle) -> Self {
        Self::Handle(lah)
    }
}

/*impl Drop for LAsyncHandler {
    fn drop(&mut self) {
        let mut sender = self.switch.clone();
        tokio::spawn(async move { sender.interrupt().await });
    }
}*/
