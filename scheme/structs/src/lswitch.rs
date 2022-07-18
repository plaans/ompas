use tokio::sync::mpsc;

const TOKIO_CHANNEL_SIZE: usize = 10;

#[derive(Eq, PartialEq)]
pub enum InterruptSignal {
    Interrupted,
    NInterrupted,
}

#[derive(Clone, Debug)]
pub struct InterruptionSender {
    inner: mpsc::Sender<InterruptSignal>,
}

impl InterruptionSender {
    pub fn new(tx: mpsc::Sender<InterruptSignal>) -> Self {
        Self { inner: tx }
    }

    pub async fn interrupt(&mut self) {
        self.inner.send(InterruptSignal::Interrupted).await;
    }
}

pub struct InterruptionReceiver {
    inner: mpsc::Receiver<InterruptSignal>,
}

impl InterruptionReceiver {
    pub fn new(rx: mpsc::Receiver<InterruptSignal>) -> Self {
        Self { inner: rx }
    }

    pub fn is_interrupted(&mut self) -> InterruptSignal {
        match self.inner.try_recv() {
            Ok(i) => i,
            Err(mpsc::error::TryRecvError::Empty) => InterruptSignal::NInterrupted,
            Err(mpsc::error::TryRecvError::Disconnected) => {
                panic!("parent evaluation task has been killed")
            }
        }
    }
}

pub fn new_interruption_handler() -> (InterruptionSender, InterruptionReceiver) {
    let (tx, rx) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let tx = InterruptionSender::new(tx);
    let rx = InterruptionReceiver::new(rx);
    (tx, rx)
}
