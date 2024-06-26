use tokio::sync::mpsc;
use tokio::sync::mpsc::error::TryRecvError;

pub type InterruptSignal = bool;

#[derive(Clone, Debug)]
pub struct InterruptionSender {
    inner: mpsc::UnboundedSender<InterruptSignal>,
}

impl InterruptionSender {
    pub fn new(tx: mpsc::UnboundedSender<InterruptSignal>) -> Self {
        Self { inner: tx }
    }

    pub fn interrupt(&mut self) {
        let _ = self.inner.send(true);
    }
}

pub struct InterruptionReceiver {
    inner: mpsc::UnboundedReceiver<InterruptSignal>,
    interrupted: Option<bool>,
}

impl InterruptionReceiver {
    pub fn new(rx: mpsc::UnboundedReceiver<InterruptSignal>) -> Self {
        Self {
            inner: rx,
            interrupted: None,
        }
    }

    pub fn is_interrupted(&mut self) -> InterruptSignal {
        if self.interrupted.is_none() {
            match self.inner.try_recv() {
                Ok(i) => match i {
                    true => {
                        self.interrupted = Some(true);
                        true
                    }
                    false => unreachable!(),
                },
                Err(TryRecvError::Empty) => false,
                Err(TryRecvError::Disconnected) => {
                    self.interrupted = Some(false);
                    false
                }
            }
        } else {
            true
        }
    }

    pub async fn recv(&mut self) -> InterruptSignal {
        match self.inner.recv().await {
            Some(i) => match i {
                true => {
                    self.interrupted = Some(true);
                    true
                }
                false => unreachable!(),
            },
            None => {
                self.interrupted = Some(false);
                false
            }
        }
    }
}

pub fn new_interruption_handler() -> (InterruptionSender, InterruptionReceiver) {
    let (tx, rx) = mpsc::unbounded_channel();

    let tx = InterruptionSender::new(tx);
    let rx = InterruptionReceiver::new(rx);
    (tx, rx)
}
