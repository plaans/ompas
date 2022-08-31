use crate::job::Job;
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Clone, Debug)]
pub enum RAECommand {
    Job(Job),
    //Stop,
}

impl From<Job> for RAECommand {
    fn from(j: Job) -> Self {
        Self::Job(j)
    }
}

pub struct RAECommandStream {
    sender: Sender<RAECommand>,
    receiver: Receiver<RAECommand>,
}

impl RAECommandStream {
    pub fn new(sender: Sender<RAECommand>, receiver: Receiver<RAECommand>) -> Self {
        Self { sender, receiver }
    }

    pub fn get_sender(&self) -> Sender<RAECommand> {
        self.sender.clone()
    }

    pub fn get_ref_receiver(&mut self) -> &mut Receiver<RAECommand> {
        &mut self.receiver
    }
}
