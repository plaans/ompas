use crate::interface::job::Job;
use tokio::sync::mpsc::{Receiver, Sender};

#[derive(Clone, Debug)]
pub enum OMPASJob {
    Job(Job),
    //Stop,
}

impl From<Job> for OMPASJob {
    fn from(j: Job) -> Self {
        Self::Job(j)
    }
}

pub struct RAECommandStream {
    sender: Sender<OMPASJob>,
    receiver: Receiver<OMPASJob>,
}

impl RAECommandStream {
    pub fn new(sender: Sender<OMPASJob>, receiver: Receiver<OMPASJob>) -> Self {
        Self { sender, receiver }
    }

    pub fn get_sender(&self) -> Sender<OMPASJob> {
        self.sender.clone()
    }

    pub fn get_ref_receiver(&mut self) -> &mut Receiver<OMPASJob> {
        &mut self.receiver
    }
}
