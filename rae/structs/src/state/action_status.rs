use crate::state::task_status::TaskStatus;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone, Copy)]
pub enum CommandStatus {
    Pending,
    Accepted,
    Rejected,
    Progress(f64), //Progress of the action
    Success,       //True the action is a success, false the action is a failure
    Failure,
    Cancelled(bool), //True the action has been successfully stopped, false it was a failure to cancel
}

impl Display for CommandStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            CommandStatus::Accepted => write!(f, "accepted"),
            CommandStatus::Progress(fl) => write!(f, "progress: {}", fl),
            CommandStatus::Success => write!(f, "success"),
            CommandStatus::Cancelled(r) => write!(f, "cancelled: {}", r),
            CommandStatus::Rejected => write!(f, "rejected"),
            CommandStatus::Failure => write!(f, "failure"),
            CommandStatus::Pending => write!(f, "pending"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ActionStatusSet {
    pub server_id_internal_id: Arc<RwLock<im::HashMap<usize, usize>>>,
    pub status: Arc<RwLock<im::HashMap<usize, CommandStatus>>>,
    next_id: Arc<AtomicUsize>,
}

impl ActionStatusSet {
    pub fn get_new_id(&self) -> usize {
        loop {
            let id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed) //Equivalent to compare_and_swap
                .is_ok()
            {
                return id;
            }
        }
    }

    pub async fn set_status(&mut self, internal_id: usize, status: CommandStatus) {
        self.status.write().await.insert(internal_id, status);
    }

    pub async fn set_status_from_server(&mut self, server_id: usize, status: CommandStatus) {
        let id = *self
            .server_id_internal_id
            .read()
            .await
            .get(&server_id)
            .unwrap();
        self.status.write().await.insert(id, status);
    }

    pub async fn get_status(&self, internal_id: &usize) -> Option<CommandStatus> {
        self.status.read().await.get(internal_id).cloned()
    }

    pub async fn get_status_from_server(&self, server_id: usize) -> Option<CommandStatus> {
        match self.server_id_internal_id.read().await.get(&server_id) {
            None => None,
            Some(id) => self.status.read().await.get(id).cloned(),
        }
    }

    pub async fn pretty_print(&self) -> String {
        let mut str = String::new();
        str.push_str("Action(s) Status:\n");
        let status = self.status.read().await.clone();
        let server_id_internal_id = self.server_id_internal_id.read().await.clone();
        for e in &server_id_internal_id {
            str.push_str(format!("- {}({}): {:?}\n", e.1, e.0, status.get(e.1).unwrap()).as_str());
        }
        str
    }
}

impl From<CommandStatus> for TaskStatus {
    fn from(cs: CommandStatus) -> Self {
        match cs {
            CommandStatus::Accepted => TaskStatus::Pending,
            CommandStatus::Rejected => TaskStatus::Failure,
            CommandStatus::Progress(_) => TaskStatus::Running,
            CommandStatus::Success => TaskStatus::Done,
            CommandStatus::Failure => TaskStatus::Failure,
            CommandStatus::Cancelled(_) => TaskStatus::Failure,
            CommandStatus::Pending => TaskStatus::Pending,
        }
    }
}
