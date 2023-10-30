use serde::{Deserialize, Serialize};
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;
pub const STATUS_PENDING: &str = "pending";
pub const STATUS_ACCEPTED: &str = "accepted";
pub const STATUS_REJECTED: &str = "rejected";
pub const STATUS_RUNNING: &str = "running";
pub const STATUS_SUCCESS: &str = "success";
pub const STATUS_FAILURE: &str = "failure";
pub const STATUS_CANCELLED: &str = "cancelled";

#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub enum ProcessStatus {
    Pending,
    Accepted,
    Rejected,
    Running(Option<f64>), //Progress of the action
    Success,              //True the action is a success, false the action is a failure
    Failure,
    Cancelled(bool),
    Planned,
    //True the action has been successfully stopped, false it was a failure to cancel
}

impl ProcessStatus {
    pub fn is_failed(&self) -> bool {
        matches!(self, Self::Failure | Self::Rejected | Self::Cancelled(_))
    }
}

impl Display for ProcessStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ProcessStatus::Accepted => write!(f, "accepted"),
            ProcessStatus::Running(progress) => {
                write!(f, "running")?;
                if let Some(p) = progress {
                    write!(f, "({}%)", (p * 100.0) as i64)
                } else {
                    Ok(())
                }
            }
            ProcessStatus::Success => write!(f, "success"),
            ProcessStatus::Cancelled(r) => write!(f, "cancelled: {}", r),
            ProcessStatus::Rejected => write!(f, "rejected"),
            ProcessStatus::Failure => write!(f, "failure"),
            ProcessStatus::Pending => write!(f, "pending"),
            ProcessStatus::Planned => write!(f, "planned"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ActionStatusSet {
    pub server_id_internal_id: Arc<RwLock<im::HashMap<usize, usize>>>,
    pub status: Arc<RwLock<im::HashMap<usize, ProcessStatus>>>,
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

    pub async fn set_status(&mut self, internal_id: usize, status: ProcessStatus) {
        self.status.write().await.insert(internal_id, status);
    }

    pub async fn set_status_from_server(&mut self, server_id: usize, status: ProcessStatus) {
        let id = *self
            .server_id_internal_id
            .read()
            .await
            .get(&server_id)
            .unwrap();
        self.status.write().await.insert(id, status);
    }

    pub async fn get_status(&self, internal_id: &usize) -> Option<ProcessStatus> {
        self.status.read().await.get(internal_id).cloned()
    }

    pub async fn get_status_from_server(&self, server_id: usize) -> Option<ProcessStatus> {
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

/*impl From<ActionStatus> for TaskStatus {
    fn from(cs: ActionStatus) -> Self {
        match cs {
            ActionStatus::Accepted => TaskStatus::Pending,
            ActionStatus::Rejected => TaskStatus::Failure,
            ActionStatus::Running(_) => TaskStatus::Running,
            ActionStatus::Success => TaskStatus::Success,
            ActionStatus::Failure => TaskStatus::Failure,
            ActionStatus::Cancelled(_) => TaskStatus::Failure,
            ActionStatus::Pending => TaskStatus::Pending,
        }
    }
}*/
