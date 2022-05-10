use crate::state::task_status::TaskStatus;
use std::fmt::{Display, Formatter};
use std::sync::atomic::{AtomicUsize, Ordering};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Debug, Clone, Copy)]
pub enum ActionStatus {
    ActionPending,
    ActionDenied,
    ActionResponse(usize),
    ActionFeedback(f64), //Progress of the action
    ActionResult(bool),  //True the action is a success, false the action is a failure
    ActionPreempt,
    ActionCancel(bool), //True the action has been successfully stopped, false it was a failure to cancel
}

impl Display for ActionStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            ActionStatus::ActionPending => write!(f, "action pending"),
            ActionStatus::ActionResponse(id) => write!(f, "action response: {}", id),
            ActionStatus::ActionFeedback(fl) => write!(f, "action feedback: {}", fl),
            ActionStatus::ActionResult(r) => write!(f, "action result: {}", r),
            ActionStatus::ActionPreempt => write!(f, "action preempt"),
            ActionStatus::ActionCancel(r) => write!(f, "action cancel {}", r),
            ActionStatus::ActionDenied => write!(f, "action denied"),
        }
    }
}

#[derive(Default, Debug, Clone)]
pub struct ActionStatusSet {
    pub server_id_internal_id: Arc<RwLock<im::HashMap<usize, usize>>>,
    pub status: Arc<RwLock<im::HashMap<usize, ActionStatus>>>,
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

    pub async fn set_status(&mut self, internal_id: usize, status: ActionStatus) {
        self.status.write().await.insert(internal_id, status);
    }

    pub async fn set_status_from_server(&mut self, server_id: usize, status: ActionStatus) {
        let id = *self
            .server_id_internal_id
            .read()
            .await
            .get(&server_id)
            .unwrap();
        self.status.write().await.insert(id, status);
    }

    pub async fn get_status(&self, internal_id: &usize) -> Option<ActionStatus> {
        self.status.read().await.get(internal_id).cloned()
    }

    pub async fn get_status_from_server(&self, server_id: usize) -> Option<ActionStatus> {
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

impl From<ActionStatus> for TaskStatus {
    fn from(_as: ActionStatus) -> Self {
        match _as {
            ActionStatus::ActionPending => TaskStatus::Pending,
            ActionStatus::ActionResponse(_) => TaskStatus::Running,
            ActionStatus::ActionFeedback(_) => TaskStatus::Running,
            ActionStatus::ActionResult(b) => match b {
                true => TaskStatus::Done,
                false => TaskStatus::Failure,
            },
            ActionStatus::ActionPreempt => TaskStatus::Pending,
            ActionStatus::ActionCancel(b) => match b {
                true => TaskStatus::Done,
                false => TaskStatus::Failure,
            },
            ActionStatus::ActionDenied => TaskStatus::Failure,
        }
    }
}
