use std::fmt::{Display, Formatter};

pub const STATUS_PENDING: &str = "pending";
pub const STATUS_RUNNING: &str = "running";
pub const STATUS_FAILURE: &str = "failure";
pub const STATUS_DONE: &str = "done";

#[derive(Clone, Copy, Debug, Eq, PartialEq)]
pub enum TaskStatus {
    Pending,
    Running,
    Failure,
    Done,
}

impl Display for TaskStatus {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "{}",
            match self {
                Self::Pending => STATUS_PENDING,
                Self::Running => STATUS_RUNNING,
                Self::Failure => STATUS_FAILURE,
                Self::Done => STATUS_DONE,
            }
        )
    }
}
