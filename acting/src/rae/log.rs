use crate::rae::job::JobId;

pub struct RAEStatus {
    pub task: JobId,
    pub msg: String,
}
