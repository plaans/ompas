use crate::rae::module::mod_rae_exec::JobId;

pub struct RAEStatus {
    pub task: JobId,
    pub msg: String,
}
