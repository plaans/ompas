use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::state::action_status::ProcessStatus;

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ProcessKind>,
    pub status: Option<ProcessStatus>,
}
