use crate::acting_manager::action_status::ProcessStatus;
use crate::acting_manager::inner::ProcessKind;

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ProcessKind>,
    pub status: Option<ProcessStatus>,
}
