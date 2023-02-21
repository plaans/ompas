use crate::supervisor::action_status::ActionStatus;
use crate::supervisor::inner::ProcessKind;

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ProcessKind>,
    pub status: Option<ActionStatus>,
}
