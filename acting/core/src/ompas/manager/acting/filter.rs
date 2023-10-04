use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::state::action_status::ProcessStatus;

#[derive(Copy, Clone, Default, Debug)]
pub struct ProcessFilter {
    pub task_type: Option<ActingProcessKind>,
    pub status: Option<ProcessStatus>,
}
