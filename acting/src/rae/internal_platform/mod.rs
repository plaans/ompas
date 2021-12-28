use crate::rae::context::actions_progress::{ActionsProgress, Status};
use crate::rae::context::rae_state::{ActionStatus, RAEState};
use crate::rae::module::mod_rae_exec::{CtxPlatform, RAEInterface};
use ompas_lisp::structs::{LError, LResult, LValue};

pub struct InternalPlatform {
    state: RAEState,
    status: ActionsProgress,
}

#[async_trait]
impl RAEInterface for InternalPlatform {
    async fn init(&mut self, state: RAEState, status: ActionsProgress) {
        todo!()
    }

    async fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError> {
        todo!()
    }

    async fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn get_state(&self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn get_status(&self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    async fn get_action_status(&self, action_id: &usize) -> Status {
        todo!()
    }

    async fn set_status(&self, action_id: usize, status: Status) {
        todo!()
    }

    async fn domain(&self) -> &'static str {
        todo!()
    }

    async fn instance(&self, args: &[LValue]) -> LResult {
        todo!()
    }

    fn context_platform(&self) -> CtxPlatform {
        todo!()
    }
}
