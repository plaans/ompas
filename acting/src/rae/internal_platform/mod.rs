/*use crate::rae::context::actions_progress::{ActionsProgress, Status};
use crate::rae::context::rae_state::{ActionStatus, RAEState};
use crate::rae::module::rae_exec::{CtxPlatform, RAEInterface};
use async_trait::async_trait;
use im::HashMap;
use ompas_lisp::core::{ContextCollection, LEnv};
use ompas_lisp::structs::{LError, LResult, LValue};
use tokio::io::AsyncWriteExt;

pub struct InternalPlatform {
    state: RAEState,
    status: ActionsProgress,
    env: LEnv,
    ctxs: ContextCollection,
}

impl InternalPlatform {
    pub fn new() -> Self {
        todo!()
    }
}

#[async_trait]
impl RAEInterface for InternalPlatform {
    async fn init(&mut self, state: RAEState, status: ActionsProgress) {
        self.state = state;
        self.status = status;
    }

    async fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError> {
        self.status
            .status
            .write()
            .await
            .insert(command_id, Status::Pending);

        //Do the action

        self.status
            .status
            .write()
            .await
            .insert(command_id, Status::Done);
        Ok(LValue::Nil)
    }

    async fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError> {
        Ok(LValue::Nil)
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
        Ok("platform launched".into())
    }

    async fn start_platform(&self, args: &[LValue]) -> Result<LValue, LError> {
        Ok("platform started".into())
    }

    async fn open_com(&mut self, args: &[LValue]) -> Result<LValue, LError> {
        Ok("Communication opened with the platform".into())
    }

    async fn get_action_status(&self, action_id: &usize) -> Status {
        todo!()
    }

    async fn set_status(&self, action_id: usize, status: Status) {
        todo!()
    }

    async fn domain(&self) -> &'static str {
        //Platform domain
        "(read instances/internal_platform/init.lisp)"
    }

    async fn instance(&self, args: &[LValue]) -> LResult {
        todo!()
    }

    fn context_platform(&self) -> CtxPlatform {
        todo!()
    }
}*/
