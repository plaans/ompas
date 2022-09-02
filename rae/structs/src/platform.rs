use crate::rae_interface::RAEInterface;
use async_trait::async_trait;
use sompas_structs::documentation::Documentation;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalue::LValue::Nil;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::any::Any;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc::Sender;
use tokio::sync::RwLock;

pub struct Platform {
    inner: Arc<RwLock<dyn RAEPlatform>>,
}

impl Clone for Platform {
    fn clone(&self) -> Self {
        Self {
            inner: self.inner.clone(),
        }
    }
}

impl Platform {
    pub fn new(platform: impl RAEPlatform) -> Self {
        Self {
            inner: Arc::new(RwLock::new(platform)),
        }
    }

    pub fn get_ref(&self) -> Arc<RwLock<dyn RAEPlatform>> {
        self.inner.clone()
    }

    /// Initial what needs to be.
    pub async fn init(&self, interface: RAEInterface) {
        self.inner.write().await.init(interface).await;
    }

    ///Launch the platform (such as the simulation in godot) and open communication
    pub async fn launch_platform(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.launch_platform(args).await
    }

    pub async fn stop_platform(&self) {
        self.inner.write().await.stop_platform().await
    }

    pub async fn trigger_event(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.trigger_event(args).await
    }
    /// Start the platform process
    pub async fn start_platform(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.start_platform(args).await
    }

    /// Open communication with the platform
    pub async fn open_com(&self, args: &[LValue]) -> LResult {
        self.inner.write().await.open_com(args).await
    }

    /// Executes a command on the platform
    pub async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult {
        self.inner.read().await.exec_command(args, command_id).await
    }

    pub async fn cancel_command(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.cancel_command(args).await
    }

    /// Returns the RAE domain of the platform.
    pub async fn domain(&self) -> String {
        self.inner.read().await.domain().await
    }

    pub async fn instance(&self, args: &[LValue]) -> LResult {
        self.inner.read().await.instance(args).await
    }
}

#[derive(Default, Clone)]
pub struct Log {
    pub path: PathBuf,
    pub channel: Option<Sender<String>>,
    pub display: bool,
}

/// Trait that a platform needs to implement to be able to be used as execution platform in RAE.
#[async_trait]
pub trait RAEPlatform: Any + Send + Sync {
    /// Initial what needs to be.
    async fn init(&mut self, interface: RAEInterface);

    /// Executes a command on the platform
    async fn exec_command(&self, args: &[LValue], command_id: usize) -> LResult;

    async fn cancel_command(&self, args: &[LValue]) -> LResult;

    ///Launch the platform (such as the simulation in godot) and open communication
    async fn launch_platform(&mut self, args: &[LValue]) -> LResult;

    /// Start the platform process
    async fn start_platform(&mut self, args: &[LValue]) -> LResult;

    /// Open communication with the platform
    async fn open_com(&mut self, args: &[LValue]) -> LResult;

    /// Returns the RAE domain of the platform.
    async fn domain(&self) -> String;

    /// Instance function implementation depending on the platform
    async fn instance(&self, args: &[LValue]) -> LResult;

    async fn trigger_event(&self, args: &[LValue]) -> LResult;

    async fn stop_platform(&self);

    fn context_platform(&self) -> CtxPlatform;
}

#[async_trait]
impl RAEPlatform for () {
    async fn init(&mut self, _: RAEInterface) {}

    async fn exec_command(&self, _args: &[LValue], _: usize) -> LResult {
        Ok(Nil)
    }

    async fn cancel_command(&self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn launch_platform(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn start_platform(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn open_com(&mut self, _: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn domain(&self) -> String {
        "".to_string()
    }

    async fn instance(&self, _args: &[LValue]) -> LResult {
        Ok(Nil)
    }

    async fn trigger_event(&self, _: &[LValue]) -> LResult {
        todo!()
    }

    async fn stop_platform(&self) {
        todo!()
    }

    fn context_platform(&self) -> CtxPlatform {
        todo!()
    }
}

pub struct CtxPlatform {
    module: Module,
}

impl CtxPlatform {
    pub fn new(ctx: impl IntoModule) -> Self {
        Self {
            module: ctx.into_module(),
        }
    }
}

impl IntoModule for CtxPlatform {
    fn into_module(self) -> Module {
        self.module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}
