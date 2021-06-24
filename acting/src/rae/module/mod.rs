use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_exec::{RAEInterface, CtxRaeExec};
use ompas_lisp::core::{load_module, LEnv};
use ompas_modules::math::CtxMath;
use crate::rae::context::RAEEnv;
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::TOKIO_CHANNEL_SIZE;
use tokio::sync::mpsc;
use ompas_lisp::structs::InitLisp;

pub mod mod_rae_exec;
pub mod mod_rae_monitor;
pub mod mod_rae;
pub mod domain;

pub fn init_ctx_rae(platform: Box<dyn RAEInterface>) -> (CtxRae, CtxRaeMonitor){
    let mut ctx_rae = CtxRae::default();
    let (sender, receiver) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let mut ctx_rae_monitor = CtxRaeMonitor {
        sender_to_rae: Some(sender),
        env: Default::default()
    };
    let mut ctx_rae_exec = CtxRaeExec {
        receiver : Some(receiver),
        actions_progress: Default::default(),
        agenda: Default::default(),
        state: Default::default(),
        platform_interface: Box::new(())
    };
    let domain = platform.domain();
    ctx_rae_exec.add_platform(platform);

    let mut rae_env = RAEEnv::default();

    let mut rae_init_lisp = vec![
        domain
    ].into();

    load_module(&mut rae_env.env, &mut rae_env.ctxs, ctx_rae_exec,&mut rae_init_lisp);
    load_module(&mut rae_env.env, &mut rae_env.ctxs, CtxMath::default(), &mut rae_init_lisp);
    //We can add other modules if we want

    ctx_rae.env = rae_env;
    ctx_rae.init = rae_init_lisp;
    (ctx_rae, ctx_rae_monitor)
}
