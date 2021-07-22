use crate::rae::context::RAEEnv;
use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::TOKIO_CHANNEL_SIZE;
use ompas_lisp::async_await;
use ompas_lisp::core::{eval, load_module, parse, LEnv};
use ompas_lisp::functions::env;
use ompas_lisp::structs::{InitLisp, LValue};
use ompas_modules::math::CtxMath;
use ompas_modules::utils::CtxUtils;
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod domain;
pub mod mod_rae;
pub mod mod_rae_exec;
pub mod mod_rae_monitor;

/// Initialize the libraries to load inside Scheme env.
/// Takes as argument the execution platform.
pub fn init_ctx_rae(mut platform: Box<dyn RAEInterface>) -> (CtxRae, CtxRaeMonitor) {
    //println!("in init ctx_rae");

    let mut ctx_rae = CtxRae::default();
    let (sender_job, receiver_job) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let (sender_sync, receiver_sync) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let ctx_rae_monitor = CtxRaeMonitor {
        sender_to_rae: Some(sender_job),
        env: Default::default(),
    };

    let domain = platform.domain();

    let mut rae_env = RAEEnv::new(Some(receiver_job), Some(receiver_sync));
    rae_env.actions_progress.sync.sender = Some(sender_sync);

    platform.init(rae_env.state.clone(), rae_env.actions_progress.clone());

    let ctx_rae_exec = CtxRaeExec {
        actions_progress: rae_env.actions_progress.clone(),
        state: rae_env.state.clone(),
        platform_interface: platform,
    };

    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        ctx_rae_exec,
        &mut rae_env.init_lisp,
    );
    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxMath::default(),
        &mut rae_env.init_lisp,
    );
    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxUtils::default(),
        &mut rae_env.init_lisp,
    );

    for element in rae_env.init_lisp.inner() {
        let lvalue = match parse(element, &mut rae_env.env, &mut rae_env.ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                panic!("error: {}", e)
            }
        };

        if lvalue != LValue::Nil {
            match eval(&lvalue, &mut rae_env.env, &mut rae_env.ctxs) {
                Ok(_lv) => {}
                Err(e) => {
                    panic!("error: {}", e)
                }
            };
        }
    }
    //We can add other modules if we want

    ctx_rae.env = rae_env;
    ctx_rae.domain = vec![domain].into();
    (ctx_rae, ctx_rae_monitor)
}
