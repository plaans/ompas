use crate::rae::context::rae_env::RAEEnv;
use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::TOKIO_CHANNEL_SIZE;
use ompas_lisp::async_await;
use ompas_lisp::core::{eval, load_module, parse, LEnv};
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::math::CtxMath;
use ompas_lisp::modules::utils::CtxUtils;
use ompas_lisp::structs::{InitLisp, LValue};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod domain;
pub mod mod_rae;
pub mod mod_rae_exec;
pub mod mod_rae_monitor;

/// Initialize the libraries to load inside Scheme env.
/// Takes as argument the execution platform.
pub async fn init_ctx_rae(
    mut platform: Box<dyn RAEInterface>,
    working_directory: Option<PathBuf>,
) -> (CtxRae, CtxRaeMonitor) {
    //println!("in init ctx_rae");

    let mut ctx_rae = CtxRae::default();
    let (sender_job, receiver_job) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let (sender_sync, receiver_sync) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let ctx_rae_monitor = CtxRaeMonitor {
        sender_to_rae: Some(sender_job),
        env: Default::default(),
    };

    let domain = platform.domain().await;

    let mut rae_env = RAEEnv::new(Some(receiver_job), Some(receiver_sync));
    rae_env.actions_progress.sync.sender = Some(sender_sync);

    platform
        .init(rae_env.state.clone(), rae_env.actions_progress.clone())
        .await;

    //Clone all structs that need to be shared to monitor action status, state and agenda.
    let ctx_rae_exec = CtxRaeExec {
        actions_progress: rae_env.actions_progress.clone(),
        state: rae_env.state.clone(),
        platform_interface: platform,
        agenda: rae_env.agenda.clone(),
    };

    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxUtils::default(),
        &mut rae_env.init_lisp,
    );

    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxMath::default(),
        &mut rae_env.init_lisp,
    );

    let mut ctx_io = CtxIo::default();
    if let Some(ref path) = working_directory {
        ctx_io.set_log_output(path.clone().into());
        ctx_rae.log = working_directory;
    }

    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        ctx_rae_exec,
        &mut rae_env.init_lisp,
    );

    load_module(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        ctx_io,
        &mut rae_env.init_lisp,
    );

    for element in rae_env.init_lisp.inner() {
        //println!("Adding {} to rae_env", element);
        let lvalue = match parse(element, &mut rae_env.env, &mut rae_env.ctxs).await {
            Ok(lv) => lv,
            Err(e) => {
                panic!("error: {}", e)
            }
        };

        if lvalue != LValue::Nil {
            match eval(&lvalue, &mut rae_env.env, &mut rae_env.ctxs).await {
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
