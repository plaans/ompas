use crate::rae::context::rae_env::RAEEnv;
use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_description::CtxRaeDescription;
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::module::mod_rae_sim::CtxRaeSim;
use crate::rae::TOKIO_CHANNEL_SIZE;
use ompas_lisp::async_await;
use ompas_lisp::core::ImportType::WithoutPrefix;
use ompas_lisp::core::{eval, import, parse, ContextCollection, LEnv};
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::math::CtxMath;
use ompas_lisp::modules::utils::CtxUtils;
use ompas_lisp::structs::{InitLisp, LValue};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod mod_rae;
pub mod mod_rae_description;
pub mod mod_rae_exec;
pub mod mod_rae_monitor;
pub(crate) mod mod_rae_sim;
pub(crate) mod mod_rae_sim_interface;

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
        env: RAEEnv::new(None, None).await,
    };

    let domain = platform.domain().await;

    let mut rae_env = RAEEnv::new(Some(receiver_job), Some(receiver_sync)).await;
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

    import(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxUtils::default(),
        WithoutPrefix,
    )
    .await
    .expect("error loading utils");

    import(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxMath::default(),
        WithoutPrefix,
    )
    .await
    .expect("error loading math");

    let mut ctx_io = CtxIo::default();
    if let Some(ref path) = working_directory {
        ctx_io.set_log_output(path.clone().into());
        ctx_rae.log = working_directory;
    }

    import(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        ctx_rae_exec,
        WithoutPrefix,
    )
    .await
    .expect("error loading rae exec");

    import(
        &mut rae_env.env,
        &mut rae_env.ctxs,
        CtxRaeDescription::default(),
        WithoutPrefix,
    )
    .await
    .expect("error loading rae description");

    import(&mut rae_env.env, &mut rae_env.ctxs, ctx_io, WithoutPrefix)
        .await
        .expect("error loading io");

    ctx_rae.env = rae_env;
    ctx_rae.domain = vec![domain].into();
    (ctx_rae, ctx_rae_monitor)
}

pub(crate) async fn init_simu_env(working_directory: Option<PathBuf>) -> (LEnv, ContextCollection) {
    /*Construction of the environment for simulation.
    This enviroment will contain the following modules:
    - io
    - math
    - utils
    - ctx_rae_simu
     */
    let (mut env, mut ctxs) = LEnv::root().await;
    import(&mut env, &mut ctxs, CtxUtils::default(), WithoutPrefix)
        .await
        .expect("error loading utils");

    import(&mut env, &mut ctxs, CtxMath::default(), WithoutPrefix)
        .await
        .expect("error loading math");

    let mut ctx_io = CtxIo::default();
    if let Some(ref path) = working_directory {
        ctx_io.set_log_output(path.clone().into());
    }

    import(&mut env, &mut ctxs, ctx_io, WithoutPrefix)
        .await
        .expect("error loading io");

    import(&mut env, &mut ctxs, CtxRaeSim::default(), WithoutPrefix)
        .await
        .expect("error loading raesim");

    (env, ctxs)
}
