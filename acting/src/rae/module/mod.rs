use crate::rae::context::rae_env::RAEEnv;
use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_description::CtxRaeDescription;
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::module::rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::TOKIO_CHANNEL_SIZE;

use ompas_lisp::core::structs::contextcollection::ContextCollection;
use ompas_lisp::core::structs::lenv::ImportType::{WithPrefix, WithoutPrefix};
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::modules::_type::CtxType;
use ompas_lisp::modules::advanced_math::CtxMath;
use ompas_lisp::modules::io::CtxIo;
use ompas_lisp::modules::utils::CtxUtils;
use std::ops::Deref;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod mod_rae;
pub mod mod_rae_description;
pub mod mod_rae_monitor;
pub mod rae_exec;

/// Initialize the libraries to load inside Scheme env.
/// Takes as argument the execution platform.
pub async fn init_ctx_rae(
    platform: Option<Box<dyn RAEInterface>>,
    working_directory: Option<PathBuf>,
) -> (CtxRae, CtxRaeMonitor) {
    //println!("in init ctx_rae");

    let mut ctx_rae = CtxRae::default();
    let (sender_job, receiver_job) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let ctx_rae_monitor = CtxRaeMonitor {
        sender_to_rae: Some(sender_job),
        env: RAEEnv::new(None, None).await,
    };

    let (mut rae_env, platform) = match platform {
        Some(mut platform) => {
            let (sender_sync, receiver_sync) = mpsc::channel(TOKIO_CHANNEL_SIZE);
            let mut rae_env: RAEEnv = RAEEnv::new(Some(receiver_job), Some(receiver_sync)).await;
            let domain = platform.domain().await;

            ctx_rae.domain = vec![domain].into();

            let context_platform = platform.context_platform();

            rae_env
                .env
                .import(context_platform, WithPrefix)
                .await
                .expect("error loading ctx of the platform");

            rae_env.actions_progress.sync.sender = Some(sender_sync);

            platform
                .init(rae_env.state.clone(), rae_env.actions_progress.clone())
                .await;

            (rae_env, Some(platform))
        }
        None => (RAEEnv::new(Some(receiver_job), None).await, None),
    };

    //Clone all structs that need to be shared to monitor action status, state and agenda.

    let ctx_rae_exec = CtxRaeExec {
        actions_progress: rae_env.actions_progress.clone(),
        state: rae_env.state.clone(),
        platform_interface: platform,
        agenda: rae_env.agenda.clone(),
    };

    rae_env
        .env
        .import(CtxUtils::default(), WithoutPrefix)
        .await
        .expect("error loading utils");

    rae_env
        .env
        .import(CtxMath::default(), WithoutPrefix)
        .await
        .expect("error loading math");

    let mut ctx_io = CtxIo::default();
    if let Some(ref path) = working_directory {
        ctx_io.set_log_output(path.clone().into());
        ctx_rae.log = working_directory;
    }

    rae_env
        .env
        .import(ctx_rae_exec, WithoutPrefix)
        .await
        .expect("error loading rae exec");

    rae_env
        .env
        .import(CtxRaeDescription::default(), WithoutPrefix)
        .await
        .expect("error loading rae description");

    rae_env
        .env
        .import(ctx_io, WithoutPrefix)
        .await
        .expect("error loading io");

    ctx_rae.env = rae_env;
    (ctx_rae, ctx_rae_monitor)
}

#[allow(unused)]
pub(crate) async fn init_simu_env(working_directory: Option<PathBuf>) -> LEnv {
    /*Construction of the environment for simulation.
    This enviroment will contain the following modules:
    - io
    - math
    - utils
    - ctx_rae_simu
     */
    let mut env = LEnv::root().await;
    env.import(CtxUtils::default(), WithoutPrefix)
        .await
        .expect("error loading utils");

    env.import(CtxMath::default(), WithoutPrefix)
        .await
        .expect("error loading math");

    let mut ctx_io = CtxIo::default();
    if let Some(ref path) = working_directory {
        ctx_io.set_log_output(path.clone().into());
    }

    env.import(ctx_io, WithoutPrefix)
        .await
        .expect("error loading io");

    /*import(&mut env, &mut ctxs, CtxRaeSim::default(), WithoutPrefix)
    .await
    .expect("error loading raesim");*/

    env.import(CtxType::default(), WithoutPrefix)
        .await
        .expect("error loading type");

    env
}
