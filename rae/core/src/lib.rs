use crate::ctx_planning::CtxPlanning;
use futures::FutureExt;
use log::{error, info, warn};
use ompas_rae_planning::conversion::convert_domain_to_chronicle_hierarchy;
use ompas_rae_planning::structs::{ConversionCollection, ConversionContext};
use ompas_rae_structs::domain::RAEDomain;
use ompas_rae_structs::options::SelectMode::Planning;
use ompas_rae_structs::options::{RAEOptions, SelectMode};
use ompas_rae_structs::platform::Platform;
use ompas_rae_structs::rae_command::RAECommand;
use ompas_rae_structs::rae_interface::RAEInterface;
use sompas_core::{eval, eval_init};
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::ImportType::WithoutPrefix;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lnumber::*;
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use std::ops::Deref;
use std::time::Instant;
use tokio::sync::mpsc::Receiver;

use crate::error::RaeExecError;

pub type ReactiveTriggerId = usize;

pub mod ctx_planning;
pub mod error;

#[derive(Debug, Clone)]
pub enum TaskType {
    Task,
    Event,
}

#[derive(Default, Debug, Clone)]
pub struct RAEEvent {}

pub const TOKIO_CHANNEL_SIZE: usize = 100;

/// Main RAE Loop:
/// Receives Job to handle in separate tasks.
pub async fn run(
    platform: Option<Platform>,
    domain: RAEDomain,
    interface: RAEInterface,
    mut env: LEnv,
    mut command_rx: Receiver<RAECommand>,
    options: &RAEOptions,
) {
    //Ubuntu::
    /*let lvalue: LValue = match options.get_platform_config() {
        None => vec![RAE_LAUNCH_PLATFORM].into(),
        Some(string) => {
            info!("Platform config: {}", string);
            vec![RAE_LAUNCH_PLATFORM.to_string(), string].into()
        }
    };*/

    match &platform {
        None => info!("No platform defined"),
        Some(platform) => match options.get_platform_config() {
            None => {
                platform
                    .launch_platform(&[])
                    .await
                    .expect("error launching platform");
            }
            Some(config) => {
                info!("Platform config: {}", config);
                platform
                    .launch_platform(&[config.into()])
                    .await
                    .expect("error launching platform");
            }
        },
    };

    /*let result = eval(&lvalue, &mut env, None).await;

    match result {
        Ok(_) => {}
        Err(e) => error!("{}", e),
    }*/

    let mut select_mode = *options.get_select_mode();

    let cc: Option<ConversionCollection> = if matches!(select_mode, Planning(_, _)) {
        let instant = Instant::now();
        match convert_domain_to_chronicle_hierarchy(ConversionContext {
            domain: domain.clone(),
            env: env.clone(),
            state: interface.state.get_snapshot().await,
        }) {
            Ok(r) => {
                info!(
                    "Conversion time: {:.3} ms",
                    instant.elapsed().as_micros() as f64 / 1000.0
                );
                Some(r)
            }
            Err(e) => {
                select_mode = SelectMode::Greedy;
                warn!("Cannot plan with the domain...{}", e);
                None
            }
        }
    } else {
        None
    };

    env.import(
        CtxPlanning::new(cc, domain.clone(), env.clone(), select_mode),
        WithoutPrefix,
    );

    eval_init(&mut env).await;

    let mut killed = interface.killer.read().await.as_ref().unwrap().subscribe();

    let mut killers = vec![];

    loop {
        tokio::select! {
            command = command_rx.recv() => {
                match command {
                    None => break,
                    Some(RAECommand::Job(job)) => {
                        info!("new job received!");

                        let mut new_env = env.clone();

                        let job_lvalue = job.core;
                        info!("new triggered task: {}", job_lvalue);
                        //info!("LValue to be evaluated: {}", job_lvalue);

                        let (tx, rx) = new_interruption_handler();
                        killers.push(tx.clone());

                        let future = (Box::pin(async move {
                            let result = eval(&job_lvalue, &mut new_env, Some(rx)).await;
                            match &result {
                                Ok(lv) => info!(
                                    "result of task {}: {}",
                                    job_lvalue,
                                    match lv {
                                        LValue::Err(e) => {
                                            match e.deref() {
                                                LValue::Number(LNumber::Int(i)) => {
                                                    format!("{:?}", RaeExecError::i64_as_err(*i))
                                                }
                                                lv => lv.to_string(),
                                            }
                                        }
                                        lv => lv.to_string(),
                                    }
                                ),
                                Err(e) => error!("Error in asynchronous task: {}", e),
                            }

                            result
                        }) as FutureResult)
                            .shared();

                        let f2 = future.clone();


                        tokio::spawn(f2);


                        match job.sender
                            .try_send(LAsyncHandler::new(future, tx)) {
                            Ok(_) =>{}
                            Err(e) => error!("{}", e.to_string())
                        }
                    }
                }
            }
            _ = killed.recv() => {
                if let Some(platform) = &platform {
                    platform.stop_platform().await;
                }
                println!("rae killed");

                for k in &mut killers {
                    k.interrupt().await;
                }
            }
        }
        //For each new event or task to be addressed, we search for the best method a create a new refinement stack
        //Note: The whole block could be in an async block
    }
}
