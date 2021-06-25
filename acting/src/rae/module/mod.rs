use crate::rae::context::RAEEnv;
use crate::rae::module::mod_rae::CtxRae;
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::module::mod_rae_monitor::CtxRaeMonitor;
use crate::rae::TOKIO_CHANNEL_SIZE;
use ompas_lisp::core::{eval, load_module, parse, LEnv};
use ompas_lisp::functions::env;
use ompas_lisp::structs::{InitLisp, LValue};
use ompas_modules::math::CtxMath;
use std::sync::Arc;
use tokio::sync::mpsc;

pub mod domain;
pub mod mod_rae;
pub mod mod_rae_exec;
pub mod mod_rae_monitor;

pub fn init_ctx_rae(platform: Box<dyn RAEInterface>) -> (CtxRae, CtxRaeMonitor) {
    //println!("in init ctx_rae");

    let mut ctx_rae = CtxRae::default();
    let (sender, receiver) = mpsc::channel(TOKIO_CHANNEL_SIZE);

    let mut ctx_rae_monitor = CtxRaeMonitor {
        sender_to_rae: Some(sender),
        env: Default::default(),
    };

    let domain = platform.domain();

    let mut rae_env = RAEEnv::default();
    rae_env.receiver = Some(receiver);

    let mut ctx_rae_exec = CtxRaeExec {
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

    for element in rae_env.init_lisp.inner() {
        let lvalue = match parse(element, &mut rae_env.env, &mut rae_env.ctxs) {
            Ok(lv) => lv,
            Err(e) => {
                //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                panic!("error: {}", e)
            }
        };
        //stdout.write_all(b"parsing done\n");
        if lvalue != LValue::Nil {
            match eval(&lvalue, &mut rae_env.env, &mut rae_env.ctxs) {
                Ok(lv) => {}
                Err(e) => {
                    //stderr.write_all(format!("ELI>>{}\n", e).as_bytes());
                    panic!("error: {}", e)
                }
            };
        }
    }
    //We can add other modules if we want

    ctx_rae.env = rae_env;
    ctx_rae.domain  = vec![domain].into();
    //println!("end init rae");
    (ctx_rae, ctx_rae_monitor)
}
