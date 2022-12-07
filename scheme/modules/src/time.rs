use chrono::{Datelike, Timelike, Utc};
use futures::FutureExt;
use im::HashMap;
use sompas_language::time::*;
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::interrupted;
use sompas_structs::lasynchandler::LAsyncHandler;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::sync::atomic::{AtomicI64, Ordering};
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;

pub struct CtxTime {
    start: Arc<RwLock<Instant>>,
    diff: Arc<AtomicI64>,
}

impl CtxTime {
    pub fn new(time_zone: i64) -> Self {
        Self {
            start: Arc::new(RwLock::new(Instant::now())),
            diff: Arc::new(AtomicI64::new(time_zone)),
        }
    }
}

impl CtxTime {
    pub async fn reset_time(&mut self) {
        *self.start.write().await = Instant::now();
    }
}

impl IntoModule for CtxTime {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_SLEEP].into(),
            label: CTX_TIME.to_string(),
        };

        module.add_async_fn_prelude(__SLEEP__, __sleep__);
        module.add_async_fn_prelude(MY_TIME, my_time);
        module.add_async_fn_prelude(TIME, time);

        module
    }

    fn documentation(&self) -> Documentation {
        Documentation::new(
            LHelp::new(CTX_TIME, DOC_CTX_TIME),
            vec![
                LHelp::new(__SLEEP__, DOC___SLEEP__),
                LHelp::new(SLEEP, DOC_SLEEP),
                LHelp::new(TIME, DOC_TIME),
                LHelp::new(MY_TIME, DOC_MY_TIME),
            ],
        )
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

#[async_scheme_fn]
pub async fn __sleep__(n: LNumber) -> LAsyncHandler {
    let (tx, mut rx) = new_interruption_handler();
    let f: FutureResult = Box::pin(async move {
        let duration = Duration::from_micros((f64::from(&n) * 1_000_000.0) as u64);

        //let sleep = tokio::time::sleep(duration).shared();
        //let sleep_2 = sleep.clone();
        tokio::select! {
            _ = rx.recv() => {
                //println!("sleep interrupted");
                Ok(interrupted!())
            }
            _ = tokio::time::sleep(duration) => {
                //println!("sleep terminated");
                Ok(LValue::Nil)
            }
        }
    }) as FutureResult;
    let f = f.shared();

    let f2 = f.clone();
    tokio::spawn(f2);

    LAsyncHandler::new(f, tx)
}

const SECOND: &str = "second";
const MINUTE: &str = "minute";
const HOUR: &str = "hour";
const DAYS: &str = "day";
const MONTH: &str = "month";
const YEAR: &str = "year";

///Return the absolute time in function of the defined timezone.
#[async_scheme_fn]
pub async fn time(env: &LEnv) -> LResult {
    let ctx = env.get_context::<CtxTime>(CTX_TIME)?;
    let diff = ctx.diff.load(Ordering::Relaxed);
    let time = Utc::now() + chrono::Duration::hours(diff);
    let mut map: HashMap<LValue, LValue> = Default::default();
    map.insert(SECOND.into(), time.second().into());
    map.insert(MINUTE.into(), time.minute().into());
    map.insert(HOUR.into(), time.hour().into());
    map.insert(DAYS.into(), time.day().into());
    map.insert(MONTH.into(), time.month().into());
    map.insert(YEAR.into(), time.year().into());

    Ok(map.into())
}

#[async_scheme_fn]
pub async fn my_time(env: &LEnv) -> Result<f64, LRuntimeError> {
    let ctx = env.get_context::<CtxTime>(CTX_TIME)?;
    let t = (ctx.start.read().await.elapsed().as_micros() as f64) / 1_000_000.0;
    Ok(t)
}
