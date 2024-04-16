use chrono::{Datelike, Local, Timelike};
use futures::FutureExt;
use im::HashMap;
use sompas_language::time::*;
use sompas_macros::async_scheme_fn;
use sompas_structs::interrupted;
use sompas_structs::lasynchandler::LAsyncHandle;
use sompas_structs::lenv::LEnv;
use sompas_structs::lfuture::FutureResult;
use sompas_structs::lmodule::LModule;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lswitch::new_interruption_handler;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use std::time::{Duration, Instant};
use tokio::sync::RwLock;

pub struct ModTime {
    start: Arc<RwLock<Instant>>,
}

impl Default for ModTime {
    fn default() -> Self {
        Self::new()
    }
}

impl ModTime {
    pub fn new() -> Self {
        Self {
            start: Arc::new(RwLock::new(Instant::now())),
        }
    }

    pub async fn reset_time(&mut self) {
        *self.start.write().await = Instant::now();
    }

    pub async fn get_instant_as_secs(&self) -> f64 {
        (self.start.read().await.elapsed().as_micros() as f64) / 1_000_000.0
    }

    pub async fn get_micros(&self) -> u128 {
        self.start.read().await.elapsed().as_micros()
    }

    pub async fn get_millis(&self) -> u128 {
        self.start.read().await.elapsed().as_millis()
    }
}

impl From<ModTime> for LModule {
    fn from(m: ModTime) -> Self {
        let mut module = LModule::new(m, MOD_TIME, DOC_MOD_TIME);
        module.add_async_fn(__SLEEP__, __sleep__, DOC___SLEEP__, false);
        module.add_lambda(SLEEP, LAMBDA_SLEEP, DOC_SLEEP);
        module.add_async_fn(TIME, time, DOC_TIME, false);
        module.add_async_fn(MY_TIME, my_time, DOC_MY_TIME, false);

        module
    }
}

#[async_scheme_fn]
pub async fn __sleep__(n: LNumber) -> LAsyncHandle {
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

    LAsyncHandle::new(f, tx)
}

const SECOND: &str = "second";
const MINUTE: &str = "minute";
const HOUR: &str = "hour";
const DAYS: &str = "day";
const MONTH: &str = "month";
const YEAR: &str = "year";

///Return the absolute time in function of the defined timezone.
#[async_scheme_fn]
pub async fn time() -> LResult {
    let time = Local::now();
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
    let ctx = env.get_context::<ModTime>(MOD_TIME)?;
    Ok(ctx.get_instant_as_secs().await)
}
