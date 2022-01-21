use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError;
use crate::core::structs::lvalue::LValue;
use std::any::Any;
use std::sync::Arc;

pub type NativeFn = fn(&[LValue], &LEnv) -> Result<LValue, anyhow::Error>;

#[derive(Clone, Copy)]
pub struct LFn {
    fun: NativeFn,
    debug: String,
}

impl LFn {
    pub fn new(lbd: NativeFn, label: String) -> Self {
        Self {
            fun: lbd,
            debug: label,
        }
    }

    pub fn call(&self, args: &[LValue], env: &LEnv) -> Result<LValue, anyhow::Error> {
        self.fun(args, env)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}

pub type DynFut<'a> = ::std::pin::Pin<
    Box<dyn 'a + Send + ::std::future::Future<Output = Result<LValue, anyhow::Error>>>,
>;
pub type AsyncNativeFn = for<'a> fn(&'a [LValue], &'a LEnv) -> DynFut<'a>;

#[derive(Clone, Copy)]
pub struct LAsyncFn {
    fun: Arc<AsyncNativeFn>,
    debug: String,
}

impl LAsyncFn {
    pub fn call<'a>(&'a self, args: &'a [LValue], env: &'a LEnv) -> DynFut<'a> {
        self.fun(args, env)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}
