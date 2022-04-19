use crate::lenv::LEnv;
use crate::lerror::LResult;
use crate::lvalue::LValue;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

pub type NativeFn = fn(&im::Vector<LValue>, &LEnv) -> LResult;

#[derive(Clone)]
pub struct LFn {
    fun: NativeFn,
    debug: Arc<String>,
}

impl LFn {
    pub fn new(lbd: NativeFn, label: String) -> Self {
        Self {
            fun: lbd,
            debug: Arc::new(label),
        }
    }

    pub fn call(&self, args: &im::Vector<LValue>, env: &LEnv) -> LResult {
        (self.fun)(args, env)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}

impl Debug for LFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug)
    }
}

pub type DynFut<'a> = ::std::pin::Pin<Box<dyn 'a + Send + ::std::future::Future<Output = LResult>>>;
pub type AsyncNativeFn = for<'a> fn(&'a im::Vector<LValue>, &'a LEnv) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncFn {
    fun: AsyncNativeFn,
    debug: Arc<String>,
}

impl LAsyncFn {
    pub fn new(lbd: AsyncNativeFn, label: String) -> Self {
        Self {
            fun: lbd,
            debug: Arc::new(label),
        }
    }
    pub fn call<'a>(&'a self, args: &'a im::Vector<LValue>, env: &'a LEnv) -> DynFut<'a> {
        (self.fun)(args, env)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}

impl Debug for LAsyncFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug)
    }
}
