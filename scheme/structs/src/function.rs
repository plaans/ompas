use crate::lenv::LEnv;
use crate::lruntimeerror::LResult;
use crate::lvalue::LValue;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

pub type NativeFn = fn(&LEnv, &[LValue]) -> LResult;

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

    pub fn call(&self, env: &LEnv, args: &[LValue]) -> LResult {
        (self.fun)(env, args)
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
pub type AsyncNativeFn = for<'a> fn(&'a LEnv, &'a [LValue]) -> DynFut<'a>;

pub type NativeMutFn = fn(&mut LEnv, &[LValue]) -> LResult;

#[derive(Clone)]
pub struct LMutFn {
    fun: NativeMutFn,
    debug: Arc<String>,
}

impl LMutFn {
    pub fn new(lbd: NativeMutFn, label: String) -> Self {
        Self {
            fun: lbd,
            debug: Arc::new(label),
        }
    }

    pub fn call(&self, env: &mut LEnv, args: &[LValue]) -> LResult {
        (self.fun)(env, args)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}

impl Debug for LMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug)
    }
}

pub type DynMutFut<'a> =
    ::std::pin::Pin<Box<dyn 'a + Send + ::std::future::Future<Output = LResult>>>;
pub type AsyncNativeMutFn = for<'a> fn(&'a mut LEnv, &'a [LValue]) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncMutFn {
    fun: AsyncNativeMutFn,
    debug: Arc<String>,
}

impl LAsyncMutFn {
    pub fn new(lbd: AsyncNativeMutFn, label: String) -> Self {
        Self {
            fun: lbd,
            debug: Arc::new(label),
        }
    }
    pub fn call<'a>(&'a self, env: &'a mut LEnv, args: &'a [LValue]) -> DynFut<'a> {
        (self.fun)(env, args)
    }

    pub fn get_label(&self) -> &str {
        &self.debug
    }
}

impl Debug for LAsyncMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.debug)
    }
}

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
    pub fn call<'a>(&'a self, env: &'a LEnv, args: &'a [LValue]) -> DynFut<'a> {
        (self.fun)(env, args)
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
