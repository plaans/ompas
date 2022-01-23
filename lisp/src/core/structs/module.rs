use crate::core::structs::documentation::Documentation;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::new_function::*;
use crate::core::structs::purefonction::PureFonctionCollection;
use std::any::Any;
use std::sync::{Arc, RwLock};

#[derive(Debug, Default, Clone)]
pub struct InitLisp(Vec<String>);

impl<T: ToString> From<Vec<T>> for InitLisp {
    fn from(vec: Vec<T>) -> Self {
        InitLisp(vec.iter().map(|x| x.to_string()).collect())
    }
}

impl InitLisp {
    pub fn append(&mut self, other: &mut Self) {
        self.0.append(&mut other.0)
    }

    pub fn inner(&self) -> Vec<&str> {
        self.0.iter().map(|x| x.as_str()).collect()
    }

    pub fn begin_lisp(&self) -> String {
        let mut str = String::new(); //"(begin ".to_string();
        self.0.iter().for_each(|x| {
            str.push_str(x.as_str());
            str.push('\n');
        });
        str
    }
}

pub type AsyncLTrait = dyn Any + Send + Sync;

/// Struct to define a Module, Library that will be loaded inside the Scheme Environment.
pub struct Module {
    pub ctx: Arc<AsyncLTrait>,
    pub prelude: Vec<(String, LValue)>,
    pub raw_lisp: InitLisp,
    pub label: String,
}

impl Module {
    /// Add a function to the module.
    pub fn add_fn_prelude<L: ToString>(&mut self, label: L, fun: NativeFn) {
        self.prelude.push((
            label.to_string(),
            LValue::Fn(LFn::new(fun, label.to_string())),
        ))
    }

    /// Add a mutate function to the module.
    /*pub fn add_mut_fn_prelude<
        T: 'static,
        //R: Into<Result<LValue, LError>>,
        //F: Fn(&[LValue], &LEnv, &mut T) -> R + 'static,
        L: ToString,
    >(
        &mut self,
        label: L,
        fun: NativeMutFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::MutFn(LMutFn::new(fun, label.to_string())),
        ))
    }*/

    pub fn add_async_fn_prelude<L: ToString>(&mut self, label: L, fun: AsyncNativeFn) {
        self.prelude.push((
            label.to_string(),
            LValue::AsyncFn(LAsyncFn::new(fun, label.to_string())),
        ))
    }
    /*pub fn add_async_mut_fn_prelude<
        T: 'static,
        //R: Into<Result<LValue, LError>>,
        //F: Fn(&[LValue], &LEnv, &mut T) -> R + 'static,
        L: ToString,
    >(
        &mut self,
        label: L,
        fun: AsyncNativeMutFn<T>,
    ) {
        self.prelude.push((
            label.to_string(),
            LValue::AsyncMutFn(LAsyncMutFn::new(fun, label.to_string())),
        ))
    }*/

    /// Add a LValue to the prelude.
    pub fn add_prelude(&mut self, label: &str, lv: LValue) {
        self.prelude.push((label.into(), lv));
    }
}

/// Trait that must be implemented by a context to build a Module object
/// that will be loaded into the LEnv and ContextCollection.
pub trait IntoModule {
    fn into_module(self) -> Module;

    fn documentation(&self) -> Documentation;

    fn pure_fonctions(&self) -> PureFonctionCollection;
}

impl IntoModule for () {
    fn into_module(self) -> Module {
        Module {
            ctx: Arc::new(RwLock::new(self)),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: "()".to_string(),
        }
    }

    fn documentation(&self) -> Documentation {
        todo!()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        todo!()
    }
}
