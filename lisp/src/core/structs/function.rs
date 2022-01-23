/*use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::AsyncLTrait;
use macro_rules_attribute::macro_rules_attribute;
use ompas_utils::dyn_async;
use std::any::Any;
use std::fmt::{Debug, Formatter};
use std::sync::Arc;

pub type NativeFn<T> = fn(&[LValue], &LEnv, &T) -> Result<LValue, LError>;
pub type DowncastCall =
    fn(&[LValue], &LEnv, &dyn Any, &Arc<dyn Any + Send + Sync>) -> Result<LValue, LError>;

/// Struct to define a pointer to a function.
/// Contains attributes used to downcast the pointer to the right type.
#[derive(Clone)]
pub struct LFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<DowncastCall>,
    index_mod: Option<usize>,
}

impl LFn {
    /// Constructs a new LFn from a pointer to a function
    pub fn new<T: 'static + Sync + Send>(lbd: NativeFn<T>, debug_label: String) -> Self {
        let downcast_call = |args: &[LValue],
                             env: &LEnv,
                             ctx: &dyn Any,
                             fun: &Arc<dyn Any + Send + Sync>|
         -> Result<LValue, LError> {
            let ctx: &T = ctx.downcast_ref::<T>().ok_or_else(|| {
                LError::SpecialError("LFn::new", "Impossible to downcast context".to_string())
            })?;
            let fun: &NativeFn<T> = fun.downcast_ref::<NativeFn<T>>().ok_or_else(|| {
                LError::SpecialError("LFn::new", "Impossible to downcast function".to_string())
            })?;
            fun(args, env, ctx)
        };
        LFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    ///Calls the function
    pub fn call(&self, args: &[LValue], env: &LEnv, ctx: &dyn Any) -> Result<LValue, LError> {
        (self.downcast)(args, env, ctx, &self.fun)

        //(self.fun)(args, env, ctx)
    }

    /// Set the index of the module that is used by the function.
    /// Mandatory to use the right context when called.
    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    /// Return the index_mod of the function.
    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    /// Returns the label of the function
    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

impl Debug for LFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "label: {:?}\nmod : {}",
            self.debug_label,
            match self.index_mod {
                None => "none".to_string(),
                Some(u) => u.to_string(),
            }
        )
    }
}

pub type NativeMutFn<T> = fn(&[LValue], &LEnv, &mut T) -> Result<LValue, LError>;
pub type DowncastCallMut = fn(
    &[LValue],
    &LEnv,
    &mut dyn Any,
    &Arc<dyn Any + 'static + Send + Sync>,
) -> Result<LValue, LError>;

/// Struct wrapping a pointer to a function that can mutate its context.
#[derive(Clone)]
pub struct LMutFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<DowncastCallMut>,
    index_mod: Option<usize>,
}

impl LMutFn {
    pub fn new<T: 'static>(lbd: NativeMutFn<T>, debug_label: String) -> Self {
        let downcast_call = |args: &[LValue],
                             env: &LEnv,
                             ctx: &mut dyn Any,
                             fun: &Arc<dyn Any + Send + Sync>|
         -> Result<LValue, LError> {
            let ctx: &mut T = ctx.downcast_mut::<T>().ok_or_else(|| {
                LError::SpecialError("LMutFn::new", "Impossible to downcast context".to_string())
            })?;
            let fun: &NativeMutFn<T> = fun.downcast_ref::<NativeMutFn<T>>().ok_or_else(|| {
                LError::SpecialError("LMutFn::new", "Impossible to downcast function".to_string())
            })?;

            fun(args, env, ctx)
        };
        LMutFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call(&self, args: &[LValue], env: &LEnv, ctx: &mut dyn Any) -> Result<LValue, LError> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

impl Debug for LMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(
            f,
            "label: {:?}\nmod : {}",
            self.debug_label,
            match self.index_mod {
                None => "none".to_string(),
                Some(u) => u.to_string(),
            }
        )
    }
}

pub type DynFut<'a> =
    ::std::pin::Pin<Box<dyn 'a + Send + ::std::future::Future<Output = Result<LValue, LError>>>>;
pub type AsyncNativeFn<T> = for<'a> fn(&'a [LValue], &'a LEnv, &'a T) -> DynFut<'a>;
pub type AsyncDowncastCall =
    for<'a> fn(&'a [LValue], &'a LEnv, &'a dyn Any, &'a Arc<dyn Any + Send + Sync>) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<AsyncDowncastCall>,
    index_mod: Option<usize>,
}

impl Debug for LAsyncFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LAsyncFn: {}\nindex_mod: {:?}\n",
            self.debug_label, self.index_mod
        )
    }
}

impl LAsyncFn {
    pub fn new<T: 'static>(lbd: AsyncNativeFn<T>, debug_label: String) -> Self {
        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &T = ctx.downcast_ref::<T>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<T> = fun
                .downcast_ref::<AsyncNativeFn<T>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };
        LAsyncFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call<'a>(
        &'a self,
        args: &'a [LValue],
        env: &'a LEnv,
        ctx: &'a AsyncLTrait,
    ) -> DynFut<'a> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &str {
        self.debug_label.as_str()
    }
}

pub type AsyncNativeMutFn<T> = for<'a> fn(&'a [LValue], &'a LEnv, &'a mut T) -> DynFut<'a>;
pub type AsyncDowncastCallMut = for<'a> fn(
    &'a [LValue],
    &'a LEnv,
    &'a mut dyn Any,
    &'a Arc<dyn Any + Send + Sync>,
) -> DynFut<'a>;

#[derive(Clone)]
pub struct LAsyncMutFn {
    pub(crate) fun: Arc<dyn Any + 'static + Send + Sync>,
    pub(crate) debug_label: String,
    downcast: Arc<AsyncDowncastCallMut>,
    index_mod: Option<usize>,
}

impl Debug for LAsyncMutFn {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "LAsyncFn: {}\nindex_mod: {:?}\n",
            self.debug_label, self.index_mod
        )
    }
}

impl LAsyncMutFn {
    pub fn new<T: 'static>(lbd: AsyncNativeMutFn<T>, debug_label: String) -> Self {
        let downcast_call: AsyncDowncastCallMut = |args: &[LValue],
                                                   env: &LEnv,
                                                   ctx: &mut dyn Any,
                                                   fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &mut T = ctx.downcast_mut::<T>().expect("could not downcast mut ctx");
            let fun: &AsyncNativeMutFn<T> = fun
                .downcast_ref::<AsyncNativeMutFn<T>>()
                .expect("could not downcast asyncNativeMutFn");

            fun(args, env, ctx)
        };
        LAsyncMutFn {
            fun: Arc::new(lbd),
            debug_label,
            downcast: Arc::new(downcast_call),
            index_mod: None,
        }
    }

    pub fn set_index_mod(&mut self, index_mod: usize) {
        self.index_mod = Some(index_mod);
    }

    pub fn get_index_mod(&self) -> Option<usize> {
        self.index_mod
    }

    pub fn call<'a>(
        &'a self,
        args: &'a [LValue],
        env: &'a LEnv,
        ctx: &'a mut AsyncLTrait,
    ) -> DynFut<'a> {
        (self.downcast)(args, env, ctx, &self.fun)
    }

    pub fn get_label(&self) -> &'_ str {
        self.debug_label.as_str()
    }
}

#[cfg(test)]
mod test_async {
    use super::*;
    use crate::core::structs::lenv::LEnv;
    use crate::core::structs::lerror::LError;
    use crate::core::structs::lerror::LError::{WrongNumberOfArgument, WrongType};
    use crate::core::structs::lvalue::LValue;
    use crate::core::structs::typelvalue::TypeLValue;
    use std::any::Any;
    use std::sync::Arc;

    fn fun(i: u32) -> u32 {
        i * i
    }

    #[test]
    fn test_downcast() {
        let val: u32 = 5;

        let arc2: Arc<dyn Any> = Arc::new(val);

        let val_2 = arc2.downcast_ref::<u32>().unwrap();

        assert_eq!(val, *val_2)
    }

    #[test]
    fn test_downcast_fn() {
        let p_fun = fun;

        assert_eq!(p_fun(5), 25);
        let arc: Arc<dyn Any + 'static> = Arc::new(fun as fn(u32) -> u32);

        let p_fun: &fn(u32) -> u32 = arc.downcast_ref::<fn(u32) -> u32>().unwrap();

        assert_eq!(p_fun(5), 25)
    }

    fn test<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> DynFut<'a> {
        Box::pin(async move { Ok(LValue::Nil) })
    }
    #[macro_rules_attribute(dyn_async!)]
    async fn test_2<'a>(_: &'a [LValue], _: &'a LEnv, _: &'a ()) -> Result<LValue, LError> {
        Ok(LValue::Nil)
    }

    #[macro_rules_attribute(dyn_async!)]
    async fn test_computation_square<'a>(
        args: &'a [LValue],
        _: &'a LEnv,
        _: &'a (),
    ) -> Result<LValue, LError> {
        if args.len() != 1 {
            return Err(WrongNumberOfArgument(
                "test_computation_square",
                args.into(),
                args.len(),
                1..1,
            ));
        }

        if let LValue::Number(n) = &args[0] {
            Ok((n * n).into())
        } else {
            Err(WrongType(
                "test_computation_square",
                args[0].clone(),
                (&args[0]).into(),
                TypeLValue::Number,
            ))
        }
    }

    #[macro_rules_attribute(dyn_async!)]
    async fn test_computation_square_with_mut_ctx<'a>(
        args: &'a [LValue],
        _: &'a LEnv,
        _: &'a mut (),
    ) -> Result<LValue, LError> {
        if args.len() != 1 {
            return Err(WrongNumberOfArgument(
                "test_computation_square",
                args.into(),
                args.len(),
                1..1,
            ));
        }

        if let LValue::Number(n) = &args[0] {
            Ok((n * n).into())
        } else {
            Err(WrongType(
                "test_computation_square",
                args[0].clone(),
                (&args[0]).into(),
                TypeLValue::Number,
            ))
        }
    }

    #[tokio::test]
    async fn test_pointer_async() -> Result<(), LError> {
        let p_test = test;
        let result = p_test(&[LValue::Nil], &LEnv::default(), &()).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_arc_downcast_pointer_async() -> Result<(), LError> {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test as AsyncNativeFn<()>);

        let fun: &AsyncNativeFn<()> = fun.downcast_ref::<AsyncNativeFn<()>>().unwrap();

        let env = &LEnv::default();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = fun(args, env, ctx).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_arc_downcast_pointer_async_with_macro_def() -> Result<(), LError> {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test_2 as AsyncNativeFn<()>);

        let fun: &AsyncNativeFn<()> = fun.downcast_ref::<AsyncNativeFn<()>>().unwrap();

        let env = &LEnv::default();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = fun(args, env, ctx).await?;

        assert_eq!(result, LValue::Nil);

        Ok(())
    }

    #[tokio::test]
    async fn test_downcast_call() {
        let fun: Arc<dyn Any + 'static + Send + Sync> = Arc::new(test_2 as AsyncNativeFn<()>);

        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &() = ctx.downcast_ref::<()>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<()> = fun
                .downcast_ref::<AsyncNativeFn<()>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };

        let env = &LEnv::default();
        let args = &[LValue::Nil];
        let ctx = &();

        let result = downcast_call(args, env, ctx, &fun).await;
        if let Ok(result) = result {
            assert_eq!(result, LValue::Nil);
        }
    }

    #[tokio::test]
    async fn test_downcast_call_with_computation() {
        let fun: Arc<dyn Any + 'static + Send + Sync> =
            Arc::new(test_computation_square as AsyncNativeFn<()>);

        let downcast_call: AsyncDowncastCall = |args: &[LValue],
                                                env: &LEnv,
                                                ctx: &dyn Any,
                                                fun: &Arc<dyn Any + Send + Sync>|
         -> DynFut {
            let ctx: &() = ctx.downcast_ref::<()>().expect("could not downcast ctx");
            let fun: &AsyncNativeFn<()> = fun
                .downcast_ref::<AsyncNativeFn<()>>()
                .expect("could not downcast ctx");

            fun(args, env, ctx)
        };

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx = &();

        let result = downcast_call(args, env, ctx, &fun).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }

    #[tokio::test]
    async fn test_l_async_fn() {
        let fun = LAsyncFn::new(
            test_computation_square,
            "test_computation_square".to_string(),
        );

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx: &AsyncLTrait = &();

        let result = fun.call(args, env, ctx).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }

    #[tokio::test]
    async fn test_async_mut_fn() {
        let fun = LAsyncMutFn::new(
            test_computation_square_with_mut_ctx,
            "test_computation_square".to_string(),
        );

        let env = &LEnv::empty();
        let args: &[LValue] = &[5.into()];
        let ctx: &mut AsyncLTrait = &mut ();

        let result = fun.call(args, env, ctx).await;
        if let Ok(result) = result {
            println!("result: {}", result);
            assert_eq!(result, LValue::from(25));
        }
    }
}
*/
