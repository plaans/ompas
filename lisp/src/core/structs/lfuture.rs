use crate::core::structs::lerror::LResult;
use crate::core::structs::lvalue::LValue;
use futures::future::Shared;
use futures::Future;
use futures::FutureExt;
use std::pin::Pin;

/// Internal type of future returned by an async
pub type FutureResult = Pin<Box<dyn Send + Future<Output = LResult>>>;

/// Type returned by an async and clonable.
pub type LFuture = Shared<FutureResult>;

impl From<LFuture> for LValue {
    fn from(lf: LFuture) -> Self {
        LValue::Future(lf)
    }
}

impl From<FutureResult> for LValue {
    fn from(fr: FutureResult) -> Self {
        LValue::Future(fr.shared())
    }
}

#[cfg(test)]
mod test_lfuture {
    use crate::core::eval;
    use crate::core::structs::lenv::LEnv;
    use crate::core::structs::lerror::LError;
    use crate::core::structs::lfuture::FutureResult;
    use crate::core::structs::lvalue::LValue;

    #[tokio::test]
    async fn create_lvalue_future() -> Result<(), LError> {
        let mut env = LEnv::root().await;
        let args = LValue::Nil;

        let future: LValue =
            (Box::pin(async move { eval(&args, &mut env).await }) as FutureResult).into();

        //let future: LValue = future.into();

        let result: LValue = if let LValue::Future(ft) = future {
            ft.await?
        } else {
            LValue::Nil
        };

        println!("LValue: {}", result);

        Ok(())
    }
}
