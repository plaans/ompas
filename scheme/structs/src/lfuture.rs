use crate::lruntimeerror::LResult;
use crate::lvalue::LValue;
use futures::future::Shared;
use futures::Future;
use futures::FutureExt;
use std::pin::Pin;
use std::process::Output;

/// Internal type of future returned by an async
pub type FutureResult = Pin<Box<dyn Send + Future<Output = LResult>>>;

/// Type returned by an async and clonable.
pub type LFuture = Shared<FutureResult>;

/*impl From<LFuture> for LValue {
    fn from(lf: LFuture) -> Self {
        LValue::Future(lf)
    }
}

impl From<FutureResult> for LValue {
    fn from(fr: FutureResult) -> Self {
        LValue::Future(fr.shared())
    }
}*/

#[cfg(test)]
mod test_lfuture {
    use crate::lruntimeerror::LRuntimeError;
    use crate::lvalue::LValue;

    #[tokio::test]
    async fn create_lvalue_future() -> Result<(), LRuntimeError> {
        let args = LValue::Nil;

        let future: LValue = args.into();

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
