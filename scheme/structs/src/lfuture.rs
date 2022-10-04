use crate::lruntimeerror::LResult;
use futures::future::Shared;
use futures::Future;
use std::pin::Pin;

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
