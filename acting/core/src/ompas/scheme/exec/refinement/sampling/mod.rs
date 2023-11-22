use async_trait::async_trait;
use sompas_structs::lenv::LEnv;
use sompas_structs::lvalue::LValue;
use std::fmt::Debug;

pub mod c_choice;
pub mod cost;
pub mod efficiency;
pub mod rae_plan;
pub mod robustness;
pub mod upom;

#[async_trait]
pub trait Utility: Send + Sized + 'static + Default + Clone + Sync + Debug {
    async fn compute<'a>(env: &LEnv, result: SampledResult<'a>) -> Self;
    fn compose(u1: &Self, u2: &Self) -> Self;
    fn f64(&self) -> f64;
    fn identity() -> Self;
    fn failure() -> Self;
}

pub enum SampledResult<'a> {
    Failure,
    Command(&'a [LValue]),
    Method(&'a [LValue]),
}
