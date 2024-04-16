use crate::ompas::scheme::exec::refinement::sampling::{SampledResult, Utility};
use async_trait::async_trait;
use sompas_structs::lenv::LEnv;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;

#[derive(Default, Clone, Debug)]
pub struct Robustness(bool);

impl TryFrom<LValue> for Robustness {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(LNumber::Int(0)) => Ok(Self::failure()),
            LValue::Number(LNumber::Int(1)) => Ok(Self::identity()),
            _ => Err(LRuntimeError::default()),
        }
    }
}

#[async_trait]
impl Utility for Robustness {
    async fn compute<'a>(_: &LEnv, sampled_result: SampledResult<'a>) -> Self {
        match sampled_result {
            SampledResult::Failure => Self::failure(),
            SampledResult::Command(_) | SampledResult::Method(_) => Self::identity(),
        }
    }

    fn compose(u1: &Self, u2: &Self) -> Self {
        Self(u1.0 & u2.0)
    }

    fn f64(&self) -> f64 {
        match self.0 {
            true => 1.0,
            false => 0.0,
        }
    }

    fn identity() -> Self {
        Self(true)
    }

    fn failure() -> Self {
        Self(false)
    }
}
