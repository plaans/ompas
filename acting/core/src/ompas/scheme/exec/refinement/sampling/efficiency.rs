use crate::model::acting_domain::model::ModelKind;
use crate::ompas::scheme::exec::refinement::sampling::cost::Cost;
use crate::ompas::scheme::exec::refinement::sampling::upom::ModUPOM;
use crate::ompas::scheme::exec::refinement::sampling::{SampledResult, Utility};
use async_trait::async_trait;
use ompas_language::exec::upom::MOD_UPOM;
use sompas_core::eval;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug)]
pub struct Efficiency(f64);

impl Default for Efficiency {
    fn default() -> Self {
        Self(0.0)
    }
}

impl Display for Efficiency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

impl PartialEq<Self> for Efficiency {
    fn eq(&self, _: &Self) -> bool {
        todo!()
    }
}

impl PartialOrd for Efficiency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        self.0.partial_cmp(&other.0)
    }
}

#[async_trait]
impl Utility for Efficiency {
    async fn compute<'a>(env: &LEnv, sampled: SampledResult<'a>) -> Self {
        match sampled {
            SampledResult::Failure => Self::failure(),
            SampledResult::Success(s) => {
                let label = (&s[0]).to_string();
                let model = env
                    .get_context::<ModUPOM<Efficiency>>(MOD_UPOM)
                    .unwrap()
                    .actions
                    .get(&label)
                    .unwrap()
                    .get_model(&ModelKind::CostModel)
                    .unwrap();
                let mut lv = vec![model];
                for arg in &s[1..] {
                    lv.push(arg.clone())
                }
                let mut env = env.clone();
                let n: f64 = eval(&lv.into(), &mut env, None)
                    .await
                    .unwrap()
                    .try_into()
                    .unwrap();
                let cost = Cost::Some(n);
                cost.into()
            }
        }
    }

    fn compose(u1: &Self, u2: &Self) -> Self {
        Self((u1.0 * u2.0) / (u1.0 + u2.0))
    }

    fn f64(&self) -> f64 {
        self.0
    }

    fn success() -> Self {
        Self(1.0)
    }

    fn failure() -> Self {
        Self(0.0)
    }
}

impl From<Cost> for Efficiency {
    fn from(c: Cost) -> Self {
        match c {
            Cost::Inf => Efficiency(0.0),
            Cost::Some(f) => Efficiency(1.0 / f),
        }
    }
}

impl From<Efficiency> for Cost {
    fn from(e: Efficiency) -> Self {
        if e.0 == 0.0 {
            Cost::Inf
        } else {
            Cost::Some(1.0 / e.0)
        }
    }
}

impl TryFrom<&LValue> for Efficiency {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(Efficiency(n.into())),
            _ => Err(Default::default()),
        }
    }
}
