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
use std::cmp::Ordering;
use std::fmt::{Display, Formatter};

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Efficiency {
    Inf,
    Finite(f64),
}

impl Efficiency {
    const MAX_EFFICIENCY: f64 = 10e12;
}

impl Default for Efficiency {
    fn default() -> Self {
        Self::Inf
    }
}

impl Display for Efficiency {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Inf => write!(f, "inf"),
            Self::Finite(e) => write!(f, "{}", e),
        }
    }
}

impl PartialOrd for Efficiency {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Inf, Self::Inf) => Some(Ordering::Equal),
            (Self::Inf, Self::Finite(_)) => Some(Ordering::Greater),
            (Self::Finite(_), Self::Inf) => Some(Ordering::Less),
            (Self::Finite(f1), Self::Finite(f2)) => f1.partial_cmp(f2),
        }
    }
}

impl From<f64> for Efficiency {
    fn from(value: f64) -> Self {
        if value > Self::MAX_EFFICIENCY {
            Self::Inf
        } else {
            Self::Finite(value)
        }
    }
}

#[async_trait]
impl Utility for Efficiency {
    async fn compute<'a>(env: &LEnv, sampled: SampledResult<'a>) -> Self {
        match sampled {
            SampledResult::Failure => Self::failure(),
            SampledResult::Command(s) | SampledResult::Method(s) => {
                let label = (&s[0]).to_string();
                let model = env
                    .get_context::<ModUPOM<Efficiency>>(MOD_UPOM)
                    .unwrap()
                    .models
                    .get(&label)
                    .unwrap()
                    .get(&ModelKind::CostModel)
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
        match (u1, u2) {
            (Self::Finite(f1), Self::Finite(f2)) => Self::Finite((f1 * f2) / (f1 + f2)),
            (Self::Inf, Self::Finite(_)) => *u2,
            (Self::Finite(_), Self::Inf) => *u1,
            (Self::Inf, Self::Inf) => Self::Inf,
        }
    }

    fn f64(&self) -> f64 {
        match self {
            Efficiency::Inf => Self::MAX_EFFICIENCY,
            Efficiency::Finite(f) => *f,
        }
    }

    fn identity() -> Self {
        Self::Inf
    }

    fn failure() -> Self {
        Self::Finite(0.0)
    }
}

impl From<Cost> for Efficiency {
    fn from(c: Cost) -> Self {
        match c {
            Cost::Inf => Self::Finite(0.0),
            Cost::Some(f) => {
                if f == 0.0 {
                    Self::Inf
                } else {
                    Self::Finite(1.0 / f)
                }
            }
        }
    }
}

impl From<Efficiency> for Cost {
    fn from(e: Efficiency) -> Self {
        match e {
            Efficiency::Inf => Cost::Some(0.0),
            Efficiency::Finite(f) => {
                if f < 1.0 / Cost::MAX_COST {
                    Cost::Inf
                } else {
                    Cost::Some(1.0 / f)
                }
            }
        }
    }
}

impl TryFrom<&LValue> for Efficiency {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(Efficiency::Finite(n.into())),
            _ => Err(Default::default()),
        }
    }
}
