use ompas_language::exec::c_choice::INF;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::cmp;
use std::fmt::{Display, Formatter};
use std::ops::Add;

#[derive(Copy, Clone, Debug)]
pub enum Cost {
    Inf,
    Some(f64),
}

impl Cost {
    pub const MAX_COST: f64 = 1e13;
}

impl Default for Cost {
    fn default() -> Self {
        Self::Some(0.0)
    }
}

impl Add for Cost {
    type Output = Cost;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (Cost::Some(f1), Cost::Some(f2)) => Cost::Some(f1 + f2),
            _ => Cost::Inf,
        }
    }
}

impl PartialEq<Self> for Cost {
    fn eq(&self, _: &Self) -> bool {
        todo!()
    }
}

impl PartialOrd for Cost {
    fn partial_cmp(&self, other: &Self) -> Option<std::cmp::Ordering> {
        match (self, other) {
            (Self::Inf, Self::Inf) => Some(cmp::Ordering::Equal),
            (Self::Inf, Self::Some(_)) => Some(cmp::Ordering::Greater),
            (Self::Some(_), Self::Inf) => Some(cmp::Ordering::Less),
            (Self::Some(f1), Self::Some(f2)) => {
                if f1 == f2 {
                    Some(cmp::Ordering::Equal)
                } else if f1 < f2 {
                    Some(cmp::Ordering::Less)
                } else {
                    Some(cmp::Ordering::Greater)
                }
            }
        }
    }
}

impl Display for Cost {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self {
            Cost::Inf => write!(f, "{}", INF),
            Cost::Some(u) => write!(f, "{}", u),
        }
    }
}

impl TryFrom<&LValue> for Cost {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(Cost::Some(n.into())),
            LValue::Symbol(s) => {
                if s.as_str() == INF {
                    Ok(Cost::Inf)
                } else {
                    Err(Default::default())
                }
            }
            _ => Err(Default::default()),
        }
    }
}

impl From<Cost> for LValue {
    fn from(c: Cost) -> Self {
        match c {
            Cost::Inf => INF.into(),
            Cost::Some(u) => u.into(),
        }
    }
}
