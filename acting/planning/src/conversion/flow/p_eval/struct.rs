use im::{hashset, HashSet};
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::exec::resource::{ACQUIRE, RELEASE};
use ompas_language::exec::state::WAIT_FOR;
use sompas_language::time::SLEEP;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, symbol};
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct PLValue {
    pub(crate) lvalue: LValue,
    pure: bool,
}

impl PLValue {
    pub fn is_pure(&self) -> bool {
        self.pure
    }

    pub fn get_lvalue(&self) -> LValue {
        self.lvalue.clone()
    }

    pub fn into_pure(lv: LValue) -> PLValue {
        PLValue {
            lvalue: lv,
            pure: true,
        }
    }

    pub fn into_unpure(lv: LValue) -> PLValue {
        PLValue {
            lvalue: lv,
            pure: false,
        }
    }

    pub fn lvalue_as_quote(&self) -> LValue {
        match self.pure {
            false => self.lvalue.clone(),
            true => match self.lvalue {
                LValue::Nil | LValue::True | LValue::Number(_) => self.lvalue.clone(),
                _ => list![LPrimitive::Quote.into(), self.lvalue.clone()],
            },
        }
    }
}

impl Default for PLValue {
    fn default() -> Self {
        Self {
            lvalue: LValue::Nil,
            pure: true,
        }
    }
}

impl Display for PLValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        match self.pure {
            true => write!(f, "{}", self.lvalue),
            false => write!(f, "u:{}", self.lvalue),
        }
    }
}

impl From<PLValue> for LValue {
    fn from(pl: PLValue) -> Self {
        pl.lvalue
    }
}

impl From<&PLValue> for LValue {
    fn from(pl: &PLValue) -> Self {
        pl.clone().into()
    }
}

#[derive(Default, Clone)]
pub struct ParameterTable {
    pub inner: im::HashMap<String, PLValue>,
}

impl ParameterTable {
    pub fn add_param(&mut self, param: String) {
        self.inner.insert(
            param.to_string(),
            PLValue::into_unpure(symbol!(param.into())),
        );
    }
    pub fn add_instantiated(&mut self, param: String, value: LValue) {
        self.inner
            .insert(param.to_string(), PLValue::into_pure(value));
    }

    pub fn try_get_param(&self, param: &str) -> Option<&PLValue> {
        self.inner.get(param)
    }
}

#[derive(Clone)]
pub struct PConfig {
    pub avoid: HashSet<String>,
    pub p_table: ParameterTable,
}

impl Default for PConfig {
    fn default() -> Self {
        Self {
            avoid: hashset![
                EXEC_TASK.to_string(),
                SLEEP.to_string(),
                WAIT_FOR.to_string(),
                ACQUIRE.to_string(),
                RELEASE.to_string()
            ],
            p_table: Default::default(),
        }
    }
}
