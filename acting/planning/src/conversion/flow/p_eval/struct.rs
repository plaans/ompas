use im::HashSet;
use sompas_structs::lvalue::LValue;
use sompas_structs::symbol;
use std::collections::HashMap;
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

    pub fn get_lvalue(&self) -> &LValue {
        &self.lvalue
    }

    pub fn into_pure(lv: &LValue) -> PLValue {
        PLValue {
            lvalue: lv.clone(),
            pure: true,
        }
    }

    pub fn into_unpure(lv: &LValue) -> PLValue {
        PLValue {
            lvalue: lv.clone(),
            pure: false,
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
        write!(f, "{}", self.lvalue)
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
    pub inner: HashMap<String, PLValue>,
}

impl ParameterTable {
    pub fn add_param(&mut self, param: String) {
        self.inner.insert(
            param.to_string(),
            PLValue::into_unpure(&symbol!(param.into())),
        );
    }
    pub fn add_instantiated(&mut self, param: String, value: LValue) {
        self.inner
            .insert(param.to_string(), PLValue::into_pure(&value));
    }

    pub fn try_get_param(&self, param: &str) -> Option<&PLValue> {
        self.inner.get(param)
    }
}

#[derive(Default, Clone)]
pub struct PConfig {
    pub avoid: HashSet<String>,
    pub p_table: ParameterTable,
}
