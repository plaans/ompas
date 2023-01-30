use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct PLValue {
    pub lvalue: LValue,
    pub pure: bool,
}

impl PLValue {
    pub fn is_pure(&self) -> bool {
        self.pure
    }
}

impl PLValue {
    pub fn get_lvalue(&self) -> &LValue {
        &self.lvalue
    }
}

impl PLValue {
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
