use ompas_rae_language::*;
use sompas_language::kind::LIST;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Debug, Clone)]
pub enum Type {
    Single(Arc<Sym>),
    List(Box<Type>),
    Tuple(Vec<Type>),
}

impl Type {
    pub fn try_as_single(&self) -> Option<&String> {
        if let Self::Single(s) = self {
            Some(s.as_ref())
        } else {
            None
        }
    }
}

impl From<Type> for LValue {
    fn from(t: Type) -> Self {
        match t {
            Type::Single(s) => LValue::Symbol(s),
            Type::List(l) => vec![LValue::from(TYPE_LIST), l.deref().into()].into(),
            Type::Tuple(tuple) => {
                let mut vec: Vec<LValue> = vec![TUPLE_TYPE.into()];
                for t in tuple {
                    vec.push(t.into())
                }
                vec.into()
            }
        }
    }
}

impl From<&Type> for LValue {
    fn from(t: &Type) -> Self {
        t.clone().into()
    }
}

impl From<&str> for Type {
    fn from(str: &str) -> Self {
        Self::Single(Arc::new(str.to_string()))
    }
}

impl Display for Type {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let str = match self {
            Type::Single(s) => s.to_string(),
            Type::List(t) => format!("[{}]", t),
            Type::Tuple(t) => {
                let mut str = "(".to_string();
                for (i, e) in t.iter().enumerate() {
                    if i != 0 {
                        str.push(',');
                    }
                    str.push_str(format!("{}", e).as_str())
                }
                str.push(')');
                str
            }
        };

        write!(f, "{}", str)
    }
}

impl TryFrom<&LValue> for Type {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        value.clone().try_into()
    }
}

impl TryFrom<LValue> for Type {
    type Error = LRuntimeError;

    #[function_name::named]
    fn try_from(lv: LValue) -> Result<Self, Self::Error> {
        let err = lruntimeerror!(
            function_name!(),
            format!("{} or {} was expected", TUPLE_TYPE, LIST)
        );

        match lv {
            LValue::Symbol(s) => {
                let string = if s.as_str() == LIST {
                    TYPE_LIST.to_string()
                } else {
                    s.to_string()
                };
                Ok(Self::Single(Arc::new(string)))
            }
            LValue::List(list) => {
                assert!(list.len() >= 2);
                if let LValue::Symbol(s) = &list[0] {
                    match s.as_str() {
                        TUPLE_TYPE => Ok(Self::Tuple({
                            let mut vec: Vec<Type> = Vec::with_capacity(list.len() - 1);
                            for e in &list[1..] {
                                vec.push(e.try_into()?)
                            }
                            vec
                        })),
                        LIST => {
                            assert_eq!(list.len(), 2);
                            Ok(Self::List(Box::new(list[1].borrow().try_into()?)))
                        }
                        _ => Err(err),
                    }
                } else {
                    Err(err)
                }
            }
            _ => Err(LRuntimeError::not_in_list_of_expected_types(
                function_name!(),
                &lv,
                vec![KindLValue::Symbol, KindLValue::List],
            )),
        }
    }
}
