use crate::domain::_type::Type;
use sompas_language::kind::OBJECT;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::{wrong_n_args, wrong_type};
use std::borrow::Borrow;
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct Parameters {
    inner: Vec<(Arc<Sym>, Type)>,
}

impl Parameters {
    pub fn inner(&self) -> &Vec<(Arc<Sym>, Type)> {
        &self.inner
    }
}

impl TryFrom<&LValue> for Parameters {
    type Error = LRuntimeError;

    #[function_name::named]
    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        let mut vec: Vec<(Arc<Sym>, Type)> = vec![];

        if let LValue::List(list) = value {
            for e in list.iter() {
                match e {
                    LValue::List(description) => {
                        if description.len() == 2 {
                            vec.push((
                                Arc::<Sym>::try_from(&description[0])
                                    .map_err(|e| e.chain(function_name!()))?,
                                Type::try_from(&description[1])
                                    .map_err(|e| e.chain(function_name!()))?,
                            ));
                        } else {
                            return Err(wrong_n_args!(function_name!(), description.as_slice(), 2));
                        }
                    }
                    LValue::Symbol(s) => {
                        vec.push((s.clone(), OBJECT.into()));
                    }
                    _ => {
                        return Err(LRuntimeError::not_in_list_of_expected_types(
                            function_name!(),
                            e,
                            vec![KindLValue::List, KindLValue::Symbol],
                        ))
                    }
                }
            }
        } else if let LValue::Nil = &value {
        } else {
            return Err(wrong_type!(function_name!(), value, KindLValue::List));
        }

        Ok(Self { inner: vec })
    }
}

impl TryFrom<LValue> for Parameters {
    type Error = LRuntimeError;

    fn try_from(v: LValue) -> Result<Self, Self::Error> {
        v.borrow().try_into()
    }
}

impl Parameters {
    pub fn get_params(&self) -> Vec<Arc<Sym>> {
        self.inner.iter().map(|tuple| tuple.0.clone()).collect()
    }

    pub fn get_types(&self) -> Vec<Type> {
        self.inner.iter().map(|tuple| tuple.1.clone()).collect()
    }

    pub fn get_types_as_lvalue(&self) -> LValue {
        self.inner
            .iter()
            .map(|(_, t)| t.into())
            .collect::<Vec<LValue>>()
            .into()
    }
    pub fn get_params_as_lvalue(&self) -> LValue {
        self.inner
            .iter()
            .map(|(p, _)| p.into())
            .collect::<Vec<LValue>>()
            .into()
    }

    pub fn get_number(&self) -> usize {
        self.inner.len()
    }
}

impl Display for Parameters {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = '('.to_string();
        for (i, e) in self.inner.iter().enumerate() {
            if i != 0 {
                str.push(' ');
            }
            str.push_str(format!("({} {})", e.0, e.1).as_str());
        }

        str.push(')');

        write!(f, "{}", str)
    }
}
