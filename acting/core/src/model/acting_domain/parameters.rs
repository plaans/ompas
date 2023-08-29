use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use async_recursion::async_recursion;
use function_name::named;
use ompas_language::sym_table::TYPE_OBJECT;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};
use sompas_structs::{symbol, wrong_n_args, wrong_type};
use std::fmt::{Display, Formatter};
use std::sync::Arc;

#[derive(Default, Debug, Clone)]
pub struct Parameters {
    inner: Vec<(Arc<Sym>, ParameterType)>,
}

impl Parameters {
    pub fn new(inner: Vec<(Arc<Sym>, ParameterType)>) -> Self {
        Self { inner }
    }

    pub fn inner(&self) -> &Vec<(Arc<Sym>, ParameterType)> {
        &self.inner
    }

    #[named]
    pub async fn try_from_lvalue(lv: &LValue, st: &RefSymTable) -> Result<Self, LRuntimeError> {
        let mut vec: Vec<(Arc<Sym>, ParameterType)> = vec![];

        if let LValue::List(list) = lv {
            for e in list.iter() {
                match e {
                    LValue::List(description) => {
                        if description.len() == 2 {
                            vec.push((
                                Arc::<Sym>::try_from(&description[0])
                                    .map_err(|e| e.chain(function_name!()))?,
                                ParameterType::new(
                                    description[1].clone(),
                                    try_domain_from_lvalue(st, &description[1])
                                        .await
                                        .map_err(|e| e.chain(function_name!()))?,
                                ),
                            ));
                        } else {
                            return Err(wrong_n_args!(function_name!(), description.as_slice(), 2));
                        }
                    }
                    LValue::Symbol(s) => {
                        vec.push((
                            s.clone(),
                            ParameterType::new(
                                symbol!(TYPE_OBJECT.to_string()),
                                st.get_type_id(TYPE_OBJECT).unwrap().into(),
                            ),
                        ));
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
        } else if let LValue::Nil = &lv {
        } else {
            return Err(wrong_type!(function_name!(), lv, KindLValue::List));
        }

        Ok(Self { inner: vec })
    }

    pub fn get_labels(&self) -> Vec<Arc<Sym>> {
        self.inner.iter().map(|tuple| tuple.0.clone()).collect()
    }

    pub fn get_type_domain(&self) -> Vec<Domain> {
        self.inner
            .iter()
            .map(|tuple| tuple.1.domain.clone())
            .collect()
    }

    pub fn get_types_as_lvalue(&self) -> LValue {
        self.inner
            .iter()
            .map(|(_, t)| t.debug.clone())
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
            str.push_str(format!("({} {})", e.0, e.1.debug).as_str());
        }

        str.push(')');

        write!(f, "{}", str)
    }
}

#[derive(Default, Debug, Clone)]
pub struct ParameterType {
    debug: LValue,
    domain: Domain,
}

impl ParameterType {
    pub fn new(debug: LValue, domain: Domain) -> Self {
        Self { debug, domain }
    }

    pub fn get_domain(&self) -> &Domain {
        &self.domain
    }

    pub fn get_debug(&self) -> &LValue {
        &self.debug
    }
}

#[named]
#[async_recursion]
pub async fn try_domain_from_lvalue(
    st: &RefSymTable,
    lv: &LValue,
) -> Result<Domain, LRuntimeError> {
    let domain: Domain = match lv {
        LValue::List(list) => {
            let t = st.get_type_id(list[0].to_string()).unwrap();
            let mut composition = vec![];
            for t in &list[1..] {
                composition.push(try_domain_from_lvalue(st, t).await?)
            }
            Domain::Composed(t, composition)
        }
        LValue::Symbol(t) => {
            let t = match t.to_string().to_ascii_lowercase().as_str() {
                "object" => TYPE_OBJECT,
                _ => t,
            };
            Domain::Simple(st.get_type_id(t).ok_or_else(|| {
                LRuntimeError::new(
                    function_name!(),
                    format!("type {} has not been declared", t),
                )
            })?)
        }
        _ => Default::default(),
    };

    Ok(domain)
}
