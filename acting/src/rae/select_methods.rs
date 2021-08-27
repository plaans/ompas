use ompas_lisp::structs::LError::WrongType;
use ompas_lisp::structs::{LError, LValue, NameTypeLValue};

pub fn sort_greedy(methods: LValue) -> Result<LValue, LError> {
    if let LValue::List(methods) = methods {
        if methods.is_empty() {
            Ok(LValue::Nil)
        } else {
            Ok(methods.into())
        }
    } else if let LValue::Nil = methods {
        Ok(LValue::Nil)
    } else {
        Err(WrongType(
            "select_first_applicable_method",
            methods.clone(),
            methods.into(),
            NameTypeLValue::List,
        ))
    }
}

#[allow(non_snake_case, dead_code)]
pub fn RAEPlan(_: LValue) {}
