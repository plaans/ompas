use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::WrongType;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;

//Does nothing particular for the moment
pub fn sort_greedy(methods: LValue) -> Result<LValue, LError> {
    if let LValue::List(methods) = methods {
        if methods.is_empty() {
            Ok(LValue::Nil)
        } else {
            //complete to sort by list
            let sorted_list = methods
                .iter()
                .map(|lv| {
                    if let LValue::List(list) = lv {
                        list[0].clone()
                    } else {
                        LValue::Nil
                    }
                })
                .collect::<Vec<LValue>>();
            Ok(sorted_list.into())
        }
    } else if let LValue::Nil = methods {
        Ok(LValue::Nil)
    } else {
        Err(WrongType(
            "select_first_applicable_method",
            methods.clone(),
            methods.into(),
            TypeLValue::List,
        ))
    }
}

#[allow(non_snake_case, dead_code)]
pub fn RAEPlan(_: LValue) {}
