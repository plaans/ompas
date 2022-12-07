use sompas_language::predicate::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;

#[derive(Default)]
pub struct ModPredicate {}

impl From<ModPredicate> for LModule {
    fn from(t: ModPredicate) -> Self {
        let mut module = LModule::new(t, MOD_PREDICATE, DOC_MOD_PREDICATE);
        module.add_fn(KIND, kind, DOC_KIND, true);
        module.add_fn(IS_NIL, is_nil, DOC_IS_NIL, true);
        module.add_fn(IS_NUMBER, is_number, DOC_IS_NUMBER, true);
        module.add_fn(IS_FLOAT, is_float, DOC_IS_FLOAT, true);
        module.add_fn(IS_INT, is_int, DOC_IS_INT, true);
        module.add_fn(IS_BOOL, is_bool, DOC_IS_BOOL, true);
        module.add_fn(IS_SYMBOL, is_symbol, DOC_IS_SYMBOL, true);
        module.add_fn(IS_STRING, is_string, DOC_IS_STRING, true);
        module.add_fn(IS_FN, is_fn, DOC_IS_FN, true);
        module.add_fn(IS_MUT_FN, is_mut_fn, DOC_IS_MUT_FN, true);
        module.add_fn(IS_ASYNC_FN, is_async_fn, DOC_IS_ASYNC_FN, true);
        module.add_fn(IS_ASYNC_MUT_FN, is_async_mut_fn, DOC_IS_ASYNC_MUT_FN, true);
        module.add_fn(IS_LIST, is_list, DOC_IS_LIST, true);
        module.add_fn(IS_MAP, is_map, DOC_IS_MAP, true);
        module.add_fn(IS_LAMBDA, is_lambda, DOC_IS_LAMBDA, true);
        module.add_fn(IS_PRIMITIVE, is_primitive, DOC_IS_PRIMITIVE, true);
        module.add_fn(IS_HANDLE, is_handle, DOC_IS_HANDLE, true);

        module
    }
}

#[scheme_fn]
pub fn kind(lv: &LValue) -> String {
    lv.get_kind().to_string()
}

/// Returns true if LValue is Nil
#[scheme_fn]
pub fn is_nil(lv: &LValue) -> bool {
    matches!(lv, &LValue::Nil)
}

/// Returns true is LValue is number
#[scheme_fn]
pub fn is_number(lv: &LValue) -> bool {
    matches!(lv, &LValue::Number(_))
}

/// Returns true if LValue is float
#[scheme_fn]
pub fn is_float(lv: &LValue) -> bool {
    matches!(lv, LValue::Number(LNumber::Float(_)))
}

/// Returns true if LValue is integer
#[scheme_fn]
pub fn is_int(lv: &LValue) -> bool {
    matches!(lv, LValue::Number(LNumber::Int(_)))
}

/// Returns true if LValue is boolean
#[scheme_fn]
pub fn is_bool(lv: &LValue) -> bool {
    lv == &LValue::Nil || lv == &LValue::True
}

/// Returns true if LValue is a symbol
#[scheme_fn]
pub fn is_symbol(lv: &LValue) -> bool {
    matches!(lv, &LValue::Symbol(_))
}

/// Returns true if LValue is a string
#[scheme_fn]
pub fn is_string(lv: &LValue) -> bool {
    matches!(lv, &LValue::String(_))
}
/// Returns true if LValue is a list
#[scheme_fn]
pub fn is_list(lv: &LValue) -> bool {
    matches!(lv, &LValue::List(_))
}

/// Returns true if LValue is a lambda
#[scheme_fn]
pub fn is_lambda(lv: &LValue) -> bool {
    matches!(lv, &LValue::Lambda(_))
}

/// Returns true if LValue is a hashmap
#[scheme_fn]
pub fn is_map(lv: &LValue) -> bool {
    matches!(lv, &LValue::Map(_))
}

/// Returns true if LValue is a hashmap
#[scheme_fn]
pub fn is_primitive(lv: &LValue) -> bool {
    matches!(lv, &LValue::Primitive(_))
}

/// Returns true if LValue is a hashmap
#[scheme_fn]
pub fn is_handle(lv: &LValue) -> bool {
    matches!(lv, &LValue::Handle(_))
}

/// Returns true if LValue is a function
#[scheme_fn]
pub fn is_fn(lv: &LValue) -> bool {
    matches!(lv, &LValue::Fn(_)) || matches!(lv, &LValue::Fn(_))
}

/// Returns true if LValue is a function
#[scheme_fn]
pub fn is_mut_fn(lv: &LValue) -> bool {
    matches!(lv, &LValue::Fn(_)) || matches!(lv, &LValue::MutFn(_))
}

/// Returns true if LValue is a function
#[scheme_fn]
pub fn is_async_fn(lv: &LValue) -> bool {
    matches!(lv, &LValue::Fn(_)) || matches!(lv, &LValue::AsyncFn(_))
}

/// Returns true if LValue is a function
#[scheme_fn]
pub fn is_async_mut_fn(lv: &LValue) -> bool {
    matches!(lv, &LValue::Fn(_)) || matches!(lv, &LValue::AsyncMutFn(_))
}

/// Returns true if two LValues are equals.
/// The difference with eq is that it compares all kind of LValue.
#[scheme_fn]
pub fn is_equal(a: &LValue, b: &LValue) -> bool {
    a == b
}

/// Returns true if a list is not empty
#[scheme_fn]
pub fn is_pair(lv: Vec<LValue>) -> bool {
    !lv.is_empty()
}
