use sompas_macros::scheme_fn;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;

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

/// Returns true if LValue is integer
#[scheme_fn]
pub fn is_integer(lv: &LValue) -> bool {
    matches!(lv, LValue::Number(LNumber::Int(_)))
}

/// Returns true if LValue is float
#[scheme_fn]
pub fn is_float(lv: &LValue) -> bool {
    matches!(lv, LValue::Number(LNumber::Float(_)))
}
/// Returns true if LValue is boolean
#[scheme_fn]
pub fn is_bool(lv: &LValue) -> bool {
    lv == &LValue::Nil || lv == &LValue::True
}
/// Returns true if LValue is a function
#[scheme_fn]
pub fn is_fn(lv: &LValue) -> bool {
    matches!(lv, &LValue::Fn(_)) || matches!(lv, &LValue::AsyncFn(_))
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
