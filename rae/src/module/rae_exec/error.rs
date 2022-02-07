use ompas_lisp::core::structs::lvalue::LValue;

pub const ERR_NO_APPLICABLE_METHOD: &str = "err::no-applicable-method";
pub const ERR_ACTION_FAILURE: &str = "err::action_failure";

pub const DEFINE_ERR_NO_APPLICABLE_METHOD: &str = "(define err::no-applicable-method 0)";
pub const DEFINE_ERR_ACTION_FAILURE: &str = "(define err::action_failure 1)";

pub const VALUE_NO_APPLICABLE_METHOD: usize = 0;
pub const VALUE_ACTION_FAILURE: usize = 1;

pub enum RaeExecError {
    NoApplicableMethod,
    ActionFailure,
}

impl From<RaeExecError> for LValue {
    fn from(r: RaeExecError) -> Self {
        match r {
            RaeExecError::NoApplicableMethod => {
                LValue::Err(Box::new(VALUE_NO_APPLICABLE_METHOD.into()))
            }
            RaeExecError::ActionFailure => LValue::Err(Box::new(VALUE_ACTION_FAILURE.into())),
        }
    }
}
