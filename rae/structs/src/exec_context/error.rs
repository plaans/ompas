use sompas_structs::lvalue::LValue;

pub const VALUE_NO_APPLICABLE_METHOD: usize = 0;
pub const VALUE_ACTION_FAILURE: usize = 1;
pub const VALUE_EVALUATION_ERROR: usize = 2;

#[derive(Debug, Copy, Clone)]
pub enum RaeExecError {
    NoApplicableMethod,
    ActionFailure,
    EvaluationError,
    Unknown,
}

impl RaeExecError {
    pub fn i64_as_err(i: i64) -> RaeExecError {
        match i {
            0 => Self::NoApplicableMethod,
            1 => Self::ActionFailure,
            2 => Self::EvaluationError,
            _ => Self::Unknown,
        }
    }
}

impl From<RaeExecError> for LValue {
    fn from(r: RaeExecError) -> Self {
        match r {
            RaeExecError::NoApplicableMethod => {
                LValue::Err(Box::new(VALUE_NO_APPLICABLE_METHOD.into()))
            }
            RaeExecError::ActionFailure => LValue::Err(Box::new(VALUE_ACTION_FAILURE.into())),
            RaeExecError::EvaluationError => LValue::Err(Box::new(VALUE_EVALUATION_ERROR.into())),
            RaeExecError::Unknown => LValue::Err(Box::new(LValue::from(-1))),
        }
    }
}
