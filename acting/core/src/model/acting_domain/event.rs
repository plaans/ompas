use crate::model::acting_domain::parameters::Parameters;
use function_name::named;
use ompas_language::monitor::model::{ONCE, WHENEVER};
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::{Display, Formatter};

#[derive(Clone, Debug)]
pub struct Event {
    pub(crate) label: String,
    pub(crate) parameters: Parameters,
    pub(crate) trigger: Trigger,
    pub(crate) lambda_body: LValue,
}

impl Display for Event {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "
            -parameters: {}\n\
            -trigger: {}\n\
            -body: {}\n",
            self.parameters,
            self.trigger,
            self.lambda_body.format("body: ".len())
        )
    }
}
#[derive(Clone, Debug)]
pub struct Trigger {
    pub(crate) trigger_activation: TriggerActivation,
    pub(crate) pre_conditions: LValue,
}
impl Display for Trigger {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}: {}",
            match self.trigger_activation {
                TriggerActivation::Once => ONCE,
                TriggerActivation::Whenever => WHENEVER,
            },
            self.pre_conditions
        )
    }
}

impl Trigger {
    pub fn new(trigger_activation: TriggerActivation, pre_conditions: LValue) -> Self {
        Self {
            trigger_activation,
            pre_conditions,
        }
    }
}

#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash)]
pub enum TriggerActivation {
    Once,
    Whenever,
}

impl TryFrom<&LValue> for TriggerActivation {
    type Error = LRuntimeError;

    #[named]
    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        if let LValue::Symbol(s) = value {
            match s.as_str() {
                ONCE => Ok(TriggerActivation::Once),
                WHENEVER => Ok(TriggerActivation::Whenever),
                _ => Err(LRuntimeError::new(
                    function_name!(),
                    format!("Expected {} or {}, got {}", ONCE, WHENEVER, s),
                )),
            }
        } else {
            Err(LRuntimeError::wrong_type(
                function_name!(),
                value,
                KindLValue::Atom,
            ))
        }
    }
}

// impl TryFrom<LValue> for Trigger {
//     type Error = LRuntimeError;
//
//     #[named]
//     fn try_from(value: LValue) -> Result<Self, Self::Error> {
//         if let LValue::List(list) = &value {
//             if list.len() == 2 {
//                 let command: String = (&list[0]).try_into()?;
//                 match command.as_str() {
//                     "new-instance" => {
//                         if let LValue::Symbol(s) = &list[1] {
//                             Ok(Self::New(s.to_string()))
//                         } else {
//                             Err(LRuntimeError::wrong_type(
//                                 function_name!(),
//                                 &list[1],
//                                 KindLValue::Symbol,
//                             ))
//                         }
//                     }
//                     _ => Err(LRuntimeError::new(function_name!(), "")),
//                 }
//             } else {
//                 Ok(Self::PreConditions(value))
//             }
//         } else {
//             Err(LRuntimeError::wrong_type(
//                 function_name!(),
//                 &value,
//                 KindLValue::List,
//             ))
//         }
//     }
// }

impl Event {
    pub fn new(label: String, params: Parameters, trigger: Trigger, lambda_body: LValue) -> Self {
        Self {
            label,
            parameters: params,
            trigger,
            lambda_body,
        }
    }
}
