use function_name::named;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;

#[derive(Clone, Debug)]
pub struct Event {
    pub(crate) trigger: Trigger,
    pub(crate) body: LValue,
}
#[derive(Clone, Debug)]
pub enum Trigger {
    New(String),
    Clause(LValue),
}

impl TryFrom<LValue> for Trigger {
    type Error = LRuntimeError;

    #[named]
    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        if let LValue::List(list) = &value {
            if list.len() == 2 {
                let command: String = (&list[0]).try_into()?;
                match command.as_str() {
                    "new-instance" => {
                        if let LValue::Symbol(s) = &list[1] {
                            Ok(Self::New(s.to_string()))
                        } else {
                            Err(LRuntimeError::wrong_type(
                                function_name!(),
                                &list[1],
                                KindLValue::Symbol,
                            ))
                        }
                    }
                    _ => Err(LRuntimeError::new(function_name!(), "")),
                }
            } else {
                Ok(Self::Clause(value))
            }
        } else {
            Err(LRuntimeError::wrong_type(
                function_name!(),
                &value,
                KindLValue::List,
            ))
        }
    }
}

impl Event {
    pub fn new(trigger: Trigger, body: LValue) -> Self {
        Self { trigger, body }
    }
}
