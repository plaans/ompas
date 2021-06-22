use aries_planning::parsing::sexpr::SExpr;
use ompas_lisp::structs::LError::{WrongType, SpecialError};
use ompas_lisp::structs::{LError, LNumber, LValue, NameTypeLValue, LValueS};
use serde::{Deserialize, Serialize, Serializer, Deserializer};
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::Deref;
use serde::ser::SerializeStruct;
use serde::de::Visitor;
use serde_json::ser::State;
use im::HashMap;
use ompas_acting::rae::state::{LState, StateType, ActionStatus};
use ompas_acting::rae::context::Action;
use ompas_acting::rae::state::ActionStatus::{ActionResponse, ActionPreempt, ActionCancel};
use crate::serde::GodotMessageType::ActionFeedback;

#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GodotMessageType {
    #[serde(rename = "static")]
    StaticState,
    #[serde(rename = "dynamic")]
    DynamicState,
    #[serde(rename = "robot_command")]
    RobotCommand,
    #[serde(rename = "action_response")]
    ActionResponse,
    #[serde(rename = "action_feedback")]
    ActionFeedback,
    #[serde(rename = "action_result")]
    ActionResult,
    #[serde(rename = "action_preempt")]
    ActionPreempt,
    #[serde(rename = "cancel_request")]
    CancelRequest,
    #[serde(rename = "action_cancel")]
    ActionCancel,
}


impl Display for GodotMessageType {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            GodotMessageType::StaticState => write!(f, "static"),
            GodotMessageType::DynamicState => write!(f, "dynamic"),
            GodotMessageType::RobotCommand => write!(f, "robot_command"),
            GodotMessageType::ActionResponse => write!(f, "action_response"),
            GodotMessageType::ActionFeedback => write!(f, "action_feedback"),
            GodotMessageType::ActionResult => write!(f, "action_result"),
            GodotMessageType::ActionPreempt => write!(f, "action_preempt"),
            GodotMessageType::CancelRequest => write!(f, "cancel_request"),
            GodotMessageType::ActionCancel => write!(f, "action_cancel"),
        }
    }
}

impl Serialize for GodotMessageType {
    fn serialize<S>(&self, serializer: S) -> Result<<S as Serializer>::Ok, <S as Serializer>::Error>
    where
        S: Serializer,
    {
        match self {
            GodotMessageType::StaticState => serializer.serialize_str("static"),
            GodotMessageType::DynamicState => serializer.serialize_str("dynamic"),
            GodotMessageType::RobotCommand => serializer.serialize_str("robot_command"),
            GodotMessageType::ActionResponse => serializer.serialize_str("action_response"),
            GodotMessageType::ActionFeedback => serializer.serialize_str("action_feedback"),
            GodotMessageType::ActionResult => serializer.serialize_str("action_result"),
            GodotMessageType::ActionPreempt => serializer.serialize_str("action_preempt"),
            GodotMessageType::CancelRequest => serializer.serialize_str("cancel_request"),
            GodotMessageType::ActionCancel => serializer.serialize_str("action_cancel"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GodotMessageSerde {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: GodotMessageSerdeData,
}
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeRobotCommand {
    pub command_info: LValueS,
    pub temp_id : usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionResponse {
    pub temp_id : usize,
    pub action_id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionFeedback {
    pub action_id : usize,
    pub feedback: f64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionResult {
    pub action_id: usize,
    pub result: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionId {
    pub action_id: usize,
}

pub type SerdeActionPreempt = SerdeActionId;
pub type SerdeCancelRequest = SerdeActionId;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionCancel {
    pub temp_id : usize,
    pub cancelled: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum GodotMessageSerdeData {
    LValue(LValueS),
    RobotCommand(SerdeRobotCommand),
    ActionResponse(SerdeActionResponse),
    ActionFeedback(SerdeActionFeedback),
    ActionResult(SerdeActionResult),
    ActionId(SerdeActionId),
    ActionCancel(SerdeActionCancel),
}

impl TryFrom<GodotMessageSerde> for LState {
    type Error = LError;

    fn try_from(value: GodotMessageSerde) -> Result<Self, Self::Error> {
        let mut state: LState = Default::default();
        match value._type {
            GodotMessageType::StaticState => {
                state.set_type(StateType::Static);
            }
            GodotMessageType::DynamicState => {
                state.set_type(StateType::Dynamic);
            }
            _ => return Err(LError::SpecialError(
                "Was expecting a state".to_string(),
            ))
        }
        if let GodotMessageSerdeData::LValue(lvs) = value.data {
            if let LValueS::List(l) = lvs {
                for e in l {
                    match e {
                        LValueS::List(list) => {
                            state.insert(
                                LValueS::List(list[0..list.len() - 1].to_vec()),
                                list.last().unwrap().clone(),
                            );
                        }
                        _ => panic!("there should be a list"),
                    }
                }
            }
        }

        Ok(state)
    }
}

impl TryFrom<GodotMessageSerde> for (usize, ActionStatus) {
    type Error = LError;

    fn try_from(value: GodotMessageSerde) -> Result<Self, Self::Error> {
        let mut id = 0;
        let mut status = ActionStatus::ActionPreempt;

        match value._type {
            GodotMessageType::ActionResponse => {

                if let GodotMessageSerdeData::ActionResponse(ar) = value.data {
                    id = ar.temp_id;
                    status = ActionResponse(ar.action_id)
                } else {
                    todo!()
                }
            }
            GodotMessageType::ActionFeedback => {

                if let GodotMessageSerdeData::ActionFeedback(af)  = value.data {
                    id = af.action_id;
                    status = ActionStatus::ActionFeedback(af.feedback);
                } else {
                    todo!()
                }
            }
            GodotMessageType::ActionResult => {

                if let GodotMessageSerdeData::ActionResult(ar) = value.data {
                    id = ar.action_id;
                    status = ActionStatus::ActionResult(ar.result);
                } else {
                    todo!()
                }
            }
            GodotMessageType::ActionPreempt => {

                if let GodotMessageSerdeData::ActionId(ai) = value.data {
                    id = ai.action_id;
                    status = ActionPreempt;
                } else {
                    todo!()
                }
            }
            GodotMessageType::ActionCancel => {

                if let GodotMessageSerdeData::ActionCancel(ac) = value.data {
                    id = ac.temp_id;
                    status = ActionCancel(ac.cancelled);
                } else {
                    todo!()
                }
            }
            _ => {todo!()}
        }

        Ok((id,status))
    }
}


impl Display for GodotMessageSerde {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        write!(f, "type: {}\ndata: {:?}", self._type, self.data)
    }
}



#[allow(clippy::result_unit_err)]
pub fn parse_into_lvalue(se: &SExpr) -> Result<LValueS, ()> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValueS::Int(int)),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValueS::Float(float)),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        "true" => {
                            //println!("atom is boolean true");
                            Ok(LValueS::Bool(true))
                        }
                        "false" => {
                            //println!("atom is boolean false");
                            Ok(LValueS::Bool(false))
                        }

                        s => Ok(LValueS::Symbol(s.to_string())),
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let list_iter = list.iter();
            let vec: Vec<LValueS> = list_iter
                .map(|x| parse_into_lvalue(x))
                .collect::<Result<_, _>>()?;
            Ok(LValueS::List(vec))
        }
    }
}

#[cfg(test)]
mod tests {}
