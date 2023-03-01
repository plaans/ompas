use aries_planning::parsing::sexpr::SExpr;
use ompas_structs::acting_manager::action_status::ProcessStatus;
use ompas_structs::acting_manager::action_status::ProcessStatus::Rejected;
use ompas_structs::state::partial_state::PartialState;
use ompas_structs::state::world_state::StateType;
use serde::{Deserialize, Serialize, Serializer};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::convert::TryFrom;
use std::fmt::{Display, Formatter};

/// Different kinds of messages that can be exchanged with the simulation.
#[derive(Debug, Clone, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum GodotMessageType {
    #[serde(rename = "static")]
    StaticState,
    #[serde(rename = "dynamic")]
    DynamicState,
    #[serde(rename = "robot_command")]
    RobotCommand,
    #[serde(rename = "machine_command")]
    MachineCommand,
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
            GodotMessageType::MachineCommand => write!(f, "machine_command"),
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
            GodotMessageType::MachineCommand => serializer.serialize_str("machine_command"),
        }
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GodotMessageSerde {
    #[serde(rename = "type")]
    pub _type: GodotMessageType,
    pub data: GodotMessageSerdeData,
}

impl GodotMessageSerde {
    pub fn new_robot_command(_args: &[LValue], _command_id: usize) {}
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeCommand {
    pub command_info: LValueS,
    pub temp_id: usize,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionResponse {
    pub temp_id: usize,
    pub action_id: i64,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SerdeActionFeedback {
    pub action_id: usize,
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
    pub action_id: usize,
    pub cancelled: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(untagged)]
pub enum GodotMessageSerdeData {
    LValue(LValueS),
    RobotCommand(SerdeCommand),
    ActionResponse(SerdeActionResponse),
    ActionFeedback(SerdeActionFeedback),
    ActionResult(SerdeActionResult),
    ActionId(SerdeActionId),
    ActionCancel(SerdeActionCancel),
}

impl TryFrom<GodotMessageSerde> for PartialState {
    type Error = LRuntimeError;

    fn try_from(value: GodotMessageSerde) -> Result<Self, Self::Error> {
        let mut state: PartialState = Default::default();
        match value._type {
            GodotMessageType::StaticState => {
                state.set_type(StateType::Static);
            }
            GodotMessageType::DynamicState => {
                state.set_type(StateType::Dynamic);
            }
            _ => {
                return Err(lruntimeerror!(
                    "LState::TryFrom<GodotMessageSerde>",
                    "Was expecting a state"
                ))
            }
        }
        if let GodotMessageSerdeData::LValue(LValueS::List(l)) = value.data {
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

        Ok(state)
    }
}

impl TryFrom<GodotMessageSerde> for (usize, ProcessStatus) {
    type Error = LRuntimeError;

    fn try_from(value: GodotMessageSerde) -> Result<Self, Self::Error> {
        #![allow(unused_assignments)]
        let mut id = 0;
        let mut status: ProcessStatus = ProcessStatus::Accepted;

        match value._type {
            GodotMessageType::ActionResponse => {
                if let GodotMessageSerdeData::ActionResponse(ar) = value.data {
                    id = ar.temp_id;

                    status = match ar.action_id {
                        -1 => Rejected,
                        i => {
                            if i < 0 {
                                return Err(lruntimeerror!(
                                    "GodotMessageSerde",
                                    "action response is not in {-1} + N"
                                ));
                            } else {
                                ProcessStatus::Accepted
                            }
                        }
                    }
                    /*status = if ar.action_id < -1 {
                        return Err(SpecialError("", "".to_string()));
                    } else if ar.action_id == -1 {
                        ActionDenied
                    } else {
                        ActionResponse(ar.action_id as usize)
                    };*/
                } else {
                    unreachable!("{:?} and expected ActionResponse", value.data)
                }
            }
            GodotMessageType::ActionFeedback => {
                if let GodotMessageSerdeData::ActionFeedback(af) = value.data {
                    id = af.action_id;
                    status = ProcessStatus::Running(Some(af.feedback));
                } else {
                    unreachable!("{:?} and expected ActionFeedback", value.data)
                }
            }
            GodotMessageType::ActionResult => {
                if let GodotMessageSerdeData::ActionResult(ar) = value.data {
                    id = ar.action_id;
                    status = match ar.result {
                        true => ProcessStatus::Success,
                        false => ProcessStatus::Failure,
                    };
                } else {
                    unreachable!("{:?} and expected ActionResult", value.data)
                }
            }
            GodotMessageType::ActionPreempt => {
                if let GodotMessageSerdeData::ActionId(ai) = value.data {
                    id = ai.action_id;
                    status = ProcessStatus::Pending;
                } else {
                    unreachable!("{:?} and expected ActionId", value.data)
                }
            }
            GodotMessageType::ActionCancel => {
                if let GodotMessageSerdeData::ActionCancel(ac) = value.data {
                    id = ac.action_id;
                    status = ProcessStatus::Cancelled(ac.cancelled);
                } else {
                    unreachable!("{:?} and expected ActionCancel", value.data)
                }
            }
            _ => {
                unreachable!()
            }
        };

        Ok((id, status))
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
            //Test if its an int
            return match atom.canonical_str().parse::<i64>() {
                Ok(int) => Ok(LValueS::Int(int)),
                Err(_) => match atom.canonical_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValueS::Float(float)),
                    Err(_) => match atom.canonical_str() {
                        //Test if its a Boolean
                        "true" => Ok(LValueS::Bool(true)),
                        "false" => Ok(LValueS::Bool(false)),

                        s => Ok(LValueS::Symbol(s.to_string())),
                    },
                },
            };
        }
        SExpr::List(list) => {
            let list_iter = list.iter();
            let vec: Vec<LValueS> = list_iter.map(parse_into_lvalue).collect::<Result<_, _>>()?;
            Ok(LValueS::List(vec))
        }
    }
}

#[cfg(test)]
mod tests {}
