use ompas_lisp::structs::{LError, LValue, LValueS};
use ompas_godot_simulation_client::serde::*;


#[test]

#[test]
fn test_action_response() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResponse,
        data: GodotMessageSerdeData::ActionResponse(SerdeActionResponse {
            temp_id: 0,
            action_id: 0
        })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_robot_command() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::RobotCommand,
        data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
            command_info: LValue::List(vec!["navigate_to".into(), "robot1".into(), 50.into(), 100.into()].into()).into(),
            temp_id: 0,
        })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_feedback() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionFeedback,
        data: GodotMessageSerdeData::ActionFeedback(SerdeActionFeedback {
            action_id: 0,
            feedback: 0.5
        })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_result() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResult,
        data: GodotMessageSerdeData::ActionResult(SerdeActionResult {
            action_id: 0,
            result: false
        })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_preempt() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionPreempt,
        data: GodotMessageSerdeData::ActionId(SerdeActionId {
            action_id: 0,
        })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_cancel() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionCancel,
        data: GodotMessageSerdeData::ActionCancel(
            SerdeActionCancel {
                temp_id: 0,
                cancelled: false
            })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_cancel_request() -> Result<(), LError> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::CancelRequest,
        data: GodotMessageSerdeData::ActionId(
            SerdeActionId {
                action_id: 0
            })
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_state_static() -> Result<(), LError> {
    let state_msg = GodotMessageSerde {
        _type: GodotMessageType::StaticState,
        data: GodotMessageSerdeData::LValue(LValue::List(vec![10.into(),20.into(),30.into()]).into())
    };

    let string = serde_json::to_string(&state_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_state_dynamic() -> Result<(), LError> {
    let state_msg = GodotMessageSerde {
        _type: GodotMessageType::DynamicState,
        data: GodotMessageSerdeData::LValue(LValue::List(vec![10.into(),20.into(),30.into()]).into())
    };

    let string = serde_json::to_string(&state_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

/*
fn serialize_action_feedback() -> String {

    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResponse,
        data: LValue::List(vec![LValue::List(vec!["action_id".into(), 0.into()]), LValue::List(vec!["feedback".into(), 0.into()])]).into()
    };

    serde_json::to_string(&action_msg).expect("error while serializing")
}

fn serialize_action_cancel_request() -> String {

    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResponse,
        data: LValue::List(vec!["action_id".into(), 0.into()].into()).into()
    };


    serde_json::to_string(&action_msg).expect("error while serializing")
}

fn serialize_action_result() -> String {

    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResponse,
        data: LValue::List(vec![LValue::List(vec!["action_id".into(), 0.into()]), LValue::List(vec!["result".into(), false.into()])]).into()
    };

    serde_json::to_string(&action_msg).expect("error while serializing")
}

#[test]
fn test_serializing_action_message()-> Result<(), LError> {

    let vec_fun: Vec<Box<fn() -> String>> = vec![Box::new(serialize_action_cancel_request),
                   Box::new(serialize_action_feedback),
                   Box::new(serialize_action_preempt),
                   Box::new(serialize_action_response),
                   Box::new(serialize_action_result)];

    let vec_string = vec_fun.iter().map(|f| f()).collect::<Vec<String>>();

    for string in vec_string {
        println!("{}", string);
    }

    Ok(())
}

#[test]
fn test_action_preempt() -> Result<(), LError> {
    let str = serialize_action_preempt();
    println!("{} ", str );
    let msg : GodotMessageSerde = serde_json::from_str(&str.to_lowercase()).unwrap();
    println!("{}", msg);
    Ok(())
}

#[test]
fn test_serde_cancel_request() -> Result<(), LError> {
    let str = serialize_action_cancel_request();
    println!("{}", str);
    let msg : GodotMessageSerde = serde_json::from_str(&str.to_lowercase()).unwrap();
    println!("{}", msg);

    Ok(())
}

#[test]
fn test_deserialize_string() -> Result<(), LError> {
    let str =
    "{\"type\":\"robot_command\",
        \"data\":
        {\"command_info\" : [\"navigate_to\",\"robot1\",50,100],
            \"temp_id\":0
        }
    }";

    let msg :GodotMessageSerde = serde_json::from_str(&str.to_lowercase()).unwrap();
    println!("{}", msg);

    Ok(())
}

#[test]
fn test_deserializing_action_message() -> Result<(), LError> {
    let vec_fun: Vec<Box<fn() -> String>> = vec![Box::new(serialize_action_cancel_request),
                                                 Box::new(serialize_action_feedback),
                                                 Box::new(serialize_action_preempt),
                                                 Box::new(serialize_action_response),
                                                 Box::new(serialize_action_result)];

    let vec_string = vec_fun.iter().map(|f| f()).collect::<Vec<String>>();

    for str in vec_string {
        let de: GodotMessageSerde = serde_json::from_str(&str.to_lowercase()).unwrap();
        println!("{}", de);
    }
    Ok(())
}*/