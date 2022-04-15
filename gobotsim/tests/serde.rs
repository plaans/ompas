use ompas_gobotsim::serde::{
    GodotMessageSerde, GodotMessageSerdeData, GodotMessageType, SerdeActionCancel,
    SerdeActionFeedback, SerdeActionId, SerdeActionResponse, SerdeActionResult, SerdeRobotCommand,
};
use sompas_structs::lerror;
use sompas_structs::lvalue::LValue;

#[test]
fn test_action_response() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResponse,
        data: GodotMessageSerdeData::ActionResponse(SerdeActionResponse {
            temp_id: 0,
            action_id: 0,
        }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_robot_command() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::RobotCommand,
        data: GodotMessageSerdeData::RobotCommand(SerdeRobotCommand {
            command_info: LValue::List(
                vec!["navigate_to".into(), "robot1".into(), 50.into(), 100.into()].into(),
            )
            .into(),
            temp_id: 0,
        }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_feedback() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionFeedback,
        data: GodotMessageSerdeData::ActionFeedback(SerdeActionFeedback {
            action_id: 0,
            feedback: 0.5,
        }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_result() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionResult,
        data: GodotMessageSerdeData::ActionResult(SerdeActionResult {
            action_id: 0,
            result: false,
        }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_preempt() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionPreempt,
        data: GodotMessageSerdeData::ActionId(SerdeActionId { action_id: 0 }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_action_cancel() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::ActionCancel,
        data: GodotMessageSerdeData::ActionCancel(SerdeActionCancel {
            temp_id: 0,
            cancelled: false,
        }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_cancel_request() -> lerror::Result<()> {
    let action_msg = GodotMessageSerde {
        _type: GodotMessageType::CancelRequest,
        data: GodotMessageSerdeData::ActionId(SerdeActionId { action_id: 0 }),
    };

    let string = serde_json::to_string(&action_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_state_static() -> lerror::Result<()> {
    let state_msg = GodotMessageSerde {
        _type: GodotMessageType::StaticState,
        data: GodotMessageSerdeData::LValue(
            LValue::List(vec![10.into(), 20.into(), 30.into()]).into(),
        ),
    };

    let string = serde_json::to_string(&state_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}

#[test]
fn test_state_dynamic() -> lerror::Result<()> {
    let state_msg = GodotMessageSerde {
        _type: GodotMessageType::DynamicState,
        data: GodotMessageSerdeData::LValue(
            LValue::List(vec![10.into(), 20.into(), 30.into()]).into(),
        ),
    };

    let string = serde_json::to_string(&state_msg).unwrap();
    println!("{}", string);

    let msg: GodotMessageSerde = serde_json::from_str(&string).unwrap();
    println!("{:?}", msg);
    Ok(())
}
