use aries_planning::parsing::sexpr::parse;
use ompas_godot_simulation_client::serde::{
    parse_into_lvalue, GodotMessageType, GodotState, GodotStateS,
};

const TEST: &str = "(robot (test of robot) (1 1 1.0) ((1 2) (3 4)) () true nil)";

fn main() {
    test_lvalues();
    test_lvalue();
}

fn test_lvalues() {
    println!("Hello, World!");
    let data = match parse_into_lvalue(&parse(TEST).expect("couldn't parse test")) {
        Ok(s) => s,
        Err(_) => panic!("couldn't transform into lvalue"),
    };

    let godot_state = GodotStateS {
        _type: GodotMessageType::Static,
        data,
    };

    let serialized = serde_json::to_string(&godot_state).unwrap();

    println!("serialized = {}", serialized);

    // Convert the JSON string back to a Point.
    let deserialized: GodotStateS = serde_json::from_str(&serialized).unwrap();

    println!("deserialized = {}", deserialized);
}

fn test_lvalue() {
    println!("Hello, World!");

    let sexpr = parse(TEST).expect("couldn't parse test");
    let data = ompas_lisp::core::parse_into_lvalue(&sexpr);

    let godot_state = GodotState {
        _type: GodotMessageType::Static,
        data,
    };

    let serialized = serde_json::to_string(&godot_state).unwrap();

    println!("sexpr: {}", sexpr);
    println!("lvalue: {:?}", godot_state.data);
    println!("serialized = {}", serialized);

    // Convert the JSON string back to a Point.
    let deserialized: GodotState = serde_json::from_str(&serialized).unwrap();

    println!("deserialized = {}", deserialized);
    println!("debug deserialized = {:?}", deserialized);
}
