use aries_planning::parsing::sexpr::parse;
use ompas_apps::{parse_into_lvalue, GodotMessageType, GodotState};

const TEST: &str = "(robot (test of robot) (1 1 1) ((1 2) (3 4)))";

fn main() {
    println!("Hello, World!");
    let data = match parse_into_lvalue(&parse(TEST).expect("couldn't parse test")) {
        Ok(s) => s,
        Err(_) => panic!("couldn't transform into lvalue"),
    };

    let godot_state = GodotState {
        _type: GodotMessageType::Static,
        data,
    };

    let serialized = serde_json::to_string(&godot_state).unwrap();

    println!("serialized = {}", serialized);

    // Convert the JSON string back to a Point.
    let deserialized: GodotState = serde_json::from_str(&serialized).unwrap();

    println!("deserialized = {}", deserialized);
}
