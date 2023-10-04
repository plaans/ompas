pub enum Object {
    Robby,
    Robot(u32),
    Ball(u32),
    ToyPart(ToyPart),
}

enum ToyPart {
    Leg(u32),
    Arm(u32),
    Head(u32),
    Torso(u32),
}
