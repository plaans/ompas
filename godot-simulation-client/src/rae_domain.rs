pub const GODOT_DOMAIN: &str = "\
(begin \
    (def-action navigate_to ?r ?x ?y)\
    (def-action navigate_to_cell ?r ?cx ?cy)
    (def-action navigate_to_area ?r ?area)
    (def-action pick ?r)
    (def-action place ?r)
    (def-action rotate_to ?r ?a ?w)
    (def-action face_object ?node_name ?speed)
    (def-state-function robot.coordinates ?r)
)\
";
