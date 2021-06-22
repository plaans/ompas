pub const GODOT_DOMAIN: &str = "\
(begin \
    (defaction navigate_to ?r ?x ?y)\
    (defaction navigate_to_cell ?r ?cx ?cy)
    (defaction navigate_to_area ?r ?area)
    (defaction pick ?r)
    (defaction place ?r)
    (defaction rotate_to ?r ?a ?w)
    (defaction face_object ?node_name ?speed)
    (def-state-function robot.coordinates ?r)
)\
";
