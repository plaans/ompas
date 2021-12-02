(begin
    (def-action move ?from ?to)
    (def-action-model move
        '((:params (?from room) (?to room))
          (:pre-conditions (at-robby ?from))
          (:effects
            (begin
                (assert (at-robby ?from) false)
                (assert (at-robby ?to) true)))))

    (def-action pick ?obj ?room ?gripper)
    (def-action-model move
        '((:params (?obj object) (?room room) (?gripper gripper))
          (:pre-conditions (and (at ?obj ?room) (at-robby ?room) (free ?gripper)))
          (:effects
            (begin
                (assert (carry ?obj ?gripper) true)
                (assert (at ?obj ?room) false)
                (assert (free ?gripper) false)))))

    (def-action drop ?obj ?room ?gripper)
    (def-action-model drop
        '((:params (?obj object) (?room room) (?gripper gripper))
          (:pre-conditions (and (carry ?obj ?gripper) (at-robby ?room)))
          (:effects
            (begin
                (assert (carry ?obj ?gripper) false)
                (assert (at ?obj ?room) true)
                (assert (free ?gripper) true)))))
)