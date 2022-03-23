(begin
    (def-action move '(?from room) '(?to room))
    (def-action-model move
        '((:params (?from room) (?to room))
          (:pre-conditions (and-cond (= (at-robby) ?from) (!= ?from ?to)))
          (:effects
            (begin
                (assert 'at-robby ?to)))))

    (def-action pick '(?obj ball) '(?room room) '(?gripper gripper))
    (def-action-model pick
        '((:params (?obj ball) (?room room) (?gripper gripper))
          (:pre-conditions (and-cond (= (at ?obj) ?room) (= (at-robby) ?room) (= (carry ?gripper) no_ball)))
          (:effects
            (begin
                (assert `(carry ,?gripper) ?obj)
                (assert `(at ,?obj) no_place)))))

    (def-action drop '(?obj ball) '(?room room) '(?gripper gripper))
    (def-action-model drop
        '((:params (?obj ball) (?room room) (?gripper gripper))
          (:pre-conditions (and-cond (= (carry ?gripper) ?obj) (= (at-robby) ?room)))
          (:effects
            (begin
                (assert `(carry ,?gripper) no_ball)
                (assert `(at ,?obj) ?room )))))
)