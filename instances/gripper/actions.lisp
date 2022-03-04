(begin
    (def-action move '(?from room) '(?to room))
    (def-action-model move
        '((:params (?from room) (?to room))
          (:pre-conditions (= (at-robby) ?from))
          (:effects
            (begin
                (assert 'at-robby ?to)))))

    (def-action pick '(?obj ball) '(?room room) '(?gripper gripper))
    (def-action-model pick
        '((:params (?obj ball) (?room room) (?gripper gripper))
          (:pre-conditions (and (= (at ?obj) ?room) (= (at-robby) ?room) (null? (carry ?gripper))))
          (:effects
            (begin
                (assert `(carry ,?gripper) ?obj)
                (assert `(at ,?obj) nil)))))

    (def-action drop '(?obj ball) '(?room room) '(?gripper gripper))
    (def-action-model drop
        '((:params (?obj ball) (?room room) (?gripper gripper))
          (:pre-conditions (and (= (carry ?gripper) ?obj) (= (at-robby) ?room)))
          (:effects
            (begin
                (assert `(carry ,?gripper) nil)
                (assert `(at ,?obj) ?room )))))
)