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
)