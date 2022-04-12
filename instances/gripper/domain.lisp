(begin

    (def-types room gripper ball)
    (def-constants
    '(left right gripper)
    '(no_place room)
    '(no_ball ball)) 
    ;state functions
    (def-state-function at-robby '(?r room))
    (def-state-function at '(?b ball) '(?r room))
    (def-state-function carry '(?g gripper) '(?b ball))

    ;actions
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
          (:pre-conditions (and-cond 
            (= (at ?obj) ?room)
            (= (at-robby) ?room)
            (= (carry ?gripper) no_ball)))
          (:effects
            (begin
                (assert `(carry ,?gripper) ?obj)
                (assert `(at ,?obj) no_place)))))

    (def-action drop '(?obj ball) '(?room room) '(?gripper gripper))
    (def-action-model drop
        '((:params (?obj ball) (?room room) (?gripper gripper))
          (:pre-conditions (and-cond 
            (= (carry ?gripper) ?obj)
            (= (at-robby) ?room)))
          (:effects
            (begin
                (assert `(carry ,?gripper) no_ball)
                (assert `(at ,?obj) ?room )))))

    ;task with their methods
    (def-task pick-and-drop '(?ball ball) '(?room room))

    (def-method m1
        '((:task pick-and-drop)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
          (:pre-conditions (and-cond 
            ( = (at ?ball) (at-robby))
            (= (carry ?gripper) no_ball)
            (= ?departure (at-robby))))
          (:score 0)
          (:body
            (do
                (pick ?ball ?departure ?gripper)
                (move ?departure ?room)
                (drop ?ball ?room ?gripper)))))

    (def-method m2
        '((:task pick-and-drop)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room) (?intermediaire room))
          (:pre-conditions (and-cond (!= (at ?ball) (at-robby)) (= (carry ?gripper) no_ball) (= ?departure (at-robby)) (= ?intermediaire (at ?ball))))
          (:score 0)
          (:body
            (do
                (move ?departure ?intermediaire)
                (pick ?ball ?intermediaire ?gripper)
                (move ?intermediaire ?room)
                (drop ?ball ?room ?gripper)))))


    (def-task move-and-pick '(?ball ball))
    (def-method m_move
        '((:task move-and-pick)
          (:params (?ball ball) (?gripper gripper))
          (:pre-conditions (and-cond 
            (!= (at ?ball) no_place)
            (!= (at ?ball) (at-robby))
            (= (carry ?gripper) no_ball)))
          (:score 0)
          (:body
            (do
              (move (at-robby) (at ?ball))
              (check (= (at-robby) (at ?ball)))
              (pick ?ball (at ?ball) ?gripper)))))
)