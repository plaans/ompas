(begin

    (def-types ball room gripper door robot new_bool)
    (def-constants 
        (no_ball ball)
        (no_place room)
        (yes no new_bool))
        (no_door door)
        (left right)
    ;state functions
    (def-state-function at-robot '(?r robot) '(?room room))
    (def-state-function at-ball '(?b ball) '(?room room))
    (def-state-function carry '(?r robot) '(?gripper gripper) '(?b ball))
    (def-state-function connected '(?rx room) '(?ry room) '(?r new_bool))
    (def-state-function door-used '(?d door) '(?r new_bool))
    (def-state-function door-opened '(?d) '(?r new_bool))
    (def-state-function door (?r room) (?d door) (?r new_bool))

    (def-action open '(?r robot) '(?d door) '(?g gripper))
    (def-command-pddl-model open
      (:params (?r robot) (?d door) (?g gripper))
      (:pre-conditions (and-cond (= (door (at-robot ?r) ?d) yes) (= (door-opened ?d) no) (= (carry ?r ?g) no_ball)))
      (:effects
            ('carry)))
    
    )

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

    ;task with their methods
    (def-task pick-and-drop '(?ball ball) '(?room room))

    (def-method m1
        '((:task pick-and-drop)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
          (:pre-conditions (and-cond ( = (at ?ball) (at-robby)) (= (carry ?gripper) no_ball) (= ?departure (at-robby))))
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

)