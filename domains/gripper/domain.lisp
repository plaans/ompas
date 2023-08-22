(begin

    (def-types room gripper ball)
    (def-objects
      (left right gripper)
      (no_place room)
      (no_ball ball)) 
    ;state functions
    (def-state-function at-robby (:result room))
    (def-state-function at (:params (?b ball)) (:result room))
    (def-state-function carry (:params (?g gripper)) (:result ball))

    ;actions
    (def-command move (:params (?from room) (?to room)))
    (def-command-pddl-model move
      (:params (?from room) (?to room))
      (:pre-conditions (= (at-robby) ?from) (!= ?from ?to))
      (:effects
            ('at-robby ?to)))

    (def-command pick (:params (?obj ball) (?room room) (?gripper gripper)))
    (def-command-pddl-model pick
      (:params (?obj ball) (?room room) (?gripper gripper))
      (:pre-conditions
        (= (at ?obj) ?room)
        (= (at-robby) ?room)
        (= (carry ?gripper) no_ball))
      (:effects
            ('carry ?gripper ?obj)
            ('at ?obj no_place)))

    (def-command drop (:params (?obj ball) (?room room) (?gripper gripper)))
    (def-command-pddl-model drop
      (:params (?obj ball) (?room room) (?gripper gripper))
      (:pre-conditions
        (= (carry ?gripper) ?obj)
        (= (at-robby) ?room))
     (:effects
        ('carry ?gripper no_ball)
        ('at ?obj ?room )))

    ;task with their methods
    (def-task pick-and-drop (:params (?ball ball) (?room room)))

    (def-method m1
      (:task pick-and-drop)
      (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
      (:pre-conditions 
        ( = (at ?ball) (at-robby))
        (= (carry ?gripper) no_ball)
        (= ?departure (at-robby)))
      (:score 0)
      (:body
        (do
          (pick ?ball ?departure ?gripper)
          (move ?departure ?room)
          (drop ?ball ?room ?gripper))))

    (def-method m2
      (:task pick-and-drop)
      (:params (?ball ball) (?room room) (?gripper gripper) (?departure room) (?intermediaire room))
      (:pre-conditions (!= (at ?ball) (at-robby)) (= (carry ?gripper) no_ball) (= ?departure (at-robby)) (= ?intermediaire (at ?ball)))
      (:score 0)
      (:body
        (do
          (move ?departure ?intermediaire)
          (pick ?ball ?intermediaire ?gripper)
          (move ?intermediaire ?room)
          (drop ?ball ?room ?gripper))))


    (def-task move-and-pick (:params (?ball ball)))
    (def-method m_move
      (:task move-and-pick)
      (:params (?ball ball) (?gripper gripper))
      (:pre-conditions
        (!= (at ?ball) no_place)
        (!= (at ?ball) (at-robby))
        (= (carry ?gripper) no_ball))
      (:score 0)
      (:body
        (do
          (move (at-robby) (at ?ball))
          (check (= (at-robby) (at ?ball)))
          (pick ?ball (at ?ball) ?gripper))))
)