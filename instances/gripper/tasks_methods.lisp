(begin
    (def-task pick-and-drop '(?ball ball) '(?room room))
    (def-method m1
        '((:task pick-and-drop)
          (:params (?gripper gripper) (?ball ball) (?room room) (?departure room))
          (:pre-conditions (and ( = (at ?ball) (at-robby)) (free ?gripper) (= ?departure room)))
          (:score 0)
          (:body
            (begin
                (pick ?ball ?departure ?gripper)
                (move ?departure ?room)
                (drop ?ball ?room ?gripper)))))

    (def-method m2
        '((:task pick-and-drop)
          (:params (?gripper gripper) (?ball ball) (?room room) (?departure room) (?intermediaire room))
          (:pre-conditions (and (!= (at ?ball) (at-robby)) (free ?gripper) (= ?departure (at-robby)) (= intermediaire (at ?ball))))
          (:score 0)
          (:body
            (begin
                (move ?departure ?intermediaire)
                (pick ?ball ?intermediaire ?gripper)
                (move ?intermediaire ?room)
                (drop ?ball ?room ?gripper)))))
)