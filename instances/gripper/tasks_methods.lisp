(begin
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