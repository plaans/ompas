(begin
    (def-task pick-and-drop '(?ball ball) '(?room room) '(?gripper gripper) '(?departure departure))
    (def-method m1
        '((:task pick-and-drop)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
          (:pre-conditions true)
          (:score 0)
          (:body
            (do
                (pick ?ball ?departure ?gripper)
                (move ?departure ?room)
                (drop ?ball ?room ?gripper)))))
    (def-task t_pick '(?ball ball) '(?room room) '(?gripper gripper))
    (def-method m_pick
            '((:task t_pick)
              (:params (?ball ball) (?room room) (?gripper gripper))
              (:pre-conditions true)
              (:score 0)
              (:body
                    (pick ?ball ?room ?gripper))))

)