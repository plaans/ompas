(begin
    (def-types room gripper ball new_bool)
    (def-constants
    '(left right gripper)
    '(no_place room)
    '(no_ball ball)
    '(yes no new_bool))
    ;state functions
    (def-state-function at-robby '(?r room))
    (def-state-function at '(?b ball) '(?r room))
    (def-state-function carry '(?g gripper) '(?b ball))
    (def-state-function connected '(?r room) '(?r room) '(?r new_bool))

    ;actions
    (def-action move '(?from room) '(?to room))
    (def-action-model move
        '((:params (?from room) (?to room))
          (:pre-conditions (and-cond 
            (= (at-robby) ?from)
            (!= ?from ?to))
            (= (connected ?from ?to) yes))
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
    (def-task t1 '(?ball ball) '(?room room))

    (def-method m1
        '((:task t1)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room))
          (:pre-conditions (and-cond 
            ( = (at ?ball) (at-robby))
            (= (carry ?gripper) no_ball)
            (= ?departure (at-robby))))
          (:score 0)
          (:body
            (do
                (pick ?ball ?departure ?gripper)
                (t_move ?room)
                (drop ?ball ?room ?gripper)))))

    (def-method m2
        '((:task t1)
          (:params (?ball ball) (?room room) (?gripper gripper) (?departure room) (?intermediaire room))
          (:pre-conditions (and-cond 
            (!= (at ?ball) (at-robby))
            (= (carry ?gripper) no_ball)
            (= ?departure (at-robby))
            (= ?intermediaire (at ?ball))))
          (:score 0)
          (:body
            (do
                (t_move ?intermediaire)
                (pick ?ball ?intermediaire ?gripper)
                (t_move ?room)
                (drop ?ball ?room ?gripper)))))


    (def-task t_move '(?to room))
    (def-method m_already_there
        '((:task t_move)
          (:params (?to room))
          (:pre-conditions (check (= (at-robby) ?to)))
          (:score 0)
          (:body true)))

    (def-method m_recursive
        '((:task t_move)
          (:params (?to room) (?intermediaire room))
          (:pre-conditions (and-cond
            (!= (at-robby) ?to)
            (= (connected (at-robby) ?intermediaire) yes)))
          (:score 0)
          (:body 
            (do 
                (move (at-robby) ?intermediaire)
                (t_move ?to)
        ))))

)