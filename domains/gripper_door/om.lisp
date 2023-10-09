(begin
    (define gripper-door-path
            (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
    (set-current-dir gripper-door-path)
    (read "../gripper/om.lisp")
    (remove-method m_move)

    (def-method move_direct
        (:task go2)
        (:params (?r room) (?d door))
        (:pre-conditions
            (!= (at-robby) ?r)
            (is_door_of ?d ?r)
            (opened ?d))
        (:body
            (do
                (move (at-robby) ?r ?d)
                (go2 ?r))))

    (def-method open_and_move
        (:task go2)
        (:params (?r room) (?d door))
        (:pre-conditions
            (!= (at-robby) ?r)
            (is_door_of ?d ?r)
            (! (opened ?d)))
        (:body
            (do
                (t_open ?r ?d)
                (move (at-robby) ?r ?d)
                (go2 ?r))))

    (def-task t_open (:params (?d door) (?r room)))
    (def-method open_direct
        (:task t_open)
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions (= (carry ?g) empty))
        (:body
            (open ?d ?r ?g)))

    (def-method drop_and_open
        (:task t_open)
        (:params (?d door) (?r room))
        (:pre-conditions
            (! (exists
                (instances gripper)
                (lambda (?g) (= (carry ?g) empty)))))
        (:body
            (do
                (define ?g (arbitrary (instances gripper)))
                (define ?o (carry ?g))
                (drop ?o ?r ?g)
                (open ?d ?r ?g)
                (pick ?o ?r ?g
            ))))
)