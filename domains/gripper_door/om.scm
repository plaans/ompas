(begin
    (define gripper-door-path
            (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
    (set-current-dir gripper-door-path)
    (load "../gripper/om.scm")
    (remove-method m_move)

    (def-function min-distance
        (:params (?r1 room) (?r2 room))
        (:result int))

    (def-method m_move
        (:task go2)
        (:params (?r room) (?a room) (?n room) (?d door))
        (:cost (+ 1 (min-distance ?n ?r)))
        (:pre-conditions
            (= (at-robby) ?a)
            (!= ?a ?r)
            (connects ?a ?d ?n))
        (:body
            (do
                (t_open ?a ?d)
                (move ?a ?n ?d)
                (go2 ?r))))

    (def-task t_open (:params (?r room) (?d door)))
    (def-method open_noop
        (:task t_open)
        (:cost 0)
        (:params (?r room) (?d door))
        (:pre-conditions (opened ?d))
        (:body nil))

    (def-method open_direct
        (:task t_open)
        (:cost 1)
        (:params (?r room) (?d door) (?g gripper))
        (:pre-conditions (= (carry ?g) empty) (! (opened ?d)))
        (:body
            (open ?d ?r ?g)))

    (def-method drop_and_open
        (:task t_open)
        (:params (?r room) (?d door))
        (:cost 3)
        (:pre-conditions
            (! (opened ?d))
            (forall
                (instances gripper)
                (lambda (?g) (!= (carry ?g) empty))))
        (:body
            (do
                (define ?g (arbitrary (instances gripper)))
                (define ?o (carry ?g))
                (drop ?o ?r ?g)
                (open ?d ?r ?g)
                (pick ?o ?r ?g))))
)