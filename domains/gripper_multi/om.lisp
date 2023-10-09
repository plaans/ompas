(begin
    (def-task go2 (:params (?ro robot) (?r room)))
    (def-method go2_noop
        (:task go2)
        (:params (?ro robot) (?r room))
        (:pre-conditions (= (at-rob ?ro) ?r))
        (:body nil))

    (def-method move_direct
        (:task go2)
        (:params (?ro robot) (?r room) (?d door))
        (:pre-conditions
            (!= (at-rob ?ro) ?r)
            (is_door_of ?d ?r)
            (opened ?d))
        (:body
            (do
                (move (at-rob ?ro) ?r ?d)
                (go2 ?r))))

    (def-method open_and_move
        (:task go2)
        (:params (?ro robot) (?r room) (?d door))
        (:pre-conditions
            (!= (at-rob ?ro) ?r)
            (is_door_of ?d ?r)
            (! (opened ?d)))
        (:body
            (do
                (t_open ?r ?d)
                (move (at-rob ?ro) ?r ?d)
                (go2 ?r))))

    ;task with their methods
    (def-task place (:params (?o carriable) (?r room)))
    (def-method place_noop
        (:task place)
        (:params (?o carriable) (?r room))
        (:pre-conditions (= (pos ?o) ?r))
        (:body nil))

    (def-method pick_and_drop
        (:task place)
        (:params (?o carriable) (?r room) (?ro robot) (?g gripper))
        (:pre-conditions
            (!= (pos ?o) ?r)
            (instance (pos ?o) room)
            (= (carry ?ro ?g) empty)
            )
        (:body
            (do
                (define rh (acquire ?ro))
                (define ?a (pos ?o))
                (go2 ?ro ?a)
                (pick ?ro ?o ?a ?g)
                (go2 ?ro ?r)
                (drop ?ro ?o ?r ?g))))


    (def-method move_and_drop
        (:task place)
        (:params (?o carriable) (?r room) (?ro robot) (?g gripper))
        (:pre-conditions (!= (pos ?o) ?r) (= (carry ?ro ?g) ?o))
        (:body
            (do
                (go2 ?ro ?r)
                (drop ?ro ?o ?r ?g))))



    (def-task t_open (:params (?d door) (?r room)))
    (def-method open_direct
        (:task t_open)
        (:params (?d door) (?r room) (?ro robot) (?g gripper))
        (:pre-conditions (= (carry ?ro ?g) empty))
        (:body
            (do
                (define rh (acquire ?ro))
                (open ?ro ?d ?r ?g))))

    (def-method drop_and_open
        (:task t_open)
        (:params (?d door) (?r room) (?ro robot))
        (:pre-conditions
            (! (exists
                (instances gripper)
                (lambda (?g) (= (carry ?ro ?g) empty)))))
        (:body
            (do
                (define ?g (arbitrary (instances gripper)))
                (define ?o (carry ?ro ?g))
                (drop ?ro ?o ?r ?g)
                (open ?ro ?d ?r ?g)
                (pick ?ro ?o ?r ?g
            ))))
)