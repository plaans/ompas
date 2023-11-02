(begin
    (def-task go2 (:params (?ro robot) (?r room)))
    (def-method go2_noop
        (:task go2)
        (:params (?ro robot) (?r room))
        (:pre-conditions (= (at-rob ?ro) ?r))
        (:body nil))

    (def-method move_direct
        (:task go2)
        (:params (?ro robot) (?r room) (?a room) (?n room) (?d door))
        (:pre-conditions
            (= (at-rob ?ro) ?a)
            (!= ?a ?r)
            (connected ?a ?d ?n)
            (opened ?d))
        (:body
            (do
                (move ?ro ?a ?n ?d)
                (go2 ?ro ?r))))

    (def-method open_and_move
        (:task go2)
        (:params (?ro robot) (?r room) (?a room) (?n room) (?d door))
        (:pre-conditions
            (= (at-rob ?ro) ?a)
            (!= ?a ?r)
            (connected ?a ?d ?n)
            (! (opened ?d)))
        (:body
            (do
                (t_open ?ro ?d ?a)
                (move ?ro ?a ?n ?d)
                (go2 ?ro ?r))))

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
                (define rh (acquire ?ro))
                (go2 ?ro ?r)
                (drop ?ro ?o ?r ?g))))



    (def-task t_open (:params (?ro robot) (?d door) (?r room)))
    (def-method open_direct
        (:task t_open)
        (:params  (?ro robot) (?d door) (?r room) (?g gripper))
        (:pre-conditions 
            (= (carry ?ro ?g) empty)
            (is_door_of ?d ?r))
        (:body
            (do
                ;(define rh (acquire ?ro))
                (go2 ?ro ?r)
                (open ?ro ?d ?r ?g))))

    (def-method drop_and_open
        (:task t_open)
        (:params (?ro robot) (?d door) (?r room))
        (:pre-conditions
            (is_door_of ?d ?r)
            (! (exists
                (instances gripper)
                (lambda (?g) (= (carry ?ro ?g) empty)))))
        (:body
            (do
                (define ?g (arbitrary (instances gripper)))
                (go2 ?ro ?r)
                (define ?o (carry ?ro ?g))
                (drop ?ro ?o ?r ?g)
                (open ?ro ?d ?r ?g)
                (pick ?ro ?o ?r ?g
            ))))
)