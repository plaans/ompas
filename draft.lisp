(begin
    ;(define ompas_path (get-env-var "OMPAS_PATH"))
    ;(set-current-dir ompas_path)
    ;(read "./domains/gripper_door/domain.lisp")
    ;(translate move_direct)

    ;domain
    (define gripper-door-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
        (set-current-dir gripper-door-path)
        (read "../gripper/base.lisp")

        (def-types door)
        ; Additional state functions
        (def-state-function opened
            (:params (?d door))
            (:result boolean))

        (def-function connects
            (:params (?r1 room) (?d door) (?r2 room))
            (:result boolean))

        ; New commands
        (def-command move (:params (?from room) (?to room) (?d door)))
        (def-command-pddl-model move
            (:params (?from room) (?to room) (?d door))
            (:pre-conditions
                (= (at-robby) ?from)
                (connects ?from ?d ?to)
                (opened ?d))
            (:effects
                ('at-robby ?to)
            ))


    (def-lambda connected (lambda (?r1 ?d ?r2)
        (or (connects ?r1 ?d ?r2) (connects ?r2 ?d ?r1))))


    (def-lambda is_door_of (lambda (?d ?r)
        (exists (instances room)
            (lambda (?r2)
                (connects ?r ?d ?r2)
            ))))

    (def-command open (:params (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model open
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-robby) ?r)
            (= (carry ?g) empty)
            (is_door_of ?d ?r))
        (:effects
            ('opened ?d true)))

    (def-command close (:params (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model close
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-robby) ?r)
            (= (carry ?g) empty)
            (is_door_of ?d ?r))
        (:effects
            ('opened ?d false)))

    (def-task t_open (:params (?d door) (?r room)))
    (def-method open_direct
        (:task t_open)
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions (= (carry ?g) empty))
        (:body
            (open ?d ?r ?g)
        ))

    (def-method drop_and_open
        (:task t_open)
        (:params (?d door) (?r room))
        (:pre-conditions
   ;         (! (exists (instances gripper) (lambda (?g) (= (carry ?g) empty)))))
            (forall
                (instances gripper)
                (lambda (?g) (!= (carry ?g) empty))))
        (:body
            (do
                (define ?g (arbitrary (instances gripper)))
                (define ?o (carry ?g))
                (drop ?o ?r ?g)
                (open ?d ?r ?g)
                (pick ?o ?r ?g)
            )))

    ;problem
    (def-objects
    		(ball_0 ball_1 ball_3 ball_2 ball)
    		(room_0 room_1 room)
    		(door_0 door_1 door)
    	)
    	(def-facts
    		(at-robby room_0)
    		((carry left) ball_0)
    		((carry right) ball_1)
    		((pos ball_0) robby)
    		((pos ball_1) robby)
    		((pos ball_3) room_0)
    		((pos ball_2) room_1)
    		((opened door_0) false)
    		((opened door_1) false)
    	)

    	(def-static-facts
    		((connects room_0 door_0 room_1) true)
    		((connects room_0 door_0 room_0) false)
            ((connects room_0 door_1 room_1) true)
            ((connects room_0 door_1 room_0) false)
            ((connects room_1 door_0 room_1) false)
            ((connects room_1 door_0 room_0) true)
            ((connects room_1 door_1 room_1) false)
            ((connects room_1 door_1 room_0) true)
    	)

;    (plan test)
    (plan t_open door_1 room_0)

    (exit 0))