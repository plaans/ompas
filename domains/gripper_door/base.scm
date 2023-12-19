(begin
    (define gripper-door-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_door"))
    (set-current-dir gripper-door-path)
    (load "../gripper/base.scm")

    (def-types door)
    ; Additional state functions
    (def-state-function opened
        (:params (?d door))
        (:result boolean))

    (def-function connects
        (:params (?r1 room) (?d door) (?r2 room))
        (:result boolean))

    (def-function min-distance
        (:params (?r1 room) (?r2 room))
        (:result int))

    ; New commands
    (def-command move (:params (?from room) (?to room) (?d door)))
    (def-command-pddl-model move
        (:params (?from room) (?to room) (?d door))
        (:pre-conditions
            (= (at-robby) ?from)
            (connects ?from ?d ?to)
            (opened ?d))
        (:effects
            (durative 5 'at-robby ?to)
        ))

    (def-lambda is_door_of (lambda (?d ?r)
        (exists (instances room)
            (lambda (?r2) 
                (connects ?r ?d ?r2)
            ))))

    (def-command open (:params (?d door) (?r room) (?g gripper)))
    (def-command-om-model open
        (:params (?d door) (?r room) (?g gripper))
        (:body
            (do
                (check (= (at-robby) ?r))
                (check (= (carry ?g) empty))
                (check (is_door_of ?d ?r))
                (durative-effect 5 'opened ?d true))))

    (def-command close (:params (?d door) (?r room) (?g gripper)))
    (def-command-om-model close
        (:params (?d door) (?r room) (?g gripper))
        (:body
            (do
                (check (= (at-robby) ?r))
                (check (= (carry ?g) empty))
                (check (is_door_of ?d ?r))
                (durative-effect 5 'opened ?d false))))
)