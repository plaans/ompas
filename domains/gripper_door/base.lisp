(begin
    (def-types door)
    ; Additional state functions
    (def-state-function opened
        (:params (?d door))
        (:result boolean))

    (def-function connects
        (:params (?r1 room) (?d2 door) (?r2 room))
        (:result boolean))

    ; New commands
    (def-command move (:params (?from room) (?to room) (?d door)))
    (def-command-pddl-model move
        (:params (?from room) (?to room) (?d door))
        (:pre-conditions
            (= (at-robby) ?from)
            (connects ?from ?to ?d)
            (opened ?d))
        (:effects
            ('at-robby ?to)
        ))


    (def-command open (:params (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model open
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-robby) ?r)
            (connects ?r ?d ?r2)
            (= (carry ?g) 'empty))
        (:effects
            ('opened ?d true)))

    (def-command close (:params (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model close
        (:params (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-robby) ?r)
            (connects ?r ?d ?r2)
            (= (carry ?g) 'empty))
        (:effects
            ('opened ?d false)))
)