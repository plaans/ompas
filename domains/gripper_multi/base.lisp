(begin
    (def-types robot)
    ; Additional state functions
    (def-state-function at-rob (:params (?r robot)) (:result room))
    (def-state-function carry (:params (?r robot) (?g gripper)) (:result carriable))

    ; New commands
    (def-command move (:params (?r robot) (?from room) (?to room) (?d door)))
    (def-command-pddl-model move
        (:params (?r robot) (?from room) (?to room) (?d door))
        (:pre-conditions
            (= (at-rob ?r) ?from)
            (connects ?from ?to ?d)
            (opened ?d))
        (:effects
            ('at-rob ?r ?to)))

    (def-command pick (:params (?ro robot) (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model pick
      (:params (?ro robot) (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (pos ?o) ?r)
        (= (at-rob ?ro) ?r)
        (= (carry ?g) empty))
      (:effects
            ('carry ?ro ?g ?o)
            ('pos ?o ?ro)))

    (def-command drop (:params (?ro robot) (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model drop
      (:params (?ro robot) (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (carry ?g) ?o)
        (= (at-rob ?ro) ?ro))
     (:effects
        ('carry ?ro ?g empty)
        ('pos ?o ?ro)))


    (def-command open (:params (?ro robot) (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model open
        (:params (?ro robot) (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-rob ?ro) ?r)
            (connects ?r ?d ?r2)
            (= (carry ?ro ?g) 'empty))
        (:effects
            ('opened ?d true)))

    (def-command close (:params (?ro robot) (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model close
        (:params (?ro robot) (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-rob ?ro) ?r)
            (connects ?r ?d ?r2)
            (= (carry ?ro ?g) 'empty))
        (:effects
            ('opened ?d false)))
)