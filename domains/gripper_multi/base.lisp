(begin

    (def-init
        (do
            (mapf (lambda (?r)
                    (new-resource ?r)
                    (mapf (lambda (?g)
                        (define g (concatenate ?r "_" ?g))
                        (new-resource g)
                        (assert-static 'gripper_of ?r ?g g))
                    (instances gripper)))
                (instances robot))))

    (define gripper-multi-path
        (concatenate (get-env-var "OMPAS_PATH") "/domains/gripper_multi"))
    (set-current-dir gripper-multi-path)
    (read "../gripper_door/base.lisp")

    (remove-object robby)

    ; Additional state functions
    (remove-state-function at-robby)
    (def-state-function at-rob (:params (?r robot)) (:result room))
    (def-state-function carry (:params (?r robot) (?g gripper)) (:result carriable))

    ; New commands
    (def-command move (:params (?r robot) (?from room) (?to room) (?d door)))
    (def-command-pddl-model move
        (:params (?r robot) (?from room) (?to room) (?d door))
        (:pre-conditions
            (= (at-rob ?r) ?from)
            (connects ?from ?d ?to)
            (opened ?d))
        (:effects
            (durative 5 'at-rob ?r ?to)))

    (def-command pick (:params (?ro robot) (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model pick
      (:params (?ro robot) (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (pos ?o) ?r)
        (= (at-rob ?ro) ?r)
        (= (carry ?ro ?g) empty))
      (:effects
            (durative 5 'carry ?ro ?g ?o)
            ('pos ?o ?ro)))

    (def-command drop (:params (?ro robot) (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model drop
      (:params (?ro robot) (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (carry ?ro ?g) ?o)
        (= (at-rob ?ro) ?r))
     (:effects
        (durative 5 'carry ?ro ?g empty)
        ('pos ?o ?r)))


    (def-command open (:params (?ro robot) (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model open
        (:params (?ro robot) (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-rob ?ro) ?r)
            (is_door_of ?d ?r)
            (= (carry ?ro ?g) 'empty))
        (:effects
            ('opened ?d true)))

    (def-command close (:params (?ro robot) (?d door) (?r room) (?g gripper)))
    (def-command-pddl-model close
        (:params (?ro robot) (?d door) (?r room) (?g gripper))
        (:pre-conditions
            (= (at-rob ?ro) ?r)
            (is_door_of ?d ?r)
            (= (carry ?ro ?g) 'empty))
        (:effects
            ('opened ?d false)))
)