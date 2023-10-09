(begin
    (def-types
        (room location)
        gripper
        (ball carriable))
    (def-objects
      (left right gripper)
      (empty carriable)
      (robby location)
    )

    ;state functions
    (def-state-function at-robby (:result room))
    (def-state-function pos (:params (?o carriable)) (:result location))
    (def-state-function carry (:params (?g gripper)) (:result carriable))

    ;actions
    (def-command move (:params (?from room) (?to room)))
    (def-command-om-model move
      (:params (?from room) (?to room))
      (:body
            (do
                (check (= (at-robby) ?from))
                (check (!= ?from ?to))
                (transitive-effect 1 'at-robby ?to))))

    (def-command pick (:params (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model pick
      (:params (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (pos ?o) ?r)
        (= (at-robby) ?r)
        (= (carry ?g) empty))
      (:effects
            ('carry ?g ?o)
            ('pos ?o 'robby)))

    (def-command drop (:params (?o carriable) (?r room) (?g gripper)))
    (def-command-pddl-model drop
      (:params (?o carriable) (?r room) (?g gripper))
      (:pre-conditions
        (= (carry ?g) ?o)
        (= (at-robby) ?r))
     (:effects
        ('carry ?g empty)
        ('pos ?o ?r ))))
