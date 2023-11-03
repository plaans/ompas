(begin
    (def-types
            (room robot location)
            gripper
            (ball carriable))
        (def-objects
          (left right gripper)
          (empty carriable)
          (robby robot)
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
        ('pos ?o ?r )))

    (def-task go2 (:params (?r room)))
    (def-method go2_noop
        (:task go2)
        (:params (?r room))
        (:pre-conditions (= (at-robby) ?r))
        (:body nil))

    (def-method m_move
        (:task go2)
        (:params (?r room))
        (:pre-conditions (!= (at-robby) ?r))
        (:body (move (at-robby) ?r)))
    ;task with their methods
    (def-task place (:params (?o carriable) (?r room)))
    (def-method place_noop
        (:task place)
        (:params (?o carriable) (?r room))
        (:pre-conditions (= (pos ?o) ?r))
        (:body nil))

    (def-method pick_and_drop
        (:task place)
        (:params (?o carriable) (?r room))
        (:pre-conditions (!= (pos ?o) ?r) (!= (pos ?o) robby))
        (:body
            (do
                (define rh (acquire 'robby))
                (define ?a (pos ?o))
                (go2 ?a)
                (define ?g (arbitrary (instances gripper)))
                (pick ?o ?a ?g)
                (go2 ?r)
                (drop ?o ?r ?g)
                (release rh)
                )))

    (translate pick)
    (exit 0))