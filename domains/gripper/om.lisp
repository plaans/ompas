(begin

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
        (:body nil)
    )
    (def-method pick_and_drop
        (:task place)
        (:params (?o carriable) (?r room))
        (:pre-conditions (!= (pos ?o) ?r) (!= (pos ?o) robby))
        (:body
            (do 
                (define ?a (pos ?o))
                (go2 ?a)
                (define ?g (arbitrary (instances gripper)))
                (pick ?o ?a ?g)
                (go2 ?r)
                (drop ?o ?r ?g))))


    (def-method move_and_drop
      (:task place)
        (:params (?o carriable) (?r room) (?g gripper))
        (:pre-conditions (!= (pos ?o) ?r) (= (carry ?g) ?o))
        (:body
            (do
                (go2 ?r)
                (drop ?o ?r ?g))))
)