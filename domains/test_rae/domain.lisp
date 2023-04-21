(begin

    ;state variables
    (def-state-function a (:result int))

    ;commands
    (def-command c1 (:model (om-model (:body assert 'a 1))))
    (def-command c2 (:model (om-model (:body assert 'a 2))))
    (def-command c3 (:model (om-model (:body assert 'a 3))))
    (def-command c4 (:model (err nil)))
    (def-command c5 (:model (nil)))
    
    ;tasks and methods
    (def-task t1)
    (def-method t1_m1 (:task t1) (:body (do (t11) (t12) )))
    (def-method t1_m2 (:task t1) (:body (t12)))
    
    (def-task t11)
    (def-method t11_m1 (:task t11) (:body (c1)))
    (def-method t11_m2 (:task t11) (:body (c2)))
    
    (def-task t12)
    (def-method t12_m1 (:task t12) (:body (do (t6) (c1))))
    (def-method t12_m2 (:task t12) (:body (c3)))

    (def-task t2)
    (def-method t2_m1 (:task t2) (:body (do (t3) (c2))))
    (def-method t2_m2 (:task t2) (:body (c3)))

    (def-task t3)
    (def-method t3_m1 (:task t3) (:body (c3)))

    (def-task t4)
    (def-method t4_m1 (:task t4) (:body (c4)))
    (def-method t4_m2 (:task t4) (:body (c5)))

    (def-task t6)
    (def-method t6_m1 (:task t6) (:body (c4)))
    (def-method t6_m2 (:task t6) (:body (c5)))

)

    