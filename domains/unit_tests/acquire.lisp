(begin
    (def-types toy)

    (def-state-function sf_test
        (:params (?t toy))
        (:result boolean))
    (def-state-function counter
        (:params (?t toy))
        (:result int))

    (def-state-function value
        (:result int))

    (def-command c_test (:params (?t toy)))
    (def-command c_test_int (:params (?i int)))
    (def-task test)
    (def-method m_test
        (:task test)
        (:params)
        (:pre-conditions true)
        (:body
            (do
                (define ?t (arbitrary (instances 'toy)))
                (define rh (acquire ?t))
                (define new (+ (counter ?t) 1))
                (assert 'counter ?t new)
                (c_test ?t)
            )
        )
    )

    (def-objects (t1 t2 toy))
    (def-resources t1 t2)
    (def-facts
        (value 0)
        ((counter t1) 2)
        ((counter t2) 0)
        ((sf_test t1) false)
        ((sf_test t2) false)
    )
    ;(new-timed-goal-task 1 test)
    (plan test)
    (exit 0)
)