(begin
    (def-types count)

    (def-state-function counter
        (:params )
        (:result int))

    (def-task c_test)
    (def-task-om-model c_test
        (:params )
        (:body
            (do
                (assert 'counter (+ (counter) 1))
            )))

    (def-objects (c1 c2 count))
    (new-event 1 'counter 2)

    ;(def-facts
    ;    (counter 2)
    ;)
    ;(new-timed-goal-task 1 c_test); works because start timepoint is fixed
    (new-goal-task c_test) ; does not work
    (print (get-goals-events))
    (plan)
    (exit 0)
)