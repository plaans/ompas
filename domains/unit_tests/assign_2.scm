(begin
    (def-state-function value (:result int))
    (def-state-function value_2 (:result int))
    (def-function constant (:result int))
    (def-objects (robot0 robot))
    (def-resources robot0)
    (def-facts (value 1) (value_2 2))
    (def-static-facts (constant 2))

    (def-task test (:params (?a int) (?b int)))
    (def-task-om-model
        test
        (:params (?a int) (?b int))
        (:body
            (do
                (sleep ?a)
                (define x (+ (value_2) (constant)))
                (sleep ?b)
                (assert 'value x))))

    (plan test 1 2)
    (exit 0)
)