(begin
    (def-state-function value (:result float))
    (def-command increase (:params ))
    (def-command-om-model increase
        (:params )
        (:body
            (begin
                (define new (value))
                (assert 'value new)
            )))

    (def-facts (value 0))
    (new-goal-task increase)
    (plan)
    (exit 0)
)



