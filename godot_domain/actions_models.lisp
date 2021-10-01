(begin
    (def-action-model pick
        '((:params ?r)
          (:pre-conditions (> (robot.battery ?r) 0.4))
          (:effects
                (assert (robot.busy ?r) true))))
    (def-action-operational-model place
        '((:params ?r)
          (:body
            (if (> (robot.battery ?r) 0.4)
                (assert (robot.busy ?r) false)
                (failure))))))