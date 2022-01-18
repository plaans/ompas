(begin
    (def-action-model pick
        '((:params (?r robot))
          (:pre-conditions (> (robot.battery ?r) 0.4))
          (:effects
              (and
                (at-start ...)
                (at-end ...))
                (assert `(robot.busy ,?r) true))))
    (def-action-operational-model place
        '((:params (?r robot))
          (:body
            (if (> (robot.battery ?r) 0.4)
                (assert `(robot.busy ,?r) false)
                (failure))))))