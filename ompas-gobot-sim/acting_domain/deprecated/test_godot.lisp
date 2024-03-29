(begin
    (def-lambda '(go_random (lambda (?r ?l ?u)
                                (let ((x (rand-int-in-range ?l ?u))
                                      (y (rand-int-in-range ?l ?u)))
                                      (navigate_to ?r x y)))))

    (def-task t_dumber ?r)
    (def-method m_dumber
        '((:task t_dumber)
          (:params (?r robot))
          (:pre-conditions (> (robot.battery ?r) 0.4))
          (:effects nil)
          (:score 0)
          (:body (begin
                     (loop
                         (mutex::lock-and-do ?r (go_random ?r 2 5)))))))


    (def-task t_check_battery ?r)
    (def-method m_check_battery
     '((:task t_check_battery)
      (:params (?r robot))
      (:pre-conditions true)
      (:effects nil)
      (:score 0)
      (:body
         (loop
             (begin
                 (monitor `(< (robot.battery ,?r) 0.4))
                 (mutex::lock-and-do ?r
                    (begin
                        (go_charge ?r)
                        (monitor `(> (robot.battery ,?r) 0.9)))))))))
     (def-task test ?r)
     (def-method m_test
        '((:task test)
            (:params (?r robot))
            (:pre-conditions true)
            (:effects nil)
            (:score 0)
            (:body
                (let ((f1 (async (t_check_battery ?r)))
                      (f2 (async (t_dumber ?r))))
                     (begin
                        (await f1)
                        (await f2))))))

)