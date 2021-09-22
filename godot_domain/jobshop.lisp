(begin
    (def-initial-state '((robots . ())(machines . ())(packages . ())))
    (def-lambda '(go_random (lambda (?r ?l ?u)
                            (let ((x (rand-int-in-range ?l ?u))
                                  (y (rand-int-in-range ?l ?u)))
                                  (navigate_to ?r x y)))))

    (def-lambda
        '(find_machines_for_process
            (lambda (?process)
                (begin
                    (define __process__
                        (lambda (?p seq)
                            (if (null? seq)
                                nil
                                (if (contains (machine.processes_list (car seq)) ?p)
                                    (cons (car seq) (__process__ ?p (cdr seq)))
                                    (__process__ ?p (cdr seq))))))
                    (define machines (rae-get-state-variable machines))
                    (define result (__process__ ?process machines))
                    result))))
    (def-lambda '(available_robots
        (lambda nil
            (begin
                (define __l_available_robots__
                    (lambda args
                        (if (null? args)
                            nil
                            (if (not (locked? (car args)))
                                (cons (car args) (__l_available_robots__ (cdr args)))
                                (__l_available_robots__ (cdr args))))))
                (__l_available_robots__ (rae-get-state-variable robots))))))

    (def-lambda '(find_output_machine 
        (lambda nil
            (begin 
                (define __lambda__ 
                    (lambda seq
                        (if (null? seq)
                            nil
                            (if (= (machine.type (car seq)) output_machine)
                                (car seq)
                                (__lambda__ (cdr seq))))))
                (__lambda__ (rae-get-state-variable machines))))))

    (def-lambda '(take_first 
        (lambda seq 
            (if (null? seq)
                nil
                (cons (caar seq) (take_first (cdr seq)))))))

    (def-task t_navigate_to ?r ?x ?y)

    (def-method m_navigate_to '((:task t_navigate_to)
                (:params ?r ?x ?y)
                (:pre-conditions true)
                (:effects nil)
                (:parameters-generator nil true)
                (:score-generator 0)
                (:body 
                (begin
        (navigate_to ?r ?x ?y)))))
    (def-task t_dumber ?r)
    (def-method m_dumber
        '((:task t_dumber)
          (:params ?r)
          (:pre-conditions true)
          (:effects nil)
          (:parameters-generator nil true)
          (:score-generator 0)
          (:body (begin
                     (loop
                         (mutex::lock-and-do ?r (go_random ?r 2 5)))))))


    (def-task t_process_package ?p)
    (def-task t_process_on_machine ?p ?m)
    ;robot ?r takes the package ?p and place it a the machine ?m
    (def-task t_pick_and_place ?r ?p ?m)

    (def-method m_process_package
        '((:task t_process_package)
            (:params ?p)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
        (:body 
        (begin 
            (mapf t_process_on_machine
                (begin
                    (define list_machines
                        (mapf find_machines_for_process
                            (car (unzip (package.processes_list ?p)))))
                    ;(print "list_machines:" list_machines)
                    (enumerate (list ?p) (take_first list_machines))))

            (let ((?r (rand-element (rae-get-state-variable robots))))
                (begin
                    (mutex::lock-and-do ?r
                        (t_carry_to_machine ?r ?p (find_output_machine))
                    )))))))
    (def-method m_process_on_machine
        '((:task t_process_on_machine)
        (:params ?p ?m)
        (:pre-conditions true)
        (:effects nil)
        (:parameters-generator nil true)
        (:score-generator 0)
        (:body (let ((?r (rand-element (rae-get-state-variable robots))))
                (begin
                    (mutex::lock-and-do ?r
                        (t_carry_to_machine ?r ?p ?m))
                    (wait-on `(= (package.location ,?p) (machine.output_belt ,?m))))))))


    (def-method m_pick_and_place
        '((:task t_pick_and_place)
         (:params ?r ?p ?m)
         (:pre-conditions true)
         (:effects nil)
         (:parameters-generator nil true)
         (:score-generator 0)
         (:body (begin
            ;check that the location of the package is
            (let ((?l (package.location ?p )))
                (if (!= (belt.instance ?l) nil)
                    (begin
                        (navigate_to_area ?r (car (belt.interact_areas ?l)))
                        (face_belt ?r ?l)
                        ;pick the right package on the belt
                        (pick_package ?r ?p)
                        (navigate_to_area ?r (car (belt.interact_areas (machine.input_belt ?m))))
                        (face_belt ?r (machine.input_belt ?m))
                        (place ?r))
                    nil))))))

    (def-task t_position_robot_to_belt ?r ?b)
    (def-method m_position_robot_to_belt
        '((:task t_position_robot_to_belt)
            (:params ?r ?b)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
            (:body (begin
                (navigate_to_area ?r (car (belt.interact_areas ?b)))
                (face_belt ?r ?b 5)))))

    (def-task t_carry_to_machine ?r ?p ?m)
    (def-method m_carry_to_machine
        '((:task t_carry_to_machine)
            (:params ?r ?p ?m)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
            (:body
                (begin
                    (t_take_package ?r ?p)
                    (t_deliver_package ?r ?m)))))

    (def-task t_take_package ?r ?p)
    (def-method m_take_package
        '((:task t_take_package)
          (:params ?r ?p)
          (:pre-conditions true)
          (:effects nil)
          (:parameters-generator nil true)
          (:score-generator 0)
          (:body (begin
            (t_position_robot_to_belt ?r (package.location ?p))
            (pick_package ?r ?p)))))

    (def-task t_deliver_package ?r ?m)
    (def-method m_deliver_package
        '((:task t_deliver_package)
            (:params ?r ?m)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
            (:body
                (let ((?b (machine.input_belt ?m)))
                    (begin
                        (t_position_robot_to_belt ?r ?b)
                        (wait-on `(< (length (belt.packages_list ,?b)) (length (belt.cells ,?b))))
                        (place ?r))))))

    (def-task t_charge ?r)
    (def-method m_charge
        '((:task t_charge)
            (:params ?r)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
            (:body
                (begin
                    (go_charge ?r)
                    (wait-on `(= (robot.battery ,?r) 1))))))

    (def-task t_check_battery ?r)
    (def-method m_check_battery
         '((:task t_check_battery)
          (:params ?r)
          (:pre-conditions true)
          (:effects nil)
          (:parameters-generator nil true)
          (:score-generator 0)
          (:body
             (loop
                 (begin
                     (wait-on `(< (robot.battery ,?r) 0.4))
                     (mutex::lock-and-do ?r
                        (begin
                            (go_charge ?r)
                            (wait-on `(> (robot.battery ,?r) 0.9)))))))))
)