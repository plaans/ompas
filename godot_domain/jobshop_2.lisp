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
                    (define machines (machines))
                    (define result (__process__ ?process machines))
                    result))))
    (def-lambda '(available_robots
        (lambda nil
            (begin
                (define __l_available_robots__
                    (lambda (l)
                        (if (null? l)
                            nil
                            (if (not (locked? (car l)))
                                (cons (car l) (__l_available_robots__ (cdr l)))
                                (__l_available_robots__ (cdr l))))))
                (__l_available_robots__ (robots))))))

    (def-lambda '(find_output_machine 
        (lambda nil
            (begin 
                (define __lambda__ 
                    (lambda (seq)
                        (if (null? seq)
                            nil
                            (if (= (machine.type (car seq)) output_machine)
                                (car seq)
                                (__lambda__ (cdr seq))))))
                (__lambda__ (machines))))))

    (def-lambda '(take_first 
        (lambda (seq)
            (if (null? seq)
                nil
                (cons (caar seq) (take_first (cdr seq)))))))


    (def-task t_process_package ?p)

    (def-method m_process_to_do_r
        '((:task t_process_package)
            (:params (?p package))
            (:pre-conditions (!= (package.processes_list ?p) nil))
            (:effects nil)
            (:score 0)
            (:body
                (begin
                    (define ?m
                        (arbitrary (find_machines_for_process
                            (caar (unzip (package.processes_list ?p))))))
                    (t_process_on_machine ?p ?m)
                    (t_process_package ?p)))))                         

    (def-method m_no_more_process
        '((:task t_process_package)
            (:params (?p package))
            (:pre-conditions (= (package.processes_list ?p) nil))
            (:effects nil)
            (:score 0)
            (:body
                (let ((?r (arbitrary (available_robots) rand-element)))
                    (begin
                        (mutex::lock-and-do ?r 10
                            (t_carry_to_machine ?r ?p (find_output_machine))
                        ))))))
    
    (def-task t_process_on_machine ?p ?m)
    (def-method m_robot_available
        '((:task t_process_on_machine)
        (:params (?p package) (?m machine))
        (:pre-conditions (!= (available_robots) nil))
        (:effects nil)
        (:score 0)
        (:body (let ((?r (arbitrary (available_robots) rand-element)))
                (if (!= ?r nil)
                    (begin
                        (mutex::lock-and-do ?r 11
                            (t_carry_to_machine ?r ?p ?m))
                        (wait-on `(= (package.location ,?p) (machine.output_belt ,?m))))
                    (t_process_on_machine))))))

    (def-method m_no_robot_available
        '((:task t_process_on_machine)
        (:params (?p package) (?m machine))
        (:pre-conditions (= (available_robots) nil))
        (:effects nil)
        (:score 0)
        (:body (begin
                (wait-on `(!= (available_robots) nil))
                (t_process_on_machine)))))

    (def-task t_position_robot_to_belt ?r ?b)
    (def-method m_position_robot_to_belt
        '((:task t_position_robot_to_belt)
            (:params (?r robot) (?b belt))
            (:pre-conditions true)
            (:effects nil)
            (:score 0)
            (:body (begin
                (navigate_to_area ?r (car (belt.interact_areas ?b)))
                (face_belt ?r ?b 5)))))

    (def-task t_carry_to_machine ?r ?p ?m)
    (def-method m_carry_to_machine
        '((:task t_carry_to_machine)
            (:params (?r robot) (?p package) (?m machine))
            (:pre-conditions true)
            (:effects nil)
            (:score 0)
            (:body
                (begin
                    (t_take_package ?r ?p)
                    (t_deliver_package ?r ?m)))))

    (def-task t_take_package ?r ?p)
    (def-method m_take_package
        '((:task t_take_package)
          (:params (?r robot) (?p package))
          (:pre-conditions true)
          (:effects nil)
          (:score 0)
          (:body (begin
            (t_position_robot_to_belt ?r (package.location ?p))
            (pick_package ?r ?p)))))

    (def-task t_deliver_package ?r ?m)
    (def-method m_deliver_package
        '((:task t_deliver_package)
            (:params (?r robot) (?m machine))
            (:pre-conditions true)
            (:effects nil)
            (:score 0)
            (:body
                (let ((?b (machine.input_belt ?m)))
                    (begin
                        (t_position_robot_to_belt ?r ?b)
                        (wait-on `(< (length (belt.packages_list ,?b)) (length (belt.cells ,?b))))
                        (place ?r))))))

    (def-task t_charge ?r)
    (def-method m_charge
        '((:task t_charge)
            (:params (?r robot))
            (:pre-conditions true)
            (:effects nil)
            (:score 0)
            (:body
                (begin
                    (go_charge ?r)
                    (wait-on `(= (robot.battery ,?r) 1))))))

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
                     (wait-on `(< (robot.battery ,?r) 0.4))
                     (mutex::lock-and-do ?r 50
                        (begin
                            (go_charge ?r)
                            (wait-on `(> (robot.battery ,?r) 0.9)))))))))
)

(define eval-pre-conditions
(lambda args
    (begin
        (print locked?)
        (eval (cons (get-preconditions (car args)) (cdr args))))))