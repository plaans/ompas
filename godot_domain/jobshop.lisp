(begin
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
                    (define machines (instance machine))
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
                (__l_available_robots__ (instance robot))))))

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
                (__lambda__ (instance machine))))))

    (def-lambda '(take_first 
        (lambda (seq)
            (if (null? seq)
                nil
                (cons (caar seq) (take_first (cdr seq)))))))

    (def-task t_process_package '(?p package))

    (def-method m_process_to_do_r
        '((:task t_process_package)
            (:params (?p package))
            (:pre-conditions (check (!= (package.processes_list ?p) nil)))
            (:score 0)
            (:body
                (do
                    (define ?m
                        (arbitrary (find_machines_for_process
                            (caar (unzip (package.processes_list ?p))))))
                    (t_process_on_machine ?p ?m)
                    (t_process_package ?p)))))                         

    (def-method m_no_more_process
        '((:task t_process_package)
            (:params (?p package))
            (:pre-conditions (check (= (package.processes_list ?p) nil)))
            (:score 0)
            (:body
                (do
                    (print "package process of " ?p " is done")
                    (mutex::lock-in-list-and-do (instance robot) 15
                        (t_carry_to_machine r ?p (find_output_machine))
                    )))))
    
    (def-task t_process_on_machine '(?p package) '(?m machine))
    (def-method m_process_on_machine
        '((:task t_process_on_machine)
        (:params (?p package) (?m machine))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (mutex::lock-in-list-and-do (instance robot) 11
                    (t_carry_to_machine r ?p ?m))
                (monitor `(= (package.location ,?p) (machine.output_belt ,?m)))))))

    ; (def-method m_no_robot_available
    ;     '((:task t_process_on_machine)
    ;     (:params (?p package) (?m machine))
    ;     (:pre-conditions (check (= (available_robots) nil)))
    ;     (:score 0)
    ;     (:body (do
    ;             (monitor `(!= (available_robots) nil))
    ;             (print "robots are available again")
    ;             (t_process_on_machine)))))

    (def-task t_position_robot_to_belt '(?r robot) '(?b belt))
    (def-method m_position_robot_to_belt
        '((:task t_position_robot_to_belt)
            (:params (?r robot) (?b belt))
            (:pre-conditions true)
            (:score 0)
            (:body (do
                (navigate_to_area ?r (car (belt.interact_areas ?b)))
                (face_belt ?r ?b 5)))))

    (def-task t_carry_to_machine '(?r robot) '(?p package) '(?m machine))
    (def-method m_carry_to_machine
        '((:task t_carry_to_machine)
            (:params (?r robot) (?p package) (?m machine))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (t_take_package ?r ?p)
                    (t_deliver_package ?r ?m)))))

    (def-task t_take_package '(?r robot) '(?p package))
    (def-method m_take_package
        '((:task t_take_package)
          (:params (?r robot) (?p package))
          (:pre-conditions true)
          (:score 0)
          (:body (do
            (t_position_robot_to_belt ?r (package.location ?p))
            (pick_package ?r ?p)))))

    (def-task t_deliver_package '(?r robot) '(?m machine))
    (def-method m_deliver_package
        '((:task t_deliver_package)
            (:params (?r robot) (?m machine))
            (:pre-conditions true)
            (:score 0)
            (:body
                (let ((?b (machine.input_belt ?m)))
                    (do
                        (t_position_robot_to_belt ?r ?b)
                        (monitor `(< (len (belt.packages_list ,?b)) (len (belt.cells ,?b))))
                        (place ?r))))))

    (def-task t_charge '(?r robot))
    (def-method m_charge
        '((:task t_charge)
            (:params (?r robot))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (go_charge ?r)
                    (monitor `(= (robot.battery ,?r) 1))))))

    (def-task t_check_battery '(?r robot))
    (def-method m_check_battery
         '((:task t_check_battery)
          (:params (?r robot))
          (:pre-conditions true)
          (:score 0)
          (:body
             (loop
                 (do
                     (monitor `(< (robot.battery ,?r) 0.4))
                     (mutex::lock-and-do ?r 50
                        (do
                            (go_charge ?r)
                            (monitor `(> (robot.battery ,?r) 0.9)))))))))
    (def-task t_check_rob_bat)
    (def-lambda '(async_check_battery
        (lambda (?r) (async (t_check_battery ?r)))))
    
    (def-method m_check_initial_robots_batteries
        '((:task t_check_rob_bat)
        (:params)
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define list_robots (instance robot))
                (mapf async_check_battery list_robots)
                (print "launched all tasks for initial robots")
                (t_check_batteries_new_robots list_robots)))))

    (def-task t_check_batteries_new_robots '(?l list))
    (def-method m_check_oncoming_robots
        '((:task t_check_batteries_new_robots)
        (:params (?l list))
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (monitor `(> (len (instance robot)) ,(len ?l)))
                (define new_list_robots (instance robot))
                (define l_new_robots (sublist new_list_robots (len ?l)))
                (print "new robots:" l_new_robots)
                (mapf async_check_battery l_new_robots)
                (t_check_batteries_new_robots new_list_robots)))))

    (def-lambda '(async_process_package
            (lambda (?p) (async (t_process_package ?p)))))


    (def-task t_process_packages)
    (def-method m_process_initial_packages
            '((:task t_process_packages)
            (:params)
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (define list_packages (instance package))
                    (mapf async_process_package list_packages)
                    (t_process_new_packages list_packages)
                    ))))

        (def-task t_process_new_packages '(?l list))
        (def-method m_check_oncoming_packages
            '((:task t_process_new_packages)
            (:params (?l list))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (monitor `(>  (len (instance package)) ,(len ?l)))
                    (define new_lp (instance package))
                    (define l_new_p (sublist new_lp (len ?l)))
                    (print "new packages:" l_new_p)
                    (mapf async_process_package l_new_p)
                    (t_process_new_packages new_lp)))))

        (def-task t_jobshop)
        (def-method m1
            '((:task t_jobshop)
                (:params )
                (:pre-conditions true)
                (:score 0)
                (:body
                    (do
                        (define f1 (async (t_process_packages)))
                        (define f2 (async (t_check_rob_bat)))
                        (await f1)
                        (await f2)))))
)