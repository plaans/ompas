(begin
    (read godot_domain/jobshop_advanced.lisp)

    
    (def-task t_position_robot_to_belt '(?r robot) '(?b belt))
    (def-method m_position_robot_to_belt
        '((:task t_position_robot_to_belt)
            (:params (?r robot) (?b belt))
            (:pre-conditions true)
            (:score 0)
            (:body (do
                (await (navigate_to_area ?r (car (belt.interact_areas ?b))))
                (await (face_belt ?r ?b 5))))))

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
            (await (pick_package ?r ?p))))))

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
                        (await (wait-for `(< (len (belt.packages_list ,?b)) (len (belt.cells ,?b)))))
                        (await (place ?r)))))))

    (def-task t_charge '(?r robot))
    (def-method m_charge
        '((:task t_charge)
            (:params (?r robot))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (await (go_charge ?r))
                    (await (wait-for `(= (robot.battery ,?r) 1)))))))

    (def-task t_check_battery '(?r robot))
    (def-method m_check_battery
         '((:task t_check_battery)
          (:params (?r robot))
          (:pre-conditions true)
          (:score 0)
          (:body
             (loop
                 (do
                     (await (wait-for `(< (robot.battery ,?r) 0.4)))
                     (mutex::lock-and-do ?r 50
                        (do
                            (await (go_charge ?r))
                            (await (wait-for `(> (robot.battery ,?r) 0.9))))))))))
                            
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
                (define h (mapf async_check_battery list_robots))
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
                (await (wait-for `(> (len (instance robot)) ,(len ?l))))
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
                    ;(define h (async (t_process_package (car list_packages))))
                    (define h (mapf async_process_package list_packages))
                    ;(await (car h))
                    (t_process_new_packages list_packages)
                    (await h)
                    ))))

        (def-task t_process_new_packages '(?l list))
        (def-method m_check_oncoming_packages
            '((:task t_process_new_packages)
            (:params (?l list))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (await (wait-for `(>  (len (instance package)) ,(len ?l))))
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