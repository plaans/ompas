(begin
    (def-task t_position_robot_to_belt (:params (?r robot) (?b belt)))
    (def-method m_position_robot_to_belt (:task t_position_robot_to_belt)
            (:params (?r robot) (?b belt))
            (:pre-conditions true)
            (:score 0)
            (:body (do
                (navigate_to_area ?r (car (belt.interact_areas ?b)))
                (face_belt ?r ?b 5))))

    (def-task t_carry_to_machine (:params (?r robot) (?p package) (?m machine)))
    (def-method m_carry_to_machine
            (:task t_carry_to_machine)
            (:params (?r robot) (?p package) (?m machine))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (t_take_package ?r ?p)
                    (t_deliver_package ?r ?m))))

    (def-task t_take_package (:params (?r robot) (?p package)))
    (def-method m_take_package (:task t_take_package)
          (:params (?r robot) (?p package))
          (:pre-conditions true)
          (:score 0)
          (:body (do
            (t_position_robot_to_belt ?r (package.location ?p))
            (pick_package ?r ?p))))

    (def-task t_deliver_package (:params (?r robot) (?m machine)))
    (def-method m_deliver_package (:task t_deliver_package)
            (:params (?r robot) (?m machine))
            (:pre-conditions true)
            (:score 0)
            (:body
                (let ((?b (machine.input_belt ?m)))
                    (do
                        (t_position_robot_to_belt ?r ?b)
                        (wait-for `(< (len (belt.packages_list ,?b)) (len (belt.cells ,?b))))
                        (place ?r)))))

    (def-task t_charge (:params (?r robot)))
    (def-method m_charge
        (:task t_charge)
            (:params (?r robot))
            (:pre-conditions true)
            (:score 0)
            (:body
                (do
                    (go_charge ?r)
                    (wait-for `(= (robot.battery ,?r) 1)))))

    (def-task t_check_battery (:params (?r robot)))
    (def-method m_check_battery
         (:task t_check_battery)
          (:params (?r robot))
          (:pre-conditions true)
          (:score 0)
          (:body
             (loop
                 (do
                     (wait-for `(< (robot.battery ,?r) 0.4))
                     (define h (acquire ?r '(:priority 1000)))
                     (go_charge ?r)
                     (wait-for `(> (robot.battery ,?r) 0.9))))))
                            
    (def-task t_check_rob_bat)
    (def-method m_check_initial_robots_batteries
        (:task t_check_rob_bat)
        (:params)
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define list_robots (instances robot))
                (define list-h (mapf (lambda (?r) (async (t_check_battery ?r))) list_robots))
                (print "launched all tasks for initial robots")
                (mapf await list-h))))


    (def-task t_process_packages)
    (def-method m_process_initial_packages
        (:task t_process_packages)
        (:params)
        (:pre-conditions true)
        (:score 0)
        (:body
            (do
                (define list_packages (instances package))
                (define list-h (mapf (lambda (?p) (async (t_process_package ?p))) list_packages))
                (mapf await list-h)
                )))

    (def-task t_jobshop)
    (def-method m1
        (:task t_jobshop)
        (:params )
        (:pre-conditions true)
        (:score 0)
        (:body
            (do
                (mapf new-resource (instances robot))
                (mapf new-resource (instances machine))
                (define f1 (async (t_process_packages)))
                (define f2 (async (t_check_rob_bat)))
                (await f1))))
)