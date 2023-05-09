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
    (def-task-om-model t_carry_to_machine
        (:params (?r robot) (?p package) (?m machine))
        (:body (sleep 1))
    )
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

    (def-task t_check_battery (:params (?r robot)))
    (def-method m_check_battery
         (:task t_check_battery)
          (:params (?r robot))
          (:pre-conditions true)
          (:score 0)
          (:body
                 (do
                     (wait-for `(< (robot.battery ,?r) 0.5  ))
                     (define h (acquire ?r '(:priority 1000)))
                     (go_charge ?r)
                     (wait-for `(> (robot.battery ,?r) 0.9))
                     (release h)
                     (t_check_battery ?r))))
                            
    (def-task t_check_rob_bat)
    (def-task-om-model t_check_rob_bat
            (:params )
            (:body nil))
    (def-method m_check_initial_robots_batteries
        (:task t_check_rob_bat)
        (:params)
        (:pre-conditions true)
        (:score 0)
        (:body 
            (do
                (define tasks (mapf (lambda (?r) `(t_check_battery ,?r)) (instances robot)))
                (define h (apply par tasks))
                (print "end check batteries")
                )))


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

    (def-task t_process (:params (?m machine) (?p package) (?d int)))
    (def-task-om-model t_process
        (:params (?m machine) (?p package) (?d int))
        (:body (sleep ?d)))
    
    (def-method m_process 
        (:task t_process)
        (:params (?m machine) (?p package) (?d int))
        (:body 
            (do
                (process ?m ?p)
                (wait-for `(!= (package.location ,?p) ,?m))
            )))

    (def-task t_jobshop)

    (def-method m1
       (:task t_jobshop)
       (:score 0)
       (:body
           (do
               (define f2 (async (t_check_rob_bat)))
               (define tasks 
                   (mapf (lambda (?p) 
                       (do
                           (define tasks (mapf (lambda (process)
                               `(t_process_on_machine ,?p 
                                   (arbitrary ',(find_machines_for_process (car process)))
                                   ,(cadr process)
                                   ))
                               (package.all_processes ?p)))
                            (define last_task
                                `(begin
                                    (define ?r (arbitrary (instances robot)))
                                    (define h_r (acquire ?r))
                                    ,(define ?m (find_output_machine))
                                    ,?m
                                    (t_carry_to_machine ?r ,?p ,?m)))
                            (define tasks (append tasks (list last_task)))
                            ;(print tasks)
                            `(apply seq ',tasks)))
                        (instances package)))
               (define h (apply par tasks)))))
)