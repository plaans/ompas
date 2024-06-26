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
        (:body (sleep 15)))

;    (def-task-om-model t_carry_to_machine
;      (:params (?r robot) (?p package) (?m machine))
;      (:body
;        ;(robot_move ?r (package.location ?p))
;        ;(robot_move ?r (machine.input_belt ?m))
;        (begin
;          (define d1 (package.location ?p))
;          (robot_move ?r d1)
;          (assert 'package.location ?p ?r)
;          (define d2 (machine.input_belt ?m))
;          (robot_move ?r d2)
;          (assert 'package.location ?p d2))))
;
;    (def-task robot_move (:params (?r robot) (?dest location)))
;    (def-task-om-model robot_move
;      (:params (?r robot) (?dest location))
;      (:body
;        (begin
;          (define t_r (robot.location ?r))
;          (define time (travel-time t_r ?dest))
;          (durative-effect time 'robot.location ?r ?dest)
;          )))

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
    (def-task-om-model t_check_battery
            (:params (?r robot))
            (:body nil))
    (def-method m_check_battery
         (:task t_check_battery)
          (:params (?r robot))
          (:pre-conditions true)
          (:score 0)
          (:body
                 (do
                     (wait-for `(< (robot.battery ,?r) 0.4))
                     (charge_robot ?r)
                     (t_check_battery ?r))))

    (def-task charge_robot (:params (?r robot)))
    (def-task-om-model charge_robot (:params (?r robot)) (:body nil))
    (def-method m_charge_robot
        (:task charge_robot)
        (:params (?r robot))
        (:body
            (do
                (define h (acquire ?r '(:priority 1000)))
                (go_charge ?r)
                (wait-for `(> (robot.battery ,?r) 0.9))
                (release h)
            )))

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

    (def-task t_process_package (:params (?p package)))
    (def-method m_process_package
        (:task t_process_package)
        (:params (?p package))
        (:body
            (do
                (define tasks
                    (mapf
                        (lambda (process)
                        `(t_process_on_machine ,?p
                            (arbitrary ',(find_machines_for_process (car process)))
                            ,(cadr process)
                        ))
                  (package.all_processes ?p)))
                (apply seq tasks)
                (t_output_package ?p))))

    (def-task t_process (:params (?m machine) (?p package) (?d int)))
    (def-task-om-model t_process
        (:params (?m machine) (?p package) (?d int))
        (:body
            (sleep ?d)))
;        (durative-effect ?d 'package.location ?p (machine.output_belt ?m))))

    (def-method m_process
        (:task t_process)
        (:params (?m machine) (?p package) (?d int))
        (:body
            (do
                (process ?m ?p)
                (wait-for `(!= (package.location ,?p) ,?m)))))

    (def-event on_new_package
        (:params (?p package))
        (:trigger (once))
        (:body
            (do
                (wait-for `(instance  (package.location ,?p) belt))
                (t_process_package ?p))))

    (def-event on_battery_low
        (:params (?r robot))
        (:trigger (whenever (< (robot.battery ?r) 0.4)))
        (:body (charge_robot ?r)))


;    (def-task t_jobshop)
;    (def-method m1
;       (:task t_jobshop)
;       (:score 0)
;       (:body
;           (do
;               (define f2 (async (t_check_rob_bat)))
;                (define tasks
;                   (mapf (lambda (?p)
;                         `(t_process_package ,?p))
;                       (instances package)))
;               (define h (apply par tasks))
;               )))
)