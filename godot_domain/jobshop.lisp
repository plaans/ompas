(begin
    (def-initial-state '((robots . ())(machines . ())(packages . ())))
    (def-lambda '(go_random (lambda (?r ?l ?u)
                            (let ((x (rand-int-in-range ?l ?u))
                                  (y (rand-int-in-range ?l ?u)))
                                  (rae-await (navigate_to ?r x y))))))

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
                    ;(print "machines:" machines)
                    (define result (__process__ ?process machines))
                    ;(print "result of find_machines_for_process: " result)
                    result))))
    (def-lambda '(available_robots
        (lambda nil
            (begin
                (define __l_available_robots__
                    (lambda args
                        (if (null? args)
                            nil
                            (if (not (mutex.locked? (car args)))
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

    (def-action pick ?r)
    (def-action pick_package ?r ?p)
    (def-action place ?r)
    (def-action do_move ?r ?a ?s ?d)
    (def-action navigate_to ?r ?x ?y)
    (def-action navigate_to_cell ?r ?cx ?cy)
    (def-action navigate_to_area ?r ?area)
    (def-action go_charge ?r)
    (def-action do_rotation ?r ?a ?w)
    (def-action face_belt ?r ?b ?w)
    (def-state-function robot.coordinates ?r)
    (def-state-function robot.instance ?r)
    (def-state-function robot.coordinates_tile ?r)
    (def-state-function robot.battery ?r)
    (def-state-function robot.velocity ?r)
    (def-state-function robot.rotation_speed ?r)
    (def-state-function robot.in_station ?r)
    (def-state-function robot.in_interact_areas ?r)
    (def-state-function machine.instance ?m)
    (def-state-function machine.coordinates ?m)
    (def-state-function machine.coordinates_tile ?m)
    (def-state-function machine.input_belt ?m)
    (def-state-function machine.output_belt ?m)
    (def-state-function machine.processes_list ?m)
    (def-state-function machine.type ?m)
    (def-state-function machine.progress_rate ?m)
    (def-state-function package.instance ?p)
    (def-state-function package.location ?p)
    (def-state-function package.processes_list ?p)
    (def-state-function belt.instance ?b)
    (def-state-function belt.belt_type ?b)
    (def-state-function belt.polygons ?b)
    (def-state-function belt.cells ?b)
    (def-state-function belt.interact_areas ?b)
    (def-state-function belt.packages_list ?b)
    (def-state-function parking_area.instance ?pa)
    (def-state-function parking_area.polygons ?pa)
    (def-state-function parking_area.cells ?pa)
    (def-state-function interact_area.instance ?ia)
    (def-state-function interact_area.polygons ?ia)
    (def-state-function interact_area.cells ?ia)
    (def-state-function interact_area.belt ?ia)
    (def-task t_navigate_to ?r ?x ?y)
    (def-method m_navigate_to '((:task t_navigate_to)
                (:params ?r ?x ?y)
                (:pre-conditions true)
                (:effects nil)
                (:parameters-generator nil true)
                (:score-generator 0)
                (:body 
                (begin
        (rae-await (navigate_to ?r ?x ?y))
        (rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))
    (def-task t_dumber ?r)
    (def-method m_dumber
        '((:task t_dumber)
          (:params ?r)
          (:pre-conditions true)
          (:effects nil)
          (:parameters-generator nil true)
          (:score-generator 0)
          (:body (begin
                (if (not (robot.in_station ?r))
                   (if (<= (robot.battery ?r) 0.4)
                       (let*  ((areas (get-map (rae-get-state) parking_areas))
                               (area (rand-element areas)))
                               (rae-await (navigate_to_area ?r area)))
                       (go_random ?r 2 5))
                   (if (>= (robot.battery ?r) 0.9)
                       (go_random ?r 2 5)))
                (t_dumber ?r)))))
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
                    (print "list_machines:" list_machines)
                    (enumerate (list ?p) (take_first list_machines))))

            (let ((?r (rand-element (available_robots))))
                (begin
                    (mutex.lock ?r)
                    (t_carry_to_machine ?r ?p (find_output_machine))
                    (mutex.release ?r)))))))
    (def-method m_process_on_machine
        '((:task t_process_on_machine)
        (:params ?p ?m)
        (:pre-conditions true)
        (:effects nil)
        (:parameters-generator nil true)
        (:score-generator 0)
        (:body (let ((?r (rand-element (available_robots))))
                (begin
                    (mutex.lock ?r)
                    (t_carry_to_machine ?r ?p ?m)
                    (mutex.release ?r)
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
                        (rae-await (navigate_to_area ?r (car (belt.interact_areas ?l))))
                        (rae-await (face_belt ?r ?l))
                        ;pick the right package on the belt
                        (rae-await (pick_package ?r ?p))
                        (rae-await (navigate_to_area ?r (car (belt.interact_areas (machine.input_belt ?m)))))
                        (rae-await (face_belt ?r (machine.input_belt ?m)))
                        (rae-await (place ?r)))
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
                (rae-await (navigate_to_area ?r (car (belt.interact_areas ?b))))
                (rae-await (face_belt ?r ?b 5))))))

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
                    (if (< (robot.battery ?r) 0.4)
                        (t_charge ?r))
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
            (rae-await (pick_package ?r ?p))))))

    (def-task t_deliver_package ?r ?m)
    (def-method m_deliver_package
        '((:task t_deliver_package)
            (:params ?r ?m)
            (:pre-conditions true)
            (:effects nil)
            (:parameters-generator nil true)
            (:score-generator 0)
            (:body (begin
                (t_position_robot_to_belt ?r (machine.input_belt ?m))
                (rae-await (place ?r))))))

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
                    (rae-await (go_charge ?r))
                    (wait-on `(= (robot.battery ,?r) 1))))))
)