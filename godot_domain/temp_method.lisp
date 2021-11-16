
(def-task t_process_package ?p)
(def-task t_process_on_machine ?p ?m)
;robot ?r takes the package ?p and place it a the machine ?m
(def-task t_pick_and_place ?r ?p ?m)
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
(def-method m_process_package 
    '((:task t_process_package)
    (:params ?p)
    (:body (mapf t_process_on_machine (enumerate (list ?p) (car (unzip (package.process_list ?p))))))))
(def-method m_process_on_machine 
    '(:task t_process_on_machine)
    (:params ?p ?m)
    (:body (let ((?r (car (available_robots))))
            (begin
                (mutex.lock ?r)
                (t_carry_to_machine ?r ?p ?m)
                (mutex.release ?r)))))
(def-method m_pick_and_place
    '((:task t_pick_and_place)
     (:params ?r ?p ?m)
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
                    (rae-await (place ?r))
                    nil)
                nil))))))

(def-task t_position_robot_to_belt ?r ?b)
(def-method m_position_robot_to_belt 
    '((:task t_position_robot_to_belt)
        (:params ?r ?b)
        (:body (begin 
            (rae-await (navigate_to_area ?r (car (belt.interact_areas ?b))))
            (rae-await (face_belt ?r ?b 5))))))

(def-task t_carry_to_machine ?r ?p ?m)
(def-method m_carry_to_machine 
    '((:task t_carry_to_machine)
        (:params ?r ?p ?m)
        (:body (begin
            (if (< (robot.battery 0.4))
                (t_charge ?r))
            (t_take_package ?r ?p)
            (t_deliver_package ?r ?m)
            nil))))

(def-task t_take_package ?r ?p)
(def-method m_take_package    
    '((:task t_take_package)
      (:params ?r ?p)
      (:body (begin
        (t_position_robot_to_belt ?r (package.location ?p))
        (rae-await (pick_package ?r ?p))
        nil))))

(def-task t_deliver_package ?r ?m)
(def-method m_deliver_package 
    '((:task t_deliver_package)
        (:params ?r ?m)
        (:body (begin
            (t_position_robot_to_belt ?r (machine.input_belt ?m))
            (rae-await (place ?r))
            nil))))

(def-task t_charge ?r)
(def-method m_charge
    '((:task t_charge)
        (:params ?r)
        (:body 
            (begin 
                (rae-await (go_charge ?r))
                (wait-on (= (robot.battery ?r) 1))
                nil))))

(def-lambda 
    '(find_machines_for_process 
        (lambda (?process)
            (begin    
                (define __process__
                    (lambda (?p list)
                        (if (null? list)
                            nil
                            (if (contains (machine.processes_list (car list)) ?p)
                                (cons (car list) (__process__ ?p (cdr list)))
                                (__process__ (cdr list))))))
                (__process__ ?process (get-map (get-state) machines)))))))



    (define find_machines_for_process
            (lambda (?process)
                (begin
                    (define __process__
                        (lambda (?p seq)
                            (if (null? seq)
                                nil
                                (if (contains (get-map (rae-get-state) (list machine.processes_list (car seq))) ?p)
                                    (cons (car seq) (__process__ ?p (cdr seq)))
                                    (__process__ ?p (cdr seq))))))
                    (__process__ ?process (get-map (rae-get-state) machines)))))
                    
(find_machines_for_process 3)


(define take_first 
    (lambda seq 
        (if (null? seq)
            nil
            (cons (caar seq) (take_first (cdr seq))))))

(take_first '((machine2) (machine0) (machine1) (machine3) (machine5) (machine4)))


(def-lambda '(find_output_machine 
    (lambda nil
        (begin 
            (define __lambda__ 
                (lambda seq
                    (if null? seq)
                        nil
                        (if (= (machine.type (car seq)) output_machine)
                            (car seq)
                            (__lambda__ (cdr seq)))))
            (__lambda__ (rae-get-state-variable machines))))))

(define find_output_machine 
    (lambda nil
        (begin 
            (define __lambda__ 
                (lambda seq
                    (if (null? seq)
                        nil
                        (if (= (get-map (rae-get-state) (list machine.type (car seq))) output_machine)
                            (car seq)
                            (__lambda__ (cdr seq))))))
            (__lambda__ (get-map (rae-get-state) machines)))))

(find_output_machine)


(begin
    (rae-trigger-task t_check_battery robot0)
    (rae-trigger-task t_check_battery robot1)
    (rae-trigger-task t_process_package package0)
    (rae-trigger-task t_process_package package1)
    (rae-trigger-task t_process_package package2)
    (rae-trigger-task t_process_package package3)
    (rae-trigger-task t_process_package package4)
    (rae-trigger-task t_process_package package5))
