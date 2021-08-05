
(def-task t_process_package ?p)
(def-task t_process_on_machine ?p ?m)
;robot ?r takes the package ?p and place it a the machine ?m
(def-task t_pick_and_place ?r ?p ?m)
(def-lambda '(available_robots (lambda ...)))
(def-method m_process_package 
    '((:task t_process_package)
    (:params ?p)
    (:body (mapf t_process_on_machine (enumerate (list ?p) (car (unzip (package.process_list ?p))))))))
(def-method m_process_on_machine 
    '(:task t_process_on_machine)
    (:params ?p ?m)
    (:body (let ((robot (car (available_robots)))
            (t_pick_and_place robot ?p ?m)))))
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

(def-lambda '(find_machines_for_process (lambda (?process)
                
                )))

