(begin
    (def-initial-state '((robots . ())(machines . ())(packages . ())))
    (def-lambda '(go_random (lambda (?r ?l ?u)
                            (let ((x (rand-int-in-range ?l ?u))
                                  (y (rand-int-in-range ?l ?u)))
                                  (rae-await (navigate_to ?r x y))))))

    (def-action pick ?r)
    (def-action pick_package ?r ?p)
    (def-action place ?r)
    (def-action do_move ?r ?a ?s ?d)
    (def-action navigate_to ?r ?x ?y)
    (def-action navigate_to_cell ?r ?cx ?cy)
    (def-action navigate_to_area ?r ?area)
    (def-action go_charge ?r)
    (def-action do_rotation ?r ?a ?w)
    (def-action face_belt ?belt_name ?speed)
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
    (def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin
        (rae-await (navigate_to ?r ?x ?y))
        (rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))
    (def-method-parameters m_navigate_to '(() ( _ nil)))
    (def-task t_dumber ?r)
    (def-method m_dumber '((:task t_dumber)
        (:params ?r )
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
    ;(def-lambda '(available_robots (lambda ...)))
    (def-method m_process_package
        '((:task t_process_package)
        (:params ?p)
        (:body (mapf t_process_on_machine (enumerate (list ?p) (car (unzip (package.process_list ?p))))))))
    (def-method m_process_on_machine
        '((:task t_process_on_machine)
        (:params ?p ?m)
        (:body (let ((robot (car (available_robots))))
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
                        (rae-await (place ?r)))
                    nil))))))
)