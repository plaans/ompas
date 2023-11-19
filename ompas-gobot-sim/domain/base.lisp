(begin

    ;(def-init (sleep 1))
    ;types declaration
    ;(def-types machine robot package belt (parking_area interact_area area) tile)
    (def-types machine package (robot parking_area belt location) interact_area)
    ;(def-objects (unk location))


    ;globals
    (def-function globals.robot_default_battery_capacity (:result float))
    (def-function globals.robot_battery_charge_rate (:result float))
    (def-function globals.robot_battery_drain_rate (:result float))
    (def-function globals.robot_battery_drain_rate_idle (:result float))
    (def-function globals.robot_standard_velocity (:result float))
    (def-function globals.robot_battery_charge_rate_percentage (:result float))
    (def-function globals.robot_battery_drain_rate_percentage (:result float))
    (def-function globals.robot_battery_drain_rate_idle_percentage (:result float))

    ;synthetic state functions
    ;(def-function travel_distance (:params (?t1 tile) (?t2 tile))(:result float))
    ;(def-state-function location_tile (:params object) (:result tile))
    (def-function travel-time (:params (?l1 location) (?l2 location)) (:result float))

    ;state function declaration
    ;robot state functions
    (def-state-function robot.coordinates (:params (?r robot)) (:result (tuple int int)))
    (def-function robot.instance (:params (?r robot)) (:result symbol))
    ;(def-state-function robot.coordinates_tile (:params (?r robot)) (:result (tuple int int)))
    (def-state-function robot.battery (:params (?r robot)) (:result float))
    (def-state-function robot.velocity (:params (?r robot)) (:result (tuple float float)))
    (def-state-function robot.rotation_speed (:params (?r robot)) (:result float))
    (def-state-function robot.in_station (:params (?r robot)) (:result boolean))
    (def-state-function robot.in_interact_areas (:params (?r robot)) (:result (list interact_area)))
    (def-function robot.default_velocity (:params (?r robot)) (:result int))
    (def-function robot.drain_rate (:params (?r robot)) (:result float))
    (def-function robot.recharge_rate (:params (?r robot)) (:result float))
    (def-state-function robot.location (:params (?r robot)) (:result location))

    ;machine state functions
    (def-function machine.instance (:params (?m machine)) (:result  symbol))
    (def-function machine.coordinates (:params (?m machine)) (:result (tuple int int)))
    ;(def-function machine.coordinates_tile (:params (?m machine)) (:result (tuple int int)))
    (def-function machine.input_belt (:params (?m machine)) (:result belt))
    (def-function machine.output_belt (:params (?m machine)) (:result belt))
    (def-function machine.processes_list (:params (?m machine)) (:result (list int)))
    (def-function machine.type (:params (?m machine)) (:result symbol))
    (def-state-function machine.progress_rate (:params (?m machine)) (:result float))
    (def-state-function machine.package_processed (:params (?m machine)) (:result package))
    
    ;package state function
    (def-function package.instance (:params (?p package)) (:result symbol))
    (def-state-function package.location (:params (?p package)) (:result location))
    (def-state-function package.processes_list (:params (?p package)) (:result (list (tuple int float))))
    (def-function package.all_processes (:params (?p package)) (:result (list (tuple int float))))
    (def-state-function package.closest_interact_area (:params (?p package)) (:result interact_area))
    
    ;belt state function
    (def-function belt.instance (:params (?b belt)) (:result symbol))
    (def-function belt.belt_type (:params (?b belt)) (:result symbol))
    (def-function belt.polygons (:params (?b belt)) (:result (list (tuple int int))))
    (def-function belt.cells (:params (?b belt)) (:result (list (tuple int int))))
    (def-function belt.interact_areas (:params (?b belt)) (:result (list interact_area)))
    (def-state-function belt.packages_list (:params (?b belt)) (:result (list package)))
    
    ;parking_area state functions
    (def-function parking_area.instance (:params (?pa parking_area)) (:result symbol))
    (def-function parking_area.polygons (:params (?pa parking_area)) (:result (list (tuple int int))))
    (def-function parking_area.cells (:params (?pa parking_area)) (:result (list (tuple int int))))
    
    ;interact_areas state functions
    (def-function interact_area.instance (:params (?pa parking_area)) (:result symbol))
    (def-function interact_area.polygons (:params (?pa parking_area)) (:result (list (tuple int int))))
    (def-function interact_area.cells (:params (?pa parking_area)) (:result (list (tuple int int))))
    (def-function interact_area.belt (:params (?pa parking_area)) (:result belt))

    ;command definition
    (def-command process (:params (?m machine) (?p package)))
    (def-command pick (:params (?r robot)))
    (def-command pick_package (:params (?r robot) (?p package)))
    (def-command place (:params (?r robot)))
    (def-command do_move (:params (?r robot) (?a float) (?s float) (?d float)))
    (def-command navigate_to (:params (?r robot) (?x float) (?y float)))
    (def-command navigate_to_cell (:params (?r robot) (?cx int) (?cy int)))
    (def-command navigate_to_area (:params (?r robot) (?area object)))
    (def-command go_charge (:params (?r robot)))
    (def-command do_rotation (:params (?r robot) (?a float) (?w float)))   
    (def-command face_belt (:params (?r robot) (?b belt) (?w float)))
    (def-command rotate_to (:params (?r robot) (?a float) (?w float)))

    ;Lambdas definition
    (def-lambda go_random (lambda (?r ?l ?u)
                            (let ((x (rand-int-in-range ?l ?u))
                                  (y (rand-int-in-range ?l ?u)))
                                  (navigate_to ?r x y))))

    (def-lambda
        find_machines_for_process
            (lambda (?process)
                (begin
                    (define __process__
                        (lambda (?p seq)
                            (if (null? seq)
                                nil
                                (if (contains (machine.processes_list (car seq)) ?p)
                                    (cons (car seq) (__process__ ?p (cdr seq)))
                                    (__process__ ?p (cdr seq))))))
                    (define machines (instances machine))
                    (define result (__process__ ?process machines))
                    result)))


    (def-lambda available_robots
        (lambda nil
            (begin
                (define __l_available_robots__
                    (lambda (l)
                        (if (null? l)
                            nil
                            (if (not (locked? (car l)))
                                (cons (car l) (__l_available_robots__ (cdr l)))
                                (__l_available_robots__ (cdr l))))))
                (__l_available_robots__ (instances robot)))))

    (def-lambda find_output_machine 
        (lambda nil
            (begin 
                (define __lambda__ 
                    (lambda (seq)
                        (if (null? seq)
                            nil
                            (if (= (machine.type (car seq)) output_machine)
                                (car seq)
                                (__lambda__ (cdr seq))))))
                (__lambda__ (instances machine)))))

    (def-lambda take_first 
        (lambda (seq)
            (if (null? seq)
                nil
                (cons (caar seq) (take_first (cdr seq))))))
    
)