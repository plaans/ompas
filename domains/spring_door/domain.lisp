(begin

    (def-types robot location door d_status r_status d_type)
    (def-objects 
        (closed opened held d_status)
        (spring ordinary d_type)
        (empty h object))

    (def-prob 
        (unlatch (0.8 0.2))
        (unlatch_2 (0.5 0.5))
        (take (1 0))
        (put (1 0))
        (move (0.95 0.05)))


    (def-prob-exec
        (pass_door 
            (begin
                (define d (car args))
            (if (= rae_mode rea_exec)
                (if (= (door_type d) 'spring)
                    (= (door_status d) 'held)
                    (= (door_status d) 'closed))
                (if (= (door_type d) 'unk)
                    (if (= (door_status) 'held)
                        true
                        (if (= (door_status) 'closed)
                            nil
                            (rand-element '(true nil))))
                    (if (= (door_type d) 'spring)
                        (= (door_status d) 'held)
                        (= (door_status d) 'closed)))))))

    ;state functions
    (def-state-function loc (:params (r robot)) (:result location))
    (def-state-function door_status (:params (d door)) (:result d_status))
    (def-state-function load (:params (r robot)) (:result object))
    (def-state-function door_type (:params (d door)) (:result d_type))
    (def-state-function get_door (:params (l1 location) (l2 location)) (:result door))
    (def-state-function pos (:params (o object)) (:result object))
    (def-state-function robot_status (:params (r robot)) (:result r_status))


    ;commands 
    (def-command help_robot
        (:params (r1 robot) (r2 robot))
        (:body 
            (if ( = (loc r1) (loc r2))
                (print r1 " is helping " r2)
                (err nil)
                )))
    
    (def-command unlatch_1
        (:params (r robot) (d door))
        (:plant-model (om-plant-model
            (:body
                (if (!= (door_status d) closed)
                    (print "Door " d " is already open.")
                    (if (! (load r))
                        (begin 
                            (define res (prob-exec 'unlatch_1))
                            (if (res)
                                (begin
                                    (assert `(door_status ,d) opened)
                                    (success "Robot " r " has opened door " d "."))
                                (failure "Unlatching has failed due to an interval error.")))
                        (failure "Robot " r " is not free to open door " d ".")
                        ))))
        )))

    (def-command unlatch_2
        (:params (r robot) (d door))
        (:plant-model (om-plant-model
            (:body
                (if (!= (door_status d) closed)
                    (print "Door " d " is already open.")
                    (if (! (load r))
                        (begin 
                            (define res (prob-exec 'unlatch_2))
                            (if (res)
                                (begin
                                    (print "Robot " r " has opened door " d ".")
                                    (assert `(door_status ,d) opened))
                                (failure "Unlatching has failed due to an interval error.")))
                        (failure "Robot " r " is not free to open door " d ".")
                            ))))))

    (def-command pass_door
        (:params (r robot) (d door) (l location))
        (:plant-model (om-plant-model
           (:body
                (if (or (= (door_status d) opened) (= (door_status d) held))
                    (if (prob-exec 'pass_door d)
                        (begin
                            (assert `(loc r) l)
                            (success "Robot " r " has passed the door " d "."))
                        (failure "Robot " r " is not able to pass the door " d "."))
                    (failure "Robot " r "is not able to pass the door " d ".")))))

    (def-command hold_door
        (:params (r robot) (d door))
        (:plant-model (om-plant-model
            (:body
                (if (and (!=(door_status) closed) (=(load r) empty))
                    (begin
                        (assert `(load ,r) h)
                        (assert `(door_status ,d) held)
                        (success "Robot " r " is holding the door " d "."))
                    (if (=(door_status) closed)
                        (failure "Door " d " is closed and cannot be held by " r ".")
                        (if (! (load r))
                            (failure "Robot " r " is not free to hold the door " r "."))))))))

    (def-command release_door
        (:params (r robot) (d door))
        (:plant-model (om-plant-model
            (:body
                (if (!=(door_status) held)
                    (success)
                    (if (and (= (door_status d) held) (=(load r) h))
                        (begin
                            (assert `(load ,r) 'empty)
                            (assert `(door_status ,d) closed)
                            (success "Robot " r " has released the door " d ".")) 
                        (failure "Robot " r " is not holding the door " d ".")
                        ))))))

    (def-command move
        (:params (r robot) (l1 location) (l2 location))
        (:plant-model (om-plant-model
            (:body
                (if (l1 == l2)
                    (print "Robot " r " is already at location " l2 ".")
                    (if (= (loc r) l1)
                        (if (!= (door_location l1 l2) nil)
                            (failure "Robot " r " cannot move. There is a door between " l1 " and " l2 ".")
                            (if (prob-exec 'move)
                                (begin
                                    (assert `(loc ,r) l2)
                                    (success "Robot " r "has moved from " l1 " to " l2 "."))
                                (failure "Move has failed due to some internal failure.")))
                    
                        (failure "Invalid move by robot " r ".")))))))

    (def-command put 
        (:params (r robot) (o object))
        (:plant-model (om-plant-model
            (:body
                (if ( = (pos o) r)
                    (if (prob-exec 'put)
                        (begin
                            (assert `(pos o) (loc r))
                            (assert `(load r) 'empty)
                            (success "Robot " r " has put object " o " at location" (loc r)))
                        (failure "Put has failed due to some internal failure."))
                    
                    (failure "Object " o " is not with robot " r "."))))))

    (def-command take
        (:params (r robot) (o object))
        (:plant-model (om-plant-model 
            (:body
                (if (= (load r) empty)
                    (if (= (loc r) (pos o))
                        (if (prob-exec 'take)
                            (begin
                                (print "Robot " r " has picked up object " o)
                                (assert `(pos o) r)
                                (assert `(load r) o))
                            (failure "take failed due to some internal error."))
                        (failure "Robot " r " is not at object " o "'s location."))
                    (failure "Robot " r " is not free to take anything."))))))

    ;tasks
    (def-task fetch (:params (r robot) (o object) (l location)))
    (def-task move_to (:params (r robot) (l location)))
    (def-task move_through_doorway (:params (r robot) (d door) (l location)))
    (def-task unlatch (:params (r robot) (d door)))
    (def-task collision (:params (r robot)))

    ;methods
    (def-method fetch_method_1
        (:task fetch)
        (:params (r robot) (o object) (l location))
        (:body 
            (begin
                (define rh (acquire r))
                (move_to r (pos o))
                (take r o)
                (move_to r l)
                (release rh))))

    ;to define with dijkstra algorithms
    (def-method move_to_method_1
        (:task move_to)
        (:params (r robot) (l location))
        (:body
            (begin
                (define x (loc r)))))

    (def-method unlatch_method_1
        (:task unlatch)
        (:params (r robot) (d door))
        (:body 
            (unlatch_1 r d)))

    (def-method unlatch_method_2
        (:task unlatch)
        (:params (r robot) (d door))
        (:body )
            (unlatch_2 r d))

    ;to finish using resources
    (def-method recover_method_1
        (:task collision)
        (:params (r1 robot) (r2 robot))
        (:body nil))

    (def-method move_through_doorway_method_1
        (:task move_through_doorway)
        (:params (r robot) (d door) (l location))
        (:pre-conditions (= (load r) empty) (or ( = (door_type d) 'ordinary) ( = (door_type d) UNK)))
        (:body
            (begin
                (unlatch r d)
                (pass_door r d l))))

    (def-method move_through_doorway_method_2
        (:task move_through_doorway)
        (:params (r robot) (d door) (l location))
        (:pre-conditions (!= (load r) empty) (or ( = (door_type d) 'spring) ( = (door_type d) UNK)))
        (:body
            (do
                (define r2 (arbitray robot))
                (define rh (acquire r2))
                (define obj (load r2))
                (if (!= obj empty)
                    (if (!= obj h)
                        (put r2 (load r2))
                        (err nil)))
                (move_to r2 (loc r))
                (unlatch r2 d)
                (hold_door r2 d)
                (pass_door r d l)
                (release_door r d)
                (release rh))))

    (def-method move_through_doorway_method_3
        (:task move_through_doorway)
        (:params (r robot) (d door) (l location))
        (:pre-conditions (= (load r) empty) (or (= (door_type d) 'spring) (= (door_type d) UNK)))
        (:body
            (begin
                (unlatch r d)
                (hold_door r d)
                (pass_door r d l)
                (release_door r d))))

    (def-method move_through_doorway_method_4
        (:task move_through_doorway)
        (:params (r robot) (d door) (l location))
        (:pre-conditions (!= (load r) empty) (or (= (door_type d) 'ordinary) (= (door_type d) UNK)))
        (:body
            (do
                (define obj (load r))
                (if (!= obj 'h)
                    (put r obj)
                    (failure r " is holding another door."))
                (unlatch r d)
                (take r obj)
                (pass_door r d l))))
    
)