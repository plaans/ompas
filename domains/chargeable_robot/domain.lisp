
(begin
    (read ../common.lisp)

    (def-types location position charger)

    (def-objects (c1 charger))

    ;state functions
    (def-state-function load (:params (r robot)) (:result object))
    (def-state-function charge (:params (r robot)) (:result int))
    (def-state-function loc (:params (r robot)) (:result location))
    (def-state-function pos (:params (o object)) (:result position))
    (def-state-function emergency_handling (:params (r robot)) (:result bool))
    (def-state-function view (:params (l location)) (:result bool))
    (def-function containers (:params (l location)) (:result (list object)))


    (def-prob
        (take (0.9 0.1))
        (put (0.99 0.01))
        (charge (0.9 0.1))
        (move_to_emergency (0.99 0.01))
        (move (0.95 0.05))
        (address_emergency (0.98 0.02)))

    (def-env sense_objects 
        (lambda nil
            (begin
                (define total 
                    (begin
                        (define r_l (lambda (list)
                            (if (! list)
                                nil
                                (begin
                                    (define loc (car list))
                                    (assert `(containers ,loc) nil)
                                    (+ (if (!(view loc)) 1 0) (r_l (cdr list)))))))
                        (r_l (instances container)))))))

    (def-prob-exec
        (perceive (if (= rae_mode rae_sim)
                      (sense_objects)
                      true))
        (wait 
            (if (= rae_mode rae_sim)
                (begin
                    (define r_l (lambda (list)
                        (if (! list)
                            nil
                            (begin
                                (assert `(emergency_handling ,(car list)) nil)
                                (r_l (cdr list))))))
                    (r_l (instances robot)))
                true)))
    


    ;commands
    (def-command take 
        (:params (r robot) (o object))
        (:plant-model
            (if (= (load r) nil)
                (if (= (loc r) (pos o))
                    (if (begin (exec_sim 'take) (prob-exec 'take))
                        (begin
                            (assert `(pos ,o) r)
                            (assert `(load r) o)
                            (success "Robot " r " has picked up object " o ".")
                        )
                        (failure "Non-deterministic event has made the take command fail."))
                    (failure "Robot " r " is not at object" o "'s location."))
                (failure "Robot " r " is not free to take anything"))))


    (def-command put
        (:params (r robot) (o object))
        (:plant-model
            (if (= (pos o ) r )
                (if (begin (exec_sim 'put) (prob-exec put))
                    (begin
                        (assert `(pos ,o) (loc r))
                        (assert (load r) empty)
                        (success "Robot " r " has put object " o " at location " (loc r) "."))
                    (failure "Robot " r " has failed to put " o " because of some internal error."))
                (failure "Object " o " is not with robot " r "."))))

    (def-command charge
        (:params (r robot) (c charger))
        (:plant-model
            (if (or (= (loc r) (pos c)) (= (pos c) r))
                (if (prob-exec 'charge)
                    (begin
                        (assert `(charge r) 4)
                        (success "Robot " r " is fully charged."))
                    (failure "Charging of robot " r " failed due to some internal error"))
                (failure "Robot " r " is not in the charger's location or it doesn't have the charger with it."))))

    (def-command move_to_emergency
        (:params (r robot) (l1 location) (l2 location) (d int))
        (:plant-model
         (begin
            (define res (if (= l1 l2)
                (success "Robot " r " is already at location " l2 ".")
                (if (and (= (loc r) l1) (>= (charge r) dist))
                    (if (prob-exec 'move_to_emergency)
                        (begin
                            (assert `(loc ,r) l2)
                            (assert `(charge ,r) (- (charge r) dist)))
                        (failure "Moving failed due to some internal error"))
                    (if (and (!= (loc r) l1) (>= (charge r) dist))
                        (failure "Robot " r " is not in location " l1 ".")
                        (if (and (= (loc r) l1) (< (charge r) dist))
                            (failure "Robot " r " does not have enough charge to move.")
                            (failure "Robot " r "is not at location " l1 "and it does not have enough charge to move.")
                    )))))
            (if (err? res)
                (assert `(emergency_handling ,r) nil))
            res)))

    (def-command perceive
        (:params (l location))
        (:plant-model
            (if (! (view l))
                (if (prob-exec 'perceive)
                    (begin
                        (begin
                            (define r_l
                                (lambda (list)
                                    (if (! list)
                                        nil
                                        (begin
                                            (assert `(pos (car list)) l)
                                            (r_l (cdr list))))))
                            (r_l (containers l)))
                        (assert `(view l) true)
                        (success "Perceived location " l ".")))
                (success "Already perceived."))))

    (def-command move
        (:params (r robot) (l1 location) (l2 location) (d int))
        (:plant-model
            (if (! (emergency_handling r))
                (if (= l1 l2)
                    (success "Robot" r " is already at location" l2 ".")
                    (if (and (= (loc r) l1) (or (>= (charge r) dist) (= (load r) 'c1)))
                        (if (prob-exec 'move)
                            (begin
                                (assert `(loc ,r) l2)
                                (if (= (load r ) c1) (assert `(charge r) (- (charge r) dist)))
                                (success "Robot " r " has moved from " l1 " to " l2 "."))
                            (failure "Robot " r " has failed to move due to some internal failure"))
                        (if (and (!= (loc r) l1) (>= (charge r) dist))
                            (failure "Robot " r " is not in location " l1 ".")
                            (if (and (=  (loc r ) l1) (< (charge r) dist)))
                                (failure "Robot " r " does not have enough charge to move.")
                                (failure "Robot " r " is not at location " l1 " and it doesn't haev enough charge to move."))))
                (failure "Robot is addressing emergency so it cannot move."))))

    (def-command address_emergency
        (:params (r robot) (l locatiob) (i ))
        (:plant-model 
            (begin
                (if (= (loc r) l)
                    (if (prob-exec 'address_emergency)
                        (success "Robot " r " has addressed emergency " i ".")
                        (failure "Robot " r " has failed to address emergency due to some internal error."))
                    (failure "Robot " r " has failed to address emergency " i "."))
                (assert `(emergency_handling r) false))))

    
    (def-task search (:params (r robot) (o object)))
    (def-task fetch (:params (r robot) (o object)))
    (def-task recharge (:params (r robot) (c charger)))
    (def-task move_to (:params (r robot) (l location)))
    (def-task emergency (:params (r robot) (l location) (i int)))
    (def-task non_emergency_move (:params (r robot) (l1 location) (l2 location) (d int)))

    (def-method search_method_1
        (:task search)
        (:params (r robot) (o object))
        (:body
            (if (= (pos o) 'unk)
                (begin
                    (define rl (lambda (list)
                        (if (null? list)
                            nil
                            (begin
                                (define l (car list))
                                (if (!(view l))
                                    l
                                    (rl (cdr list)))))))
                    (define next (rl (instances location)))
                    (if (null? next)
                        (failure "Failed to search " o".")
                        (begin
                            (move_to r next)
                            (perceive next)
                            (if (= (pos o) next)
                                (begin
                                    (if (!= (load r) nil)
                                        (put r (load r)))
                                    (take r o))
                                (search r o))))
                (success "Position of " o " is already known.")))))

    (def-method search_method_2
        (:task search)
        (:params (r robot) (o object))
        (:body
            (if (= (pos o) 'unk)
                    (begin
                        (define rl (lambda (list)
                            (if (null? list)
                                nil
                                (begin
                                    (define l (car list))
                                    (if (!(view l))
                                        l
                                        (rl (cdr list)))))))
                        (define next (rl (instances location)))
                        (if (null? next)
                            (failure "Failed to search " o".")
                            (begin
                                (recharge r 'c1)
                                (move_to r next)
                                (perceive next)
                                (if (= (pos o) next)
                                    (begin
                                        (if (!= (load r) nil)
                                            (put r (load r)))
                                        (take r o))
                                    (search r o))))
                    (success "Position of " o " is already known.")))))

    (def-method fetch_method_1
        (:task fetch)
        (:params (r robot) (o object))
        (:body 
            (begin
                (define pos_o (pos o))
                (if (= (pos_o) 'unk)
                    (search r o)
                    (begin
                        (if (!= (loc r) pos_o)
                            (move_to r pos_o))
                        (if (!= (load r) nil) (put r (load r)))
                        (take r o))))))

    (def-method fetch_method_2
        (:task fetch)
        (:params (r robot) (o object))
        (:body (begin
                (define pos_o (pos o))
                (if (= (pos_o) 'unk)
                    (search r o)
                    (begin
                        (if (!= (loc r) pos_o)
                            (begin
                                (recharge r 'c1)
                                (move_to r pos_o)))
                        (if (!= (load r) nil) (put r (load r)))
                        (take r o)))))
    )

    (def-method recharge_method_1
        (:task recharge)
        (:params (r robot) (c charger))
        (:body 
        ;When the charger is with another robot and that robot takes the charger back.
            (begin
                (define robot
                    (begin
                        (define robot 
                            (if (and (!= (loc r) (pos c)) (!= (pos c) r))
                                (begin
                                    (define robot 
                                        (if (! (contains (instances locations) (pos c)))
                                            (begin
                                                (define robot (pos c))
                                                (put (pos c) c)
                                                robot)))
                                    (move_to r (pos c))
                                    robot)))
                        (charge r c)
                        robot))
                (if (! (null? robot))
                    (take robot c)))))

    (def-method recharge_method_2
        (:task recharge)
        (:params (r robot) (c charger))
        (:body 
            ;Robot r charges and does not carries the charger with it.
            (begin
                (if (and (!= (loc r) (pos c)) (!= (pos c) r))
                    (begin
                        (if (! (contains (instances locations) (pos c)))
                            (put (pos c) c))
                        (move_to r (pos c))))
                (charge r c)
        )))

    (def-method recharge_method_3
        (:task recharge)
        (:params (r robot) (c charger))
        (:body 
            ;Robot r charges and carries the charger with it.
            (begin
                (if (and (!= (loc r) (pos c)) (!= (pos c) r))
                    (begin
                        (if (! (contains (instances locations) (pos c)))
                            (put (pos c) c))
                        (move_to r (pos c))))
                (charge r c)
                (take r c)
        )))

    (def-method move_to_method_1
        (:task move_to)
        (:params (r robot) (l location))
        (:body 
            (begin
                (define x (loc r))
                (define dist (get_distance x l))
                (if (or (>= (charge r) dist) (= (load r) 'c1))
                    (non_emergency_move r x l dist)
                    (begin
                        (assert `(charge r) 0)
                        (failure "Robot " r " does not have enough charge to move from " x " to " l "."))))))

    (def-method emergency_method_1
        (:task emergency)
        (:params (r robot) (l location) (i int))
        (:body
            (if (! (emergency_handling r))
                (do
                    (assert `(emergency_handling r) true)
                    (define load_r (load r))
                    (if (!= (load_r) empty) (put r load_r))
                    (define l1 (loc r))
                    (define dist (get_distance l1 l))
                    (move_to_emergency r l1 l dist)
                    (address_emergency r l i)
                )
                (failure r " is already busy handling another emergency"))))

    (def-method non_emergency_move_method_1
        (:task non_emergency_move)
        (:params (r robot) (l1 location) (l2 location) (d int))
        (:body
            (do
                (wait-for `(! (emergency_handling ,r)))
                (move r l1 l2 dist))))
    ;tasks methods

)