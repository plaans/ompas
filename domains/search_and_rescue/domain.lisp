(begin
    (def-types 
        (robot obstacle robot_type object)
        (wheeled_robot large_robot robot))

    (def-objects (wheeled large wheeled_robot) ('ok 'has_debri object))

    (def-state-function loc (:params (r robot)) (:result (tuple int int)))
    (def-state-function has_medicine (:params (r robot)) (:result int))
    (def-state-function robot_type (:params (r robot)) (:result robot_type))
    (def-state-function status (:params ()) (:result object))
    (def-state-function real_status (:params ()) (:result))

    (def-prob
        (give_support_to_person (0.9 0.1))
        (clear_location (0.8 0.2))
        (inspect_person (0.8 0.2))
        (move_euclidean (0.95 0.05))
        (move_curved (0.95 0.05))
        (move_manhattan (0.95 0.05))
        (fly (0.9 0.1))
        (inspect_location (0.98 0.02))
        (change_altitude (0.8 0.2)))

    (def-prob-exec
        (perceive
            (if (= rae_mode 'rae_exec)
                (sense_objects)
                true
            ))
        (wait)
        (capture_image (sense_image))

    )

    (def-env sense_image
        (lambda (r camera l)
            (begin

            )
        )
    )

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
                        (r_l (instances container))))
                ()
                        
                        )))

)