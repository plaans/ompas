(begin

    (def-types agent location taxi)
    (def-objects (taxi_1 taxi))

    ;lambda
    (def-lambda taxi_rate (lambda (dist) (+ 1.5 (* dist 0.5))))
    
    ;state functions
    (def-state-function loc (:params (a object)) (:result object))
    (def-state-function occupied (:params (o object)) (:result bool))
    (def-state-function owe (:params (a agent)) (:result float))
    (def-state-function cash (:params (a agent)) (:result float))
    (def-static-state-function dist (:params (l1 location) (l2 location)) (:result int))
    ;commands
    (def-command walk 
        (:params (a agent) (x location) (y location))
        (:model (om-model
            (:pre-conditions (= (loc a) x))
            (:body 
                (assert `(loc ,a) y)
            )
    )))

    (def-command call_taxi 
        (:params (a agent) (x location))
        (:model (om-model 
            (:pre-conditions (!(occupied taxi_1)))
            (:body
                (assert '(loc taxi_1) x)    
            ))))

    (def-command enter_taxi
        (:params (a agent))
        (:model (om-model
            (:pre-conditions (= (loc a) (loc taxi_1)) (!(occupied taxi_1)))
            (:body
                (begin
                    (assert `(loc ,a) taxi_1)
                    (assert '(occupied taxi_1) true))
            )        
            )))

    (def-command taxi_carry
        (:params (a agent) (y location))
        (:model (om-model
            (:pre-conditions (= (loc a) taxi_1))
            (:body
                (begin
                    (define x (loc taxi_1))
                    (assert `(loc taxi_1) y)
                    (assert `(owe a) (taxi_rate (dist x y)))
            )))))

    (def-command pay_driver
        (:params (a agent))
        (:model (om-model 
            (:pre-conditions (>= (cash a) (owe a)))
            (:body
                (begin
                    (assert `(cash a) (- (cash a) (owe a)))
                    (assert `(owe a) 0.0)
                )
            )
        )))
    
    (def-command leave_taxi
        (:params (a agent))
        (:model (om-model
            (:pre-conditions (=(loc a) taxi_1))
            (:body
                (assert `(occupied taxi_1) nil)
                (assert `(loc a) (loc taxi_1))
            ))))

    (def-task travel (:params (a agent) (x location) (y location)))
    (def-method travel_by_foot 
        (:task travel)
        (:params (a agent) (x location) (y location))
        (:pre-conditions (<= (dist x y) 2))
        (:body
            (walk a x y)))



    (def-method travel_by_taxi 
        (:task travel)
        (:params (a agent) (x location) (y location))
        (:pre-conditions (>= (cash a) (taxi_rate (dist x y))))
        (:body
            (do
                (call_taxi a x)
                (ride_taxi a y))))

    (def-task ride_taxi (:params (a agent) (y location)))

    (def-method ride_taxi_method 
        (:task ride_taxi)
        (:params (a agent) (y location))
        (:pre-conditions (< (dist (loc a) y) 50))
        (:body
            (do
                (enter_taxi a)
                (taxi_carry a y)
                (pay_driver a)
                (leave_taxi a))))

)