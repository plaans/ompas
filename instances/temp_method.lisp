
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
            (rae-await (face_belt ?r ?b 5))
            nil))))

(def-task t_carry_to_machine ?r ?p ?m)
(def-method m_carry_to_machine 
    '((:task t_carry_to_machine)
        (:params ?r ?p ?m)
        (:body ())))

(def-task t_charge ?r)
(def-method m_charge
    '((:task t_charge)
        (:params ?r)
        (:body 
            (begin 
                (rae-await (go_charge ?r))
                (wait-on ))))

(defmacro generate-task-simple 
    (lambda args
    (let ((label (car args))
            (params (cdr args)))
            (quasiquote (list ,label (lambda ,params
                    (progress (quote ,label) ,params)))))))


(defmacro generate-task-simple 
    (lambda args
    (let ((label (car args))
            (params (cdr args)))
            (quasiquote (list ,label (lambda ,params
                     
                    (print ,(cons 'progress (cons `(quote ,label) params)))))))))


                    (define progress (lambda args
    (let* ((result (eval (cons rae-select (cons `(quote ,(car args)) (cdr args)))))
            (first_m (car result))
            (task_id (cadr result)))
            
            (if (null? first_m)
                nil
                (if (eval first_m)
                    (rae-set-success-for-task task_id)
                    (rae-retry task_id))))))


(generate-evaluator ((:task pick)
                     (:params ?r)
                     (:parameters-generator (robots) ((?r) (robot.is_available ?r)))
                     (:score-generator 10)
                     (:body )))

(lambda args 
    (begin 
        (define eval_params 
            (lambda args 
                ((lambda (params) 
                    (if (null? params)
                        nil
                        (if (eval (cons (lambda (?r) (robot.is_available ?r)) params))
                            (cons (list params ((lamdba (?r)) 10)) (eval_params (cdr args))) (eval_params (cdr args))))) (car args)))) (eval_params (enumerate append args (quote (robots))))))


(lambda args
    (begin 
        (define eval_params
            (lambda args 
                ((lambda (params)
                    (if (null? params)
                        nil
                        (if (eval (cons (lambda (?r) (robot.is_available ?r)) params))
                            (cons (list params 
                                        ((lamdba (?r) 10) params))
                                (eval_params (cdr args)))
                            (eval_params (cdr args))))) (car args))))
    (eval_params (enumerate (append args (quote (robots)))))))

(begin 
    (defmacro generate-evaluator 
        (lambda (method)
            (let* ((parameter-generator (get-list method 2))
                (list-element (get-list parameter-generator 1))
                (params-symbols (car (get-list parameter-generator 2)))
                (body-generator (cadr (get-list parameter-generator 2)))
                (score-generator (cadr (get-list method 3))))
                    
                    `(lambda args
                        (begin
                            (define eval_params
                                (lambda args
                                    (let ((params (car args)))
                                        (if (null? params)
                                            nil
                                            (if (eval (cons (lambda ,params-symbols ,body-generator) params))
                                                (cons (list params ((lamdba ,params-symbols ,score-generator) params))
                                                    (eval_params (cdr args)))
                                                (eval_params (cdr args)))))))
                            (eval_params (enumerate (append args (quote ,list-element)))))))))
(defmacro generate-method
    (lambda (label m-def)
        (let* ((task-label (cadr (get-list m-def 0)))
                (params (cdr (get-list m-def 1)))
                (body (cadr (get-list m-def 4)))
                (parameter-generator (get-list m-def 2))
                (list-element (get-list parameter-generator 1))
                (params-symbols (car (get-list parameter-generator 2)))
                (body-generator (cadr (get-list parameter-generator 2)))
                (score-generator (cadr (get-list m-def 3))))
                `(list ,label
                        (quote ,task-label)
                        (lambda ,params ,body)
                        (lambda args
                            (begin
                                (define eval_params
                                    (lambda args
                                        (let ((params (car args)))
                                            (if (null? params)
                                                nil
                                                (if (eval (cons (lambda ,params-symbols ,body-generator) params))
                                                    (cons (list params ((lamdba ,params-symbols ,score-generator) params))
                                                        (eval_params (cdr args)))
                                                    (eval_params (cdr args)))))))
                                (eval_params (enumerate (append args (quote ,list-element))))))))))
)

(generate-method m1 ((:task pick)
                     (:params ?r ?m)
                     (:parameters-generator (robots machines) ((?r ?m) (robot.is_available ?r)))
                     (:score-generator 10)
                     (:body (+ 3 3))))
(m1 pick 
    (lambda (?r) (+ 3 3))
    (lambda args
        (begin
            (define eval_params
                (lambda args
                    ((lambda (params)
                        (if (null? params)
                            nil
                            (if (eval (cons (lambda (?r) (robot.is_available ?r)) params))
                                (cons (list params ((lamdba (?r) 10) params)) (eval_params (cdr args)))
                                (eval_params (cdr args))))) (car args))))
            (eval_params (enumerate (append args (quote (robots))))))))


(defmacro wait-on (lambda (expr)
    `(if (not ,expr)
        (check (quote ,expr)))))
