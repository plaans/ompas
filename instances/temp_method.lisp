(defmacro generate-action
    (lambda args
        (let ((label (car args))
              (params (cdr args)))
             `(list ,label
                (lambda ,params ,(cons 'rae-exec-command
                    (cons `(quote ,label)) params))
                (lambda ,params ,(cons 'rae-exec-command
                    (cons `(quote ,label)) params))))))

(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            (lambda ,params
                (get-map state ,(cons `(quote ,label) params)))))))

(robot.battery
(lambda (?r)
    (rae-get-state-variable
        (quote robot.battery) ?r))
(lambda (?r)
    (get-map state ((quote robot.battery) ?r))))

(defmacro generate-action-model
    (lambda args
        (let* ((label (car args))
                (def (cadr args))
                (params (cdar def))
                (body (cadr (get-list def 1))))zomm
              `(list ,label (lambda ,params ,body)))))


(generate-action-model pick 
	((:params ?r)
	 (:body
        (if (> (robot.battery ?r) 0.15)
            (begin
                (assert (robot.hand ?r) empty)
                (assert (robot.available ?r) true))))))

(begin
    (define x 1)
    (list ((lambda nil 
        (begin (set! x 2) x))) x))


(defmacro assert
    (lambda args
        `(set! state (set-map state (quote ,args)))))

    (set! state (set-map state ((robot.hand robot0) empty)))

(defmacro retract
    (lambda args
        `(set! state (remove-key-value-map state (quote ,args)))))

(defmacro generate-state-function (lambda args
    (let ((label (car args))
           (params (cdr args)))
        `(list ,label
         (lambda ,params
          ,(cons 'rae-get-state-variable (cons `(quote ,label) params))))))
          
          
          
          
          (cons (cons (quote quote) (cons label nil)))


(defmacro generate-action
    (lambda args
        (let ((label (car args))
              (params (cdr args)))
             `(list ,label
                 (lambda ,params ,(cons 'rae-exec-command
                     (cons `(quote ,label) params)))))))


(defmacro generate-action-model
    (lambda (label def)
        (let ((params (cdar def))
               (conds (cadr (get def 1)))
               (effs (cadr (get def 2))))
              `(lambda ,params
                    (begin
                        (if ,conds
                            ,effs)
                        state)))))


(defmacro generate-action-operational-model
    (lambda (label def)
        (let ((params (cdar def))
              (body (cadr (get def 1))))
              `(lambda ,params
                    (begin
                        ,body
                        state)))))

(test-macro "(generate-action-operational-model pick 
	((:params ?r)
		(:body
        (if (> (robot.battery ?r) 0.15)
            (begin
                (assert (robot.hand ?r) empty)
                (assert (robot.available ?r) true))))))")

(lambda (?r)
    (begin 
        (if (> (robot.battery ?r) 0.15)
            (begin 
                (assert (robot.hand ?r) empty) 
                (assert (robot.available ?r) true))
            nil)
        state))

(generate-action-model pick 
    '((:params ?r)
      (:pre-conditions (> (robot.battery ?r) 0.15))
      (:effects (begin
	       (assert (robot.hand ?r) empty)
        (assert (robot.available ?r) true)))))

(test-macro "(generate-action-model pick 
    '((:params ?r)
      (:pre-conditions (> (robot.battery ?r) 0.15))
      (:effects (begin
	       (assert (robot.hand ?r) empty)
        (assert (robot.available ?r) true)))))")

(defmacro test-macro
   (lambda (x)
    `(expand (parse ,x))))


(lambda (?r)
    (begin 
        (if (> (robot.battery ?r) 0.15)
            (begin 
                (assert (robot.hand ?r) empty)
                (assert (robot.available ?r) true))
            nil)
    state))


(def-action-model pick 
    '((:params ?r)
      (:pre-conditions (> (robot.battery ?r) 0.15))
      (:effects (begin
	       (assert (robot.hand ?r) empty)
        (assert (robot.available ?r) true)))))
>> (lambda (?r)
		(begin 
            (define result
                (if (> (robot.battery ?r) 0.15)
                    (begin
                        (assert (robot.hand ?r) empty)
                        (assert (robot.available ?r) true))
                    (failure)))
            (if (null? result)
			    (success state)
                result)))


Definition partielle d'une méthode:

(
 (:task label)
 (:params (?r robot) (?p package))); utilisation de unzip pour séparer les paramètres de leurs types
 (:pre-conditions ( = (robot.ready ?r) true)))
 (:score 0))



 ((:task t_navigate_to)
    (:params ?r ?x ?y)
    (:pre-conditions true)
    (:effects nil)
    ;(:parameters-generator nil true) removed
    (:score-generator 0)
    (:body )


-> 
(lambda (?r ?p)
    (if 
        (and
            (!= (robot.instance ?r) nil)
            (!= (pacakge.instance ?p) nil)) ;test des types
        (if (robot.ready ?r) true nil) ;test des 
        nil
    ))


(generate-types test (?r ?p) (robot package)) 
-> (and (! (null? (robot.instance ?r)))
        (! (null? (pacakge.instance ?r))))

(defmacro generate-method
    (lambda (m_label df)
        (let ((t_label (cadar def))
              (p_expr (cdr (get def 1)))
              (conds (cadr (get def 2)))
              (effs (cadr (get def 3)))
              (score (cadr (get def 4)))
              (body (cadr (get def 5))))

            (let* ((p_unzip (unzip p_expr))
                   (params (car p_unzip))
                   (types (cadr p_unzip)))
    
            `(list ,m_label 
                (quote ,t_label)
                ;lambda for preconditons
                (lambda ,params
                    (if ,(gtpc p_expr)
                        (if ,conds true)))
                (lambda ,params ,effs)
                (lambda ,params score)
                (lambda ,params body))))))
(begin
    (defmacro generate-method
        (lambda (m_label df)
            (let ((t_label (cadar def))
                (p_expr (cdr (get def 1)))
                (conds (cadr (get def 2)))
                (effs (cadr (get def 3)))
                (score (cadr (get def 4)))
                (body (cadr (get def 5))))

                (let* ((p_unzip (unzip p_expr))
                    (params (car p_unzip))
                    (types (cadr p_unzip)))
        
                `(list ,m_label 
                    (quote ,t_label)
                    (quote ,types)
                    ;lambda for preconditons
                    (lambda ,params
                        (if ,(gtpc p_expr)
                            (if ,conds true)))
                    (lambda ,params ,effs)
                    (lambda ,params ,score)
                    (lambda ,params ,body))))))
    (test-macro 
        "(generate_method m_navigate_to '((:task t_navigate_to)
            (:params (?r robot) (?x float) (?y float))
            (:pre-conditions (and (robot.available ?r) (< ?x 10) (< ?y 10))
            (:effects (assert (robot.position ?r) '(?x ?y)))
            (:score 0)
            (:body
            (begin
                (navigate_to ?r ?x ?y)))))")
)

(generate-method m_navigate_to ((:task t_navigate_to)
            (:params (?r robot) (?x float) (?y float))
            (:pre-conditions (and (robot.available ?r) (< ?x 10) (< ?y 10)))
            (:effects (assert (robot.position ?r) '(?x ?y)))
            (:score 0)
            (:body
            (begin
                (navigate_to ?r ?x ?y)))))

(defmacro generate-method;-final-till-there-is-a-new-one
    (lambda (method-label m-def)
    (let* ((task-label (cadr (get-list m-def 0)))
            (params (cdr (get-list m-def 1)))
            (pre-conditions (cadr (get-list m-def 2)))
            (effects (cadr (get-list m-def 3)))
            (body (cadr (get-list m-def 6)))
            (parameter-generator (cdr (get-list m-def 4)))
            (list-element (car parameter-generator))
            (body-generator (cadr parameter-generator))
            (score-generator (cadr (get-list m-def 5))))
            `(list ,method-label
                    ;label of the task
                    (quote ,task-label)
                    ;body of the method
                    (lambda ,params ,pre-conditions)
                    (lambda ,params ,effects)
                    ;lambda to generate instances
                    (lambda args
                        (begin
                            (define eval_params
                                (lambda args
                                    (let ((params (car args)))
                                        (if (null? params)
                                            nil
                                            (if (eval (cons (lambda ,params ,body-generator) params))
                                                (cons (list (cons (quote ,method-label) params) (eval (cons (lambda ,params ,score-generator) params)))
                                                    (eval_params (cdr args)))
                                                (eval_params (cdr args)))))))
                            (eval_params (eval (cons enumerate (append args (quote ,list-element)))))))
                    (lambda ,params ,body)))))


(generate-type-test-expr ((?r robot) (?x float) (?y float)))


(defmacro and (lambda args
    (if (null? args)
        nil
        (if (= (length args) 1)
            (car args)
            `(if ,(car args)
                 (and ,(cdr args))
                 nil)))))


(defmacro or (lambda args
                                                (if (null? args)
                                                    nil
                                                    (if (= (length args) 1)
                                                        (car args)
                                                        `(if ,(car args))
                                                              true
                                                              (or ,(cdr args))))))