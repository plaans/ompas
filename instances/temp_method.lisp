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


