(begin
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
                                                    (cons (list (cons ,method-label params) (eval (cons (lambda ,params ,score-generator) params)))
                                                        (eval_params (cdr args)))
                                                    (eval_params (cdr args)))))))
                                (eval_params (eval (cons enumerate (append args (quote ,list-element)))))))
                        (lambda ,params ,body)))))
    (define get-methods
            (lambda (label)
                (get-map rae-task-methods-map label)))
    (define get-method-generator
       (lambda (label)
            (get-map rae-method-generator-map label)))
    (define generate_instances (lambda args
        (let* ((label (car args))
                (i_params (cdr args))
                (methods (get-methods label)))

                (begin
                    (define __generate__
                        (lambda (methods)
                            (if (null? methods)
                                nil
                                (append
                                    (eval
                                        (append (list (get-method-generator (car methods)))
                                            i_params))
                                    (__generate__ (cdr methods))))))
                    (__generate__ methods)))))
    (define rae-task-methods-map (map '((t1 . (m1 m2)))))
    (define is-ok-m1 (lambda (?b) (or (= ?b b1) (= ?b b2))))
    (define is-ok-m2 (lambda (?b) (or (= ?b b3) (= ?b b4))))
    (define m1-def (generate-method m1 ((:task t1)
        (:params ?a ?b)
        (:pre-conditions true)
        (:effects nil)
        (:parameters-generator (list-b) (is-ok-m1 ?b))
        (:score-generator 10)
        (:body (print "ok m1")))))
    (define m2-def (generate-method m2 ((:task t1)
        (:params ?a ?b)
        (:pre-conditions true)
        (:effects nil)
        (:parameters-generator (list-b) (is-ok-m2 ?b))
        (:score-generator 9)
        (:body (print "ok m2")))))
    (define rae-method-generator-map (map `((m1 . ,(get-list m1-def 4)) (m2 . ,(get-list m2-def 4)))))
    (define list-b '(b1 b2 b3 b4))
)