(defmacro sim_block (lambda (body)
`(begin
    (define state (get_facts))
    (define rae-mode simu-mode)
    ,body)))

(define check_preconditions (lambda (method)
    (sim_block (eval-pre-conditions method))))

(define eval-pre-conditions
    (lambda (method)
        (sim_block
            (eval (cons (get-preconditions (car method)) (quote-list (cdr method)))))))

(define compute-score 
    (lambda (method)
        (sim_block
            (eval (cons (get-score (car method)) (quote-list (cdr method)))))))

(define applicable?
    (lambda (method)
        (sim_block
            (eval-pre-conditions method))))

(define generate_applicable_instances
    (lambda (task)
        (let* ((task_label (first task))
               (params (cdr task))
               (methods (get rae-task-methods-map task_label)))
            (r_generate_instances 
                (enr (cons enumerate (cons methods params)))))))


(define r_test_method 
    (lambda (instances)
        (if (null? instances)
            nil
            (if (eval-pre-conditions (car instances))
                (cons (list (car instances) (compute-score (car instances))) (r_test_method (cdr instances)))
                (r_test_method (cdr instances))))))

(define r_generate_instances
    (lambda (methods)
        (if (null? methods)
            nil
            (let* ((method (car methods))
                    (methods (cdr methods))
                    (method_label (first method))
                    (params (cdr method)))
                (begin
                    (define types (get rae-method-types-map method_label))
                    (if (> (length types) (length params))
                        (begin
                            (define instance_types (mapf (sublist types (length params)) instance))
                            (define instances (enumerate (cons (car methods instance_types))))
                            (append (r_test_method instances) (r_generate_instances (cdr methods))))
                        (cons
                            (if (eval-pre-conditions method)
                                (list method (compute-score method))
                                nil)
                            (r_generate_instances (cdr methods)))))))))

(define select
  (lambda args
    (sim_block
    (rae-select args (eval (cons generate_applicable_instances (quote-list args)))))))


(defmacro generate-state-function (lambda args
    (let* ((label (car args))
          (p_expr (cdr args))
          (params (car (unzip p_expr))))
        `(list ,label
            (quote ,p_expr)
            (lambda ,params
                (if (= rae-mode exec-mode)
                    ,(cons 'rae-get-state-variable (cons `(quote ,label) params))
                    ,(if (= params nil)
                        `(get-map state ',label)
                        `(get-map state ,(cons 'list (cons `(quote ,label) params))))))))))