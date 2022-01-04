pub const MACRO_SIM_BLOCK: &str = "(defmacro sim_block (lambda (body)
`(begin
    (define state (rae-get-facts))
    (define rae-mode simu-mode)
    ,body)))";

pub const LAMBDA_GET_PRECONDITIONS: &str = "(define get-preconditions\
    (lambda (label)\
        (get rae-method-pre-conditions-map label)))";

pub const LAMBDA_GET_SCORE: &str = "(define get-score\
    (lambda (label)\
        (get rae-method-score-map label)))";

pub const LAMBDA_EVAL_PRE_CONDITIONS: &str = "(define eval-pre-conditions
    (lambda (method)
        (sim_block
            (eval (cons (get-preconditions (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_COMPUTE_SCORE: &str = "(define compute-score 
    (lambda (method)
        (sim_block
            (eval (cons (get-score (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable?
    (lambda (method)
        (sim_block
            (eval-pre-conditions method))))";

pub const LAMBDA_GENERATE_APPLICABLE_INSTANCES: &str = "(define generate_applicable_instances
    (lambda (task)
        (let* ((task_label (first task))
               (params (cdr task))
               (methods (get rae-task-methods-map task_label)))
            (r_generate_instances 
                (enr (cons enumerate (cons methods params)))))))";

pub const LAMBDA_R_GENERATE_INSTANCES: &str = "(define r_generate_instances
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
                            (define instance_types (mapf instance (sublist types (length params))))
                            (define instances (enumerate (cons (car methods) instance_types)))
                            (append (r_test_method instances) (r_generate_instances (cdr methods))))
                        (cons
                            (if (eval-pre-conditions method)
                                (list method (compute-score method))
                                nil)
                            (r_generate_instances (cdr methods)))))))))";

pub const LAMBDA_R_TEST_METHOD: &str = "(define r_test_method 
    (lambda (instances)
        (if (null? instances)
            nil
            (if (eval-pre-conditions (car instances))
                (cons 
                    (list (car instances) (compute-score (car instances)))
                    (r_test_method (cdr instances)))
                (r_test_method (cdr instances))))))";

//pub const EVAL_PRE_CONDITIONS: &str = "eval-pre-conditions";
//pub const COMPUTE_SCORE: &str = "compute-score";
//pub const GENERATE_APPLICABLE_INSTANCES: &str = "generate-applicable-instances";
pub const STATE: &str = "state";

/*pub const LAMBDA_CHECK_PRECONDITIONS: &str = "(define check_preconditions
(lambda (method)
    (sim_block (eval-pre-conditions method))))";*/
