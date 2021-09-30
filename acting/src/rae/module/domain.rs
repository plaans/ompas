pub const GENERATE_TASK_SIMPLE: &str = "generate-task-simple";
pub const GENERATE_STATE_FUNCTION: &str = "generate-state-function";
pub const GENERATE_TASK: &str = "generate-task";
pub const GENERATE_METHOD: &str = "generate-method";

/// Macro used to generate code to define a state function in RAE environment.
pub const MACRO_GENERATE_STATE_FUNCTION: &str = "(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            (lambda ,params
                (get-map state ,(cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a task in RAE environment.
pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task \
                                        (lambda (l body) \
                                            (quasiquote (list (unquote l) (lambda (unquote (cdar body)) \
                                                (if (unquote (cadadr body)) \
                                                    (unquote (cadaddr body)) \
                                                    (quote (task is not applicable in the given state))))))))";

/// Macro used to generate code to define a task in the simplified representation in RAE environment.
pub const MACRO_GENERATE_TASK_SIMPLE: &str = "(defmacro generate-task-simple 
    (lambda args
    (let ((label (car args))
          (params (cdr args)))
         `(list ,label (lambda ,params
                    ,(cons 'progress (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a method in REA environment.
pub const MACRO_GENERATE_METHOD_DEPRECATED: &str = "(defmacro generate-method \
                                          (lambda (l body) \
                                            (let ((task-label (cadar body)) \
                                                  (params (cdadr body)) \
                                                  (body (cadaddr body))) \
                                                 (quasiquote (list (unquote l) \
                                                                    (quote (unquote task-label)) \
                                                                    (lambda (unquote params) \
                                                                            (unquote body)))))))";

pub const MACRO_GENERATE_METHOD: &str=
"(defmacro generate-method;-final-till-there-is-a-new-one
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
                        (lambda ,params ,body)))))";

/// Macro used to generate code to define an action in RAE environment.
pub const MACRO_GENERATE_ACTION: &str = "(defmacro generate-action
    (lambda args
        (let ((label (car args))
              (params (cdr args)))
             `(list ,label
                 (lambda ,params ,(cons 'rae-exec-command
                     (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a method in RAE environment.
pub const MACRO_GENERATE_METHOD_PARAMETERS: &str =
    "(defmacro generate-method-parameters (lambda args
    (let ((label (car args))
            (args_enum (cdr args)))

        (quasiquote (quote (unquote 
            (list label 
            (let ((p_enum (car args_enum))
                (p_labels (caadr args_enum))
                (conds (cadadr args_enum)))

                (quasiquote 
                    ((unquote begin) 
                        (define eval_params ((unquote lambda) args
                            (let ((params (car args)))
                                (if (not (null? params))
                                    (if (eval (cons ((unquote lambda) (unquote p_labels) (unquote conds)) params))
                                        (cons params (eval_params (cdr args)))
                                        (eval_params (cdr args)))
                                    nil))))
                        (eval_params (unquote (cons enumerate p_enum)))))))))))))";

pub const LABEL_GENERATE_METHOD_PARAMETERS: &str = "generate-method-parameters";

/// Macro to define lambda that will evaluates set of parameters that can instantiate a method in a given state.
pub const MACRO_ENUMERATE_PARAMS: &str = "(defmacro enumerate-params (lambda args
    (let ((p_enum (car args))
        (p_labels (caadr args))
        (conds (cadadr args)))

        (quasiquote 
            (begin 
                (define eval_params (lambda args
                    (let ((params (car args)))
                        (if (not (null? params))
                            (if (eval (cons (lambda (unquote p_labels) (unquote conds)) params))
                                (cons params (eval_params (cdr args)))
                                (eval_params (cdr args)))
                            nil))))
                (eval_params (unquote (cons enumerate p_enum))))))))";

pub const MACRO_MUTEX_LOCK_AND_DO: &str = "(defmacro mutex::lock-and-do 
    (lambda (r b)
        `(begin
            (lock ,r)
            ,b
            (release ,r))))";
pub const MACRO_WAIT_ON: &str = "(defmacro wait-on (lambda (expr)
    `(if (not (eval ,expr))
        (check ,expr))))";
pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

/*pub const LAMBDA_MUTEX_LOCK: &str = "(define mutex::lock (lambda (__symbol__)
                                        (begin
                                            (wait-on `(not (mutex::locked? ,__symbol__)))
                                            (assert `(locked ,__symbol__) true))))";

pub const LAMBDA_MUTEX_IS_LOCKED: &str = "(define mutex::locked? (lambda (__symbol__)
                                        (rae-get-state-variable `(locked ,__symbol__))))";

pub const LAMBDA_MUTEX_RELEASE: &str = "(define mutex::release (lambda (__symbol__)
                                        (retract `(locked ,__symbol__) true)))";*/

pub const LAMBDA_PROGRESS: &str = "
(define progress (lambda args
    (let* ((result (eval (cons select (cons `(quote ,(car args)) (cdr args)))))
            (first_m (car result))
            (task_id (cadr result)))
            
            (if (null? first_m)
                nil
                (if (eval first_m)
                    (rae-set-success-for-task task_id)
                    (retry task_id))))))";

pub const LAMBDA_SELECT: &str = "
(define select
  (lambda args
    (rae-select args (eval (cons generate-instances (cons `(quote ,(car args)) (cdr args)))))))";

pub const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin 
            (print \"Retrying task \" task_id)
            (if (null? new_method) ; if there is no method applicable
            nil
            (if (eval new_method)
                (rae-set-success-for-task task_id)
                (rae-retry task_id)))))))";

//Access part of the environment

pub const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get-map rae-task-methods-map label)))";

pub const LAMBDA_GET_METHOD_GENERATOR: &str = "\
(define get-method-generator
       (lambda (label)
            (get-map rae-method-generator-map label)))";

pub const LAMBDA_GENERATE_INSTANCES: &str = "
(define generate-instances (lambda args
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
                (__generate__ methods)))))";
