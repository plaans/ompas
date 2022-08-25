//manage facts
pub const RAE_ASSERT: &str = "assert";
pub const RAE_ASSERT_SHORT: &str = "+>";
pub const RAE_RETRACT: &str = "retract";
pub const RAE_RETRACT_SHORT: &str = "->";
pub const RAE_INSTANCE: &str = "instance";
pub const RAE_AWAIT: &str = "rae-await";
pub const RAE_MONITOR: &str = "monitor";
pub const RAE_WAIT_FOR: &str = "wait-for";
pub const LAMBDA_MONITOR: &str = "(define monitor
    (lambda (e)
        (wait-for `(= ,e nil))))";

pub const MACRO_RUN_MONITORING: &str = "(defmacro run-monitoring
    (lambda (b m)
        `(race ,b (monitor ,m))
))";
//RAE Interface with a platform

pub const RAE_GET_STATE_VARIBALE: &str = "rae-get-state-variable";

pub const RAE_CANCEL_COMMAND: &str = "rae-cancel-command";
pub const RAE_GET_INSTANTIATED_METHODS: &str = "rae-get-instantiated-methods";
pub const RAE_GET_BEST_METHOD: &str = "rae-get-best-method";
pub const RAE_SELECT: &str = "rae-select";
pub const RAE_GET_NEXT_METHOD: &str = "rae-get-next-method";
pub const RAE_SET_SUCCESS_FOR_TASK: &str = "rae-set-success-for-task";

pub const SUCCESS: &str = "success";
pub const FAILURE: &str = "failure";
pub const IS_SUCCESS: &str = "success?";
pub const IS_FAILURE: &str = "failure?";

pub const RAE_GET_STATE: &str = "get-state";
pub const RAE_GET_FACTS: &str = "rae-get-facts";
pub const RAE_GET_STATUS: &str = "get-status";
pub const RAE_GET_AGENDA: &str = "get-agenda";
pub const RAE_GET_TASK_NETWORK: &str = "get-task-network";
pub const RAE_GET_TYPE_HIERARCHY: &str = "get-type-hierarchy";
pub const RAE_GET_STATS: &str = "get-stats";
pub const RAE_EXPORT_STATS: &str = "export-stats";
pub const RAE_GET_METHODS: &str = "get-methods";
pub const RAE_GET_ACTIONS: &str = "get-actions";
pub const RAE_GET_SYMBOL_TYPE: &str = "get-symbol-type";
pub const RAE_GET_TASKS: &str = "get-tasks";
pub const RAE_GET_STATE_FUNCTIONS: &str = "get-state-functions";
pub const RAE_GET_ENV: &str = "get-env";
pub const RAE_GET_MUTEXES: &str = "get-mutexes";
pub const RAE_GET_MONITORS: &str = "get-monitors";

pub const DOC_RAE_GET_METHODS: &str = "Returns the list of all defined methods in RAE environment";
pub const DOC_RAE_GET_ACTIONS: &str = "Returns the list of all defined actions in RAE environment";
pub const DOC_RAE_GET_CONFIG_PLATFORM: &str = "Get the actual value of the config of the platform";
pub const DOC_RAE_GET_AGENDA: &str =
    "Get the actual agenda with for each task the current refinement stack.";
pub const DOC_RAE_GET_STATE: &str = "Returns the current state";
pub const DOC_RAE_GET_STATUS: &str = "Returns the current status of actions";
pub const DOC_RAE_GET_ENV: &str = "Returns the whole environment.";

pub const RAE_GET_CONFIG_PLATFORM: &str = "get-config-platform";
pub const RAE_GET_CONFIG_SELECT: &str = "get-config-select";
pub const DOC_RAE_GET_SYMBOL_TYPE: &str =
    "Returns the type of the symbol as defined in RAE environment";
pub const DOC_RAE_GET_SYMBOL_TYPE_VERBOSE: &str = "Types:\n\
                                           \t-state-function\n\
                                           \t-action\n\
                                           \t-task\n\
                                           \t-method";
pub const DOC_RAE_GET_TASKS: &str = "Returns the list of all defined tasks in RAE environment";
pub const DOC_RAE_GET_STATE_FUNCTIONS: &str =
    "Returns the list of all defined state-functions in RAE environment";

pub const MACRO_MUTEX_LOCK_AND_DO: &str = "(defmacro mutex::lock-and-do
    (lambda (r p b)
        `(begin
            (lock ,r ,p)
            ,b
            (release ,r))))";
pub const LOCK: &str = "lock";
pub const RELEASE: &str = "release";
pub const IS_LOCKED: &str = "locked?";
pub const LOCKED: &str = "locked";
pub const LOCKED_LIST: &str = "locked-list";
pub const LOCK_IN_LIST: &str = "lock-in-list";

pub const MACRO_MUTEX_LOCK_IN_LIST_AND_DO: &str = "(defmacro mutex::lock-in-list-and-do
    (lambda (l p b)
        `(begin
            (define r (lock-in-list ,l ,p))
            ,b
            (release r))))";

pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_IS_PLATFORM_DEFINED: &str = "rae-platform?";

pub const REFINE: &str = "refine";
pub const RETRY: &str = "retry";
pub const LAMBDA_RAE_EXEC_TASK: &str = "(define rae-exec-task
    (lambda task
        (begin
            (define result (enr (cons 'refine task)))
            (if (err? result)
                result
                (let ((method (first result))
                      (task_id (second result)))

                    (begin
                        (define parent_task task_id)
                        (print \"Trying \" method \" for \" task_id)
                        (if (err? (enr method))
                            (rae-retry task_id)
                            (rae-set-success-for-task task_id))))))))";

pub const LAMBDA_RAE_RETRY: &str = "(define rae-retry
    (lambda (task_id)
        (begin
            (define result (retry task_id))
            (if (err? result)
                result
                (begin
                    (if (err? (enr result))
                        (rae-retry task_id)
                        (rae-set-success-for-task task_id)))))))";

pub const ERR_NO_APPLICABLE_METHOD: &str = "err::no-applicable-method";
pub const ERR_ACTION_FAILURE: &str = "err::action_failure";

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
pub const GET_ACTION_MODEL: &str = "get-action-model";

pub const LAMBDA_GET_ACTION_MODEL: &str = "(define get-action-model
    (lambda (label)
        (get rae-action-model-map label)))";

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
                    (if (> (len types) (len params))
                        (begin
                            (define instance_types (mapf instance (sublist types (len params))))
                            (define instances (enr (cons enumerate (append method instance_types))))
                            (append (r_test_method instances) (r_generate_instances methods)))
                        (cons
                            (if (! (err? (eval-pre-conditions method)))
                                (list method (compute-score method))
                                nil)
                            (r_generate_instances methods))))))))";
pub const LAMBDA_R_TEST_METHOD: &str = "(define r_test_method
    (lambda (instances)
        (if (null? instances)
            nil
            (if (eval-pre-conditions (car instances))
                (cons
                    (list (car instances) (compute-score (car instances)))
                    (r_test_method (cdr instances)))
                (r_test_method (cdr instances))))))";

pub const DEFINE_ERR_NO_APPLICABLE_METHOD: &str = "(define err::no-applicable-method 0)";
pub const DEFINE_ERR_ACTION_FAILURE: &str = "(define err::action_failure 1)";
pub const MOD_RAE_DESCRIPTION: &str = "rae-description";

pub const GENERATE_TASK: &str = "generate-task";
pub const GENERATE_STATE_FUNCTION: &str = "generate-state-function";
pub const GENERATE_ACTION: &str = "generate-action";
pub const GENERATE_ACTION_MODEL: &str = "generate-action-model";
pub const GENERATE_ACTION_OPERATIONAL_MODEL: &str = "generate-action-operational-model";
pub const GENERATE_METHOD: &str = "generate-method";

pub const RAE_DEF_STATE_FUNCTION: &str = "def-state-function";
pub const RAE_DEF_COMMAND: &str = "def-command";
pub const RAE_DEF_ACTION_MODEL: &str = "def-action-model";
pub const RAE_DEF_TASK_MODEL: &str = "def-task-model";
pub const RAE_DEF_METHOD_MODEL: &str = "def-method-model";
//pub const RAE_DEF_ACTION_OPERATIONAL_MODEL: &str = "def-action-operational-model";
pub const RAE_DEF_TASK: &str = "def-task";
pub const RAE_DEF_METHOD: &str = "def-method";
pub const RAE_DEF_LAMBDA: &str = "def-lambda";
pub const RAE_DEF_INITIAL_STATE: &str = "def-initial-state";
pub const RAE_ADD_CONSTANT: &str = "add-constant";
pub const RAE_ADD_TYPE: &str = "add-type";
pub const RAE_ADD_OBJECT: &str = "add-object";
pub const RAE_DEF_OBJECTS: &str = "def-objects";
pub const RAE_DEF_TYPES: &str = "def-types";
pub const RAE_DEF_CONSTANTS: &str = "def-constants";

pub const DOC_DEF_STATE_FUNCTION: &str = "Insert a state function in RAE environment.";
pub const DOC_DEF_STATE_FUNCTION_VERBOSE: &str =
    "Example:\n(def-state-function robot.coordinates ?r)";
pub const DOC_DEF_ACTION: &str = "Insert an action in RAE environment.";
pub const DOC_DEF_ACTION_VERBOSE: &str = "Example:\n(def-action pick ?r)";
pub const DOC_DEF_TASK: &str = "Insert a task in RAE environment";
pub const DOC_DEF_TASK_VERBOSE: &str = "Example:\n(def-task t_navigate_to ?r ?x ?y)";
pub const DOC_DEF_METHOD: &str = "Insert a method in RAE environment.";
pub const DOC_DEF_METHOD_VERBOSE: &str =
    "Example:\n(def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin\n\
        \t(rae-await (navigate_to ?r ?x ?y))\n\
        \t(rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))";
pub const DOC_DEF_LAMBDA: &str = "Add a lambda to RAE environment";
pub const DOC_DEF_INITIAL_STATE: &str = "Add initial facts in the state.\
Most of the time it is general knowledge and not initialisation of facts.";

/// Macro used to generate code to define a task in the simplified representation in RAE environment.
pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task
(lambda args
(let* ((label (car args))
      (p_expr (cdr args))
      (params (car (unzip p_expr))))
     `(list ,label
        (quote ,p_expr)
        (lambda ,params
            ,(cons 'rae-exec-task (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a state function in RAE environment.
pub const MACRO_GENERATE_STATE_FUNCTION: &str = "(defmacro generate-state-function (lambda args
    (let* ((label (car args))
          (p_expr (cdr args))
          (params (car (unzip p_expr)))
          (params
            (if (null? params)
                nil
                (sublist
                   params
                   0
                   (- (len params)1)))))
        `(list ,label
            (quote ,p_expr)
            (lambda ,params
                    ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define an action in RAE environment.
pub const MACRO_GENERATE_ACTION: &str = "(defmacro generate-action
    (lambda args
        (let* ((label (car args))
               (p_expr (cdr args))
               (p_unzip (unzip p_expr))
               (params (car p_unzip))
               (types (cadr p_unzip)))
             `(list ,label
                 (quote ,p_expr)
                 (lambda ,params
                    (await ,(cons 'rae-exec-command (cons `(quote ,label) params))))))))";

pub const MACRO_GENERATE_ACTION_MODEL: &str = "
(defmacro generate-action-model
    (lambda (label def)
        (let* ((p_expr (cdar def))
               (p_unzip (unzip p_expr))
               (params (car p_unzip))
               (conds (cadr (get def 1)))
               (effs (cadr (get def 2))))
              `(list ,label (lambda ,params
                    (do
                        ,(gtpc p_expr)
                        ;(check ,conds)
                        ,conds
                        ,effs))))))";

pub const MACRO_GENERATE_ACTION_OPERATIONAL_MODEL: &str =
    "(defmacro generate-action-operational-model
    (lambda (label def)
        (let* ((p_expr (cdar def))
               (body (cadr (get def 1)))
               (p_unzip (unzip p_expr))
               (params (car p_unzip)))

              `(list ,label (lambda ,params
                    (do
                        ,(gtpc p_expr)
                        ,body))))))";

/// Macro used to generate code to define a method in REA environment.
pub const MACRO_GENERATE_METHOD: &str = "(defmacro generate-method
    (lambda (m_label def)
        (let ((t_label (cadar def))
            (p_expr (cdr (get def 1)))
            (conds (cadr (get def 2)))
            (score (cadr (get def 3)))
            (body (cadr (get def 4))))

            (let* ((p_unzip (unzip p_expr))
                (params (car p_unzip))
                (types (cadr p_unzip)))

            `(list ,m_label
                (quote ,t_label)
                (quote ,p_expr)
                ;lambda for preconditons
                (lambda ,params
                    (do
                        ,(gtpc p_expr)
                        ,conds))
                (lambda ,params ,score)
                (lambda ,params ,body))))))";

pub const GENERATE_TYPE_TEST_EXPR: &str = "generate-type-test-expr";
pub const F_AND_EFFECT: &str = "f-and-effect";
pub const F_AND_COND: &str = "f-and-cond";

pub const LAMBDA_GENERATE_TYPE_PRE_CONDITIONS: &str =
    "(define gtpc (lambda (l) (parse (generate-type-test-expr l))))";

pub const MACRO_AND_COND: &str = "(defmacro and-cond (lambda args (parse (f-and-cond args))))";
pub const MACRO_AND_EFFECT: &str =
    "(defmacro and-effect (lambda args (parse (f-and-effect args))))";

pub const RAE_CONVERT_EXPR: &str = "convert-expr";
pub const RAE_CONVERT_DOMAIN: &str = "convert-domain";
pub const RAE_PRE_PROCESS_LAMBDA: &str = "pre-process-lambda";
pub const RAE_PRE_PROCESS_EXPR: &str = "pre-process-expr";
pub const RAE_PRE_PROCESS_DOMAIN: &str = "pre-process-domain";
pub const RAE_CONVERT_COND_EXPR: &str = "convert-cond-expr";
pub const RAE_PLAN_TASK: &str = "plan-task";

pub const RAE_TRIGGER_EVENT: &str = "trigger-event";
pub const RAE_TRIGGER_TASK: &str = "trigger-task";
pub const RAE_LAUNCH: &str = "launch";
pub const RAE_CONFIGURE_PLATFORM: &str = "configure-platform";
pub const RAE_CONFIGURE_SELECT: &str = "configure-select";

pub const DOC_RAE_TRIGGER_EVENT: &str = "Sends to RAE an event to handle";
pub const DOC_RAE_TRIGGER_EVENT_VERBOSE: &str = "";
pub const DOC_RAE_TRIGGER_TASK: &str = "Sends to RAE a task to execute";
pub const DOC_RAE_TRIGGER_TASK_VERBOSE: &str = "Example: (rae-trigger-task t_dumber robot0)";
pub const DOC_RAE_CONFIGURE_PLATFORM: &str =
    "Set the options of the platform when it will be runned";
