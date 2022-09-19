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

pub const RAE_READ_STATE: &str = "read-state";

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
pub const RAE_GET_RESOURCES: &str = "get-resources";
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
pub const RAE_GET_SELECT: &str = "get-select";
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

pub const ACQUIRE: &str = "acquire";
pub const RELEASE: &str = "release";
pub const NEW_RESOURCE: &str = "new-resource";
pub const IS_LOCKED: &str = "locked?";
pub const LOCKED: &str = "locked";
pub const ACQUIRE_LIST: &str = "acquire-list";
pub const ACQUIRE_IN_LIST: &str = "acquire-in-list";

pub const RAE_EXEC_COMMAND: &str = "exec-command";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_OPEN_COM_PLATFORM: &str = "rae-open-com-platform";
pub const RAE_START_PLATFORM: &str = "rae-start-platform";
pub const RAE_IS_PLATFORM_DEFINED: &str = "rae-platform?";

pub const REFINE: &str = "refine";
pub const RETRY: &str = "retry";

pub const ERR_NO_APPLICABLE_METHOD: &str = "err::no-applicable-method";
pub const ERR_ACTION_FAILURE: &str = "err::action_failure";

pub const MACRO_SIM_BLOCK: &str = "(defmacro sim_block (lambda (body)
`(begin
    (define state (rae-get-facts))
    (define rae-mode simu-mode)
    ,body)))";
pub const GET_ACTION_MODEL: &str = "get-action-model";

pub const DEFINE_ERR_NO_APPLICABLE_METHOD: &str = "(define err::no-applicable-method 0)";
pub const DEFINE_ERR_ACTION_FAILURE: &str = "(define err::action_failure 1)";
pub const MOD_RAE_DESCRIPTION: &str = "rae-description";

pub const RAE_ADD_STATE_FUNCTION: &str = "add-state-function";
pub const RAE_ADD_COMMAND: &str = "add-command";
pub const RAE_ADD_COMMAND_MODEL: &str = "add-command-model";
pub const RAE_ADD_TASK_MODEL: &str = "add-task-model";
pub const RAE_ADD_METHOD_MODEL: &str = "add-method-model";
//pub const RAE_DEF_ACTION_OPERATIONAL_MODEL: &str = "def-action-operational-model";
pub const RAE_ADD_TASK: &str = "add-task";
pub const RAE_ADD_METHOD: &str = "add-method";
pub const RAE_ADD_LAMBDA: &str = "add-lambda";
pub const RAE_ADD_FACTS: &str = "add-facts";
pub const RAE_ADD_CONSTANT: &str = "add-constant";
pub const RAE_ADD_TYPE: &str = "add-type";
pub const RAE_ADD_OBJECT: &str = "add-object";
pub const RAE_ADD_OBJECTS: &str = "add-objects";
pub const RAE_ADD_TYPES: &str = "add-types";
//pub const RAE_DEF_CONSTANTS: &str = "def-constants";

pub const DOC_ADD_STATE_FUNCTION: &str = "Insert a state function in RAE environment.";
pub const DOC_ADD_STATE_FUNCTION_VERBOSE: &str =
    "Example:\n(def-state-function robot.coordinates ?r)";
pub const DOC_ADD_ACTION: &str = "Insert an action in RAE environment.";
pub const DOC_ADD_ACTION_VERBOSE: &str = "Example:\n(def-action pick ?r)";
pub const DOC_ADD_TASK: &str = "Insert a task in RAE environment";
pub const DOC_ADD_TASK_VERBOSE: &str = "Example:\n(def-task t_navigate_to ?r ?x ?y)";
pub const DOC_ADD_METHOD: &str = "Insert a method in RAE environment.";
pub const DOC_ADD_METHOD_VERBOSE: &str =
    "Example:\n(def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin\n\
        \t(rae-await (navigate_to ?r ?x ?y))\n\
        \t(rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))";
pub const DOC_ADD_LAMBDA: &str = "Add a lambda to RAE environment";
pub const DOC_ADD_INITIAL_STATE: &str = "Add initial facts in the state.\
Most of the time it is general knowledge and not initialisation of facts.";

/*/// Macro used to generate code to define a task in the simplified representation in RAE environment.
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
                (lambda ,params ,body))))))";*/

pub const GENERATE_TEST_TYPE_EXPR: &str = "generate_test_type_expr";
/*pub const F_AND_EFFECT: &str = "f-and-effect";
pub const F_AND_COND: &str = "f-and-cond";*/

/*pub const LAMBDA_GENERATE_TYPE_PRE_CONDITIONS: &str =
    "(define gtpc (lambda (l) (parse (generate-type-test-expr l))))";

pub const MACRO_AND_COND: &str = "(defmacro and-cond (lambda args (parse (f-and-cond args))))";
pub const MACRO_AND_EFFECT: &str =
    "(defmacro and-effect (lambda args (parse (f-and-effect args))))";*/

pub const RAE_CONVERT_EXPR: &str = "convert-expr";
pub const RAE_CONVERT_DOMAIN: &str = "convert-domain";
pub const RAE_PRE_PROCESS_LAMBDA: &str = "pre-process-lambda";
pub const RAE_PRE_PROCESS_EXPR: &str = "pre-process-expr";
pub const RAE_PRE_PROCESS_DOMAIN: &str = "pre-process-domain";
pub const RAE_CONVERT_COND_EXPR: &str = "convert-cond-expr";
pub const RAE_PLAN_TASK: &str = "plan-task";

pub const RAE_TRIGGER_EVENT: &str = "trigger-event";
pub const RAE_TRIGGER_TASK: &str = "trigger-task";
pub const RAE_ADD_TASK_TO_EXECUTE: &str = "add-task-to-execute";
pub const RAE_LAUNCH: &str = "launch";
pub const RAE_STOP: &str = "stop";
pub const RAE_CONFIGURE_PLATFORM: &str = "configure-platform";
pub const RAE_SET_SELECT: &str = "set-select";

pub const DOC_RAE_TRIGGER_EVENT: &str = "Sends to RAE an event to handle";
pub const DOC_RAE_TRIGGER_EVENT_VERBOSE: &str = "";
pub const DOC_RAE_TRIGGER_TASK: &str = "Sends to RAE a task to execute";
pub const DOC_RAE_TRIGGER_TASK_VERBOSE: &str = "Example: (rae-trigger-task t_dumber robot0)";
pub const DOC_RAE_CONFIGURE_PLATFORM: &str =
    "Set the options of the platform when it will be runned";

pub const RAE_TASK_METHODS_MAP: &str = "rae-task-methods-map";
pub const RAE_TASK_LIST: &str = "rae-task-list";
pub const RAE_METHOD_LIST: &str = "rae-methods-list";
pub const RAE_ACTION_LIST: &str = "rae-actions-list";
pub const RAE_STATE_FUNCTION_LIST: &str = "rae-state-function-list";
pub const RAE_SYMBOL_TYPE: &str = "rae-symbol-type";
pub const RAE_METHOD_TYPES_MAP: &str = "rae-method-types-map";
pub const RAE_METHOD_SCORE_MAP: &str = "rae-method-score-map";
pub const RAE_METHOD_GENERATOR_MAP: &str = "rae-method-generator-map";
pub const RAE_METHOD_PRE_CONDITIONS_MAP: &str = "rae-method-pre-conditions-map";
pub const RAE_ACTION_MODEL_MAP: &str = "rae-action-model-map";
pub const ACTION_TYPE: &str = "action_type";
pub const TASK_TYPE: &str = "task_type";
pub const METHOD_TYPE: &str = "method_type";
pub const STATE_FUNCTION_TYPE: &str = "state_function_type";
pub const LAMBDA_TYPE: &str = "lambda_type";

pub const TUPLE_TYPE: &str = "tuple";
pub const TYPE_LIST: &str = "tlist";

pub const GREEDY: &str = "greedy";
pub const PLANNING: &str = "planning";
pub const HEURISTIC: &str = "heuristic";
pub const LEARNING: &str = "learning";
pub const ARIES: &str = "aries";
pub const ARIES_OPT: &str = "aries-opt";
pub const UPOM: &str = "upom";
pub const RAE_PLAN: &str = "rae-plan";
pub const C_CHOICE: &str = "c-choice";
