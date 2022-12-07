//manage facts

pub mod exec {
    pub mod domain {
        pub const CTX_DOMAIN: &str = "ContextDomain";
        pub const DOC_CTX_DOMAIN: &str =
            "Context that contains the domain describing the platform.";
    }

    pub mod mode {
        pub const CTX_MODE: &str = "ContextMode";
        pub const DOC_CTX_MODE: &str =
            "Context that contains the mode of OMPAS, either simu of exec.";
    }

    pub mod planning {
        pub const CTX_PLANNING: &str = "ContextPlanning";
        pub const DOC_CTX_PLANNING: &str = "Context used to plan the execution.";
    }

    pub mod ompas {
        pub const CTX_OMPAS: &str = "ContextOMPAS";
        pub const DOC_CTX_OMPAS: &str =
            "Context that contains all shared structures used for the concurrent execution.";
    }

    pub mod state {
        pub const CTX_STATE: &str = "ContextState";
        pub const DOC_CTX_STATE: &str = "Context that contains the state of the world.";
    }

    pub mod task {
        pub const MOD_TASK: &str = "task";
        pub const DOC_MOD_TASK: &str =
            "Module that contains the Task Context that contains the id of the parent task.";

        pub const DEFINE_PARENT_TASK: &str = "define-parent-task";
        pub const DOC_DEFINE_PARENT_TASK: &str = "Set in the Task context the parent of the task.";
    }

    pub const ARBITRARY: &str = "arbitrary";
    pub const DOC_ARBITRARY: &str = "Takes a list of ";
    pub const ASSERT: &str = "assert";
    pub const ASSERT_SHORT: &str = "+>";
    //pub const RAE_RETRACT: &str = "retract";
    //pub const RAE_RETRACT_SHORT: &str = "->";
    pub const INSTANCE: &str = "instance";
    pub const INSTANCES: &str = "instances";
    //pub const AWAIT: &str = "rae-await";
    pub const WAIT_FOR: &str = "__wait_for__";
    pub const LAMBDA_WAIT_FOR: &str = "(lambda (e)
    (u! 
        (await-interrupt (__wait_for__ e))))";

    pub const MONITOR: &str = "monitor";
    pub const LAMBDA_MONITOR: &str = "(lambda (e)
        (wait-for `(= ,e nil)))";

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

    pub const __ACQUIRE__: &str = "__acquire__";
    pub const DOC___ACQUIRE__: &str = "";

    pub const LAMBDA_ACQUIRE: &str = "(define acquire (lambda __args__
    (u!
        (await-interrupt (enr (cons '__acquire__ __args__))))))";

    pub const ACQUIRE: &str = "acquire";
    pub const DOC_ACQUIRE: &str = "DOC_ACQUIRE";

    pub const __ACQUIRE_IN_LIST__: &str = "__acquire_in_list__";
    pub const LAMBDA_ACQUIRE_IN_LIST: &str = "(define acquire-in-list (lambda __args__
    (u!
        (await-interrupt (enr (cons '__acquire_in_list__ __args__))))))";
    pub const ACQUIRE_IN_LIST: &str = "acquire-in-list";
    pub const RELEASE: &str = "release";
    pub const NEW_RESOURCE: &str = "new-resource";
    pub const IS_LOCKED: &str = "locked?";
    pub const LOCKED: &str = "locked";
    pub const RESOURCES: &str = "resources";

    pub const RAE_EXEC_TASK: &str = "exec-task";
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
}
pub mod monitor {
    pub mod user_interface {
        pub const MOD_USER_INTERFACE: &str = "user_interface";
        pub const DOC_MOD_USER_INTERFACE: &str =
            "Collection of functions to monitor the execution of the system.";

        pub const GET_STATE: &str = "get-state";
        pub const DOC_GET_STATE: &str = "Returns the current state";

        pub const GET_CONFIG_PLATFORM: &str = "get-config-platform";
        pub const DOC_GET_CONFIG_PLATFORM: &str =
            "Return the actual value of the config of the platform";

        pub const GET_SELECT: &str = "get-select";
        pub const DOC_GET_SELECT: &str = "Return the select algorithm used.";

        pub const GET_TASK_NETWORK: &str = "get-task-network";
        pub const DOC_GET_TASK_NETWORK: &str = "Return the actual task network.";

        pub const GET_TYPE_HIERARCHY: &str = "get-type-hierarchy";
        pub const DOC_GET_TYPE_HIERARCHY: &str = "Return the type hierarchy defined in the domain.";

        pub const GET_AGENDA: &str = "get-agenda";
        pub const DOC_GET_AGENDA: &str = "Return the actual agenda with the status of all tasks.";
        pub const DOC_GET_AGENDA_VERBOSE: &str= "The agenda can be filtered in function of the kind\
        of the action {task, command} and their status {pending, accepted, rejected, success, failure, cancelled}";

        pub const GET_RESOURCES: &str = "get-resources";
        pub const DOC_GET_RESOURCES: &str =
            "Return the resources defined in the system and their waiting list.";

        pub const GET_MONITORS: &str = "get-monitors";
        pub const DOC_GET_MONITORS: &str =
            "Return the list of all dynamic expressions currently checked.";

        pub const GET_COMMANDS: &str = "get-commands";
        pub const DOC_GET_COMMANDS: &str = "Return the list of all commands defined in the domain.";

        pub const GET_TASKS: &str = "get-tasks";
        pub const DOC_GET_TASKS: &str = "Return the list of all commands defined in the domain.";

        pub const GET_METHODS: &str = "get-methods";
        pub const DOC_GET_METHODS: &str = "Return the list of all methods defined in the domain.";

        pub const GET_STATE_FUNCTIONS: &str = "get-state-functions";
        pub const DOC_GET_STATE_FUNCTIONS: &str =
            "Return the list of all state-functions defined in the domain.";

        pub const GET_DOMAIN: &str = "get-domain";
        pub const DOC_RAE_GET_ENV: &str = "Return the domain.";

        /*pub const GET_SYMBOL_TYPE: &str = "get-symbol-type";
        pub const DOC_GET_SYMBOL_TYPE: &str =
            "Return the type of the symbol as defined in the domain: state-function, command, task, method.";*/

        /*pub const GET_STATUS: &str = "get-status";
        pub const DOC_GET_STATUS: &str = "Returns the current status of actions";*/

        pub const GET_STATS: &str = "get-stats";
        pub const DOC_GET_STATS: &str = "Return the statistics of the execution of the system";

        pub const EXPORT_STATS: &str = "export-stats";
        pub const DOC_EXPORT_STATS: &str = "Export the statistics in csv format in a given file.";
    }

    pub mod log {
        pub const MOD_LOG: &str = "log";
        pub const DOC_MOD_LOG: &str = "Collection of functions control the logger.";

        pub const LOG_ROOT: &str = "log-root";
        pub const LOG_PLATFORM: &str = "log-platform";
        pub const LOG_OMPAS: &str = "log-ompas";

        pub const ACTIVATE_LOG: &str = "activate_log";
        pub const DOC_ACTIVATE_LOG: &str =
            "Create a new window where the log of the given topic will be printed.\
         Logs = {log-root, log-platform, log-ompas}";

        pub const DEACTIVATE_LOG: &str = "deactivate_log";
        pub const DOC_DEACTIVATE_LOG: &str= "Kill the windows where the log was printed. Logs = {log-root, log-platform, log-ompas}";

        pub const SET_LOG_LEVEL: &str = "set_log_level";
        pub const DOC_SET_LOG_LEVEL: &str = "Set the log level used by the system to filter logs.\
        LogLevel = {error, warn, info, debug, trace}.";

        pub const GET_LOG_LEVEL: &str = "get_log_level";
        pub const DOC_GET_LOG_LEVEL: &str =
            "Return the actual log level used by the system to filter logs.\
        LogLevel = {error, warn, info, debug, trace}.";
    }

    pub mod planning {}

    pub mod domain {

        pub const MOD_DOMAIN: &str = "domain";
        pub const DOC_MOD_DOMAIN: &str =
            "Collection of functions and macros to define the domain of the platform.";

        pub const ADD_STATE_FUNCTION: &str = "add-state-function";

        pub const ADD_COMMAND: &str = "add-command";

        pub const ADD_COMMAND_MODEL: &str = "add-command-model";

        pub const ADD_TASK_MODEL: &str = "add-task-model";

        pub const ADD_METHOD_MODEL: &str = "add-method-model";

        pub const ADD_TASK: &str = "add-task";

        pub const ADD_METHOD: &str = "add-method";

        pub const ADD_LAMBDA: &str = "add-lambda";

        pub const ADD_FACTS: &str = "add-facts";

        pub const ADD_CONSTANT: &str = "add-constant";

        pub const ADD_TYPE: &str = "add-type";

        pub const ADD_OBJECT: &str = "add-object";

        pub const ADD_OBJECTS: &str = "add-objects";

        pub const ADD_TYPES: &str = "add-types";

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

        //Macros
        pub const MACRO_DEF_COMMAND: &str = "(defmacro def-command
    (lambda attributes
        (let ((label (car attributes))
                (attributes (cdr attributes)))

        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(add-command (map 
                (quote ,(cons (cons ':name label) (__l__ attributes)))))))))";

        pub const MACRO_DEF_STATE_FUNCTION: &str = "(defmacro def-state-function
    (lambda attributes
        (let ((label (car attributes))
                (attributes (cdr attributes)))

        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(add-state-function (map 
                (quote ,(cons (cons ':name label) (__l__ attributes)))))))))";

        pub const MACRO_DEF_METHOD: &str = "(defmacro def-method
    (lambda attributes
        (let ((label (car attributes))
                (attributes (cdr attributes)))

        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(add-method (map 
                (quote ,(cons (cons ':name label) (__l__ attributes)))))))))";

        pub const MACRO_DEF_TASK: &str = "(defmacro def-task
    (lambda attributes
        (let ((label (car attributes))
                (attributes (cdr attributes)))

        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(add-task (map 
                (quote ,(cons (cons ':name label) (__l__ attributes)))))))))";

        pub const MACRO_DEF_LAMBDA: &str = "(defmacro def-lambda
    (lambda (label lambda)
            `(add-lambda ',label ',lambda)))";

        pub const MACRO_PDDL_MODEL: &str = "(defmacro pddl-model
    (lambda args
        (let ((label (car args))
               (args (cdr args)))
            (begin
                (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
                `(map 
                    (quote ,(append (cons (list ':name label) (cons '(:model-type pddl) nil )) (__l__ args))))))))";

        pub const MACRO_OM_MODEL: &str = "(defmacro om-model
    (lambda args
        (let ((label (car args))
               (args (cdr args)))
            (begin
                (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
                `(map 
                    (quote ,(append (cons (list ':name label) (cons '(:model-type om) nil )) (__l__ args))))))))";

        pub const MACRO_DEF_COMMAND_OM_MODEL: &str = "(defmacro def-command-om-model
    (lambda args
        `(add-command-model ,(cons om-model args))))";

        pub const MACRO_DEF_COMMAND_PDDL_MODEL: &str = "(defmacro def-command-pddl-model
    (lambda args
        `(add-command-model ,(cons pddl-model args))))";

        pub const MACRO_DEF_TASK_OM_MODEL: &str = "(defmacro def-task-om-model
    (lambda args
        `(add-task-model ,(cons om-model args))))";

        pub const MACRO_DEF_TASK_PDDL_MODEL: &str = "(defmacro def-task-pddl-model
    (lambda args
        `(add-task-model ,(cons pddl-model args))))";

        pub const MACRO_DEF_INITIAL_STATE: &str = "(defmacro def-initial-state (lambda args
    `(add-facts (map ',args))))";

        pub const MACRO_DEF_TYPES: &str = "(defmacro def-types (lambda args
    (cons 'add-types (quote-list args))))";
        pub const MACRO_DEF_OBJECTS: &str = "(defmacro def-objects (lambda args
    (cons 'add-objects (quote-list args))))";

        //Fields in the description
        pub const NAME: &str = ":name";
        pub const TASK: &str = ":task";
        pub const PARAMETERS: &str = ":params";
        pub const PRE_CONDITIONS: &str = ":pre-conditions";
        pub const BODY: &str = ":body";
        pub const MODEL: &str = ":model";
        pub const MODEL_TYPE: &str = ":model-type";
        pub const EFFECTS: &str = ":effects";
        pub const RESULT: &str = ":result";
        pub const SCORE: &str = ":score";
        pub const COST: &str = ":cost";
    }
}

pub const GENERATE_TEST_TYPE_EXPR: &str = "generate_test_type_expr";

pub mod conversion {
    pub const RAE_CONVERT_EXPR: &str = "convert-expr";
    pub const RAE_CONVERT_DOMAIN: &str = "convert-domain";
    pub const RAE_PRE_PROCESS_LAMBDA: &str = "pre-process-lambda";
    pub const RAE_PRE_PROCESS_EXPR: &str = "pre-process-expr";
    pub const RAE_PRE_PROCESS_DOMAIN: &str = "pre-process-domain";
    pub const RAE_CONVERT_COND_EXPR: &str = "convert-cond-expr";
    pub const RAE_PLAN_TASK: &str = "plan-task";
}

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

pub mod select {
    pub const GREEDY: &str = "greedy";
    pub const PLANNING: &str = "planning";
    pub const HEURISTIC: &str = "heuristic";
    pub const LEARNING: &str = "learning";
    pub const ARIES: &str = "aries";
    pub const ARIES_OPT: &str = "aries-opt";
    pub const UPOM: &str = "upom";
    pub const RAE_PLAN: &str = "rae-plan";
    pub const C_CHOICE: &str = "c-choice";
}

pub mod process {
    pub const PROCESS_TOPIC_OMPAS: &str = "__PROCESS_TOPIC_OMPAS__";
    pub const LOG_TOPIC_OMPAS: &str = "__LOG_TOPIC_OMPAS__";
    pub const OMPAS: &str = "OMPAS";
}
