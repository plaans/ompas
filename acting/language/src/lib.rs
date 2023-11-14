//manage facts

pub mod exec {

    pub mod c_choice {

        pub const MOD_C_CHOICE: &str = "mod_c_choice";
        pub const DOC_MOD_C_CHOICE: &str =
            "Collection of functions to ease c_choice to refine a task.";

        pub const INCREASE_COST: &str = "increase_cost";
        pub const DOC_INCREASE_COST: &str = "Increase cost of the branch.";
        pub const C_CHOICE: &str = "c_choice";
        pub const DOC_C_CHOICE: &str = "Return a method in function of the computed score.";

        //keywords
        pub const INF: &str = "inf";
    }

    pub mod rae_plan {
        pub const MOD_RAE_PLAN: &str = "mod_rae_plan";
        pub const DOC_MOD_RAE_PLAN: &str =
            "Collection of functions to ease rae_plan to refine tasks.";

        pub const COMPOSE_EFFICIENCY: &str = "compose_efficiency";
        pub const DOC_COMPOSE_EFFICIENCY: &str = "Compose two efficiencies together.";

        pub const RAE_PLAN: &str = "rae_plan";
        pub const DOC_RAE_PLAN: &str = "Return a method in function of its efficiency.";

        //keywords
        pub const INF: &str = "inf";
    }

    pub mod aries {
        pub const CTX_ARIES: &str = "aries";
        pub const SELECT_ARIES: &str = "select_aries";
    }

    pub const MOD_EXEC: &str = "exec";
    pub const DOC_MOD_EXEC: &str = "Collection of functions of the acting language.";

    pub const ARBITRARY: &str = "arbitrary";
    pub const DOC_ARBITRARY: &str = "Takes a list of element and arbitrarily return one of them, either a random, using an optional function or using the acting engine.";

    pub mod platform {
        pub const MOD_PLATFORM: &str = "platform";
        pub const DOC_MOD_PLATFORM: &str = "Collection of functions using the robotic platform.";

        pub const EXEC_COMMAND: &str = "exec-command";
        pub const LAMBDA_EXEC_COMMAND: &str = "(lambda args (await (apply _exec_command args)))";
        pub const DOC_EXEC_COMMAND: &str = "Lambda to execute a command on the platform.";

        pub const _EXEC_COMMAND: &str = "_exec_command";
        pub const DOC__EXEC_COMMAND: &str = "Send a execution request to the platform.";

        pub const CANCEL_COMMAND: &str = "cancel-command";
        pub const DOC_CANCEL_COMMAND: &str = "Send a cancel request to the platform.";

        pub const START_PLATFORM: &str = "start-platform";
        pub const DOC_START_PLATFORM: &str = "Start the platform process in a new thread";
        //pub const STOP_PLATFORM: &str = "stop-platform";

        pub const IS_PLATFORM_DEFINED: &str = "rae-platform?";
        pub const DOC_IS_PLATFORM_DEFINED: &str = "Return true if a platform is defined.";
    }

    pub mod domain {
        pub const MOD_DOMAIN: &str = "ContextDomain";
        pub const DOC_MOD_DOMAIN: &str =
            "Context that contains the domain describing the platform.";
    }

    pub mod mode {
        pub const CTX_MODE: &str = "ContextMode";
        pub const DOC_CTX_MODE: &str =
            "Context that contains the mode of OMPAS, either simu of exec.";

        //keywords

        pub const DEFINE_RAE_MODE: &str = "(define rae-mode EXEC-MODE)";
        pub const SYMBOL_EXEC_MODE: &str = "exec-mode";
        pub const SYMBOL_SIMU_MODE: &str = "simu-mode";
        pub const SYMBOL_RAE_MODE: &str = "rae-mode";
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

    pub mod resource {
        pub const MOD_RESOURCE: &str = "resource";
        pub const DOC_MOD_RESOURCE: &str = "Collection of function to manipulate resource.";

        pub const NEW_RESOURCE: &str = "new-resource";
        pub const DOC_NEW_RESOURCE: &str = "Declare a new resource with an optional capacity.";
        pub const DOC_NEW_RESOURCE_VERBOSE: &str = "Example: (new-resource r1); atomic resource\n\
        (new-resource battery 50); divisible resource";

        pub const __ACQUIRE__: &str = "__acquire__";
        pub const DOC___ACQUIRE__: &str = "Acquire a resource with an optional quantity.";

        pub const __ACQUIRE_IN_LIST__: &str = "__acquire_in_list__";
        pub const DOC___ACQUIRE_IN_LIST__: &str = "Acquire on of the element of the list.";

        pub const RELEASE: &str = "release";
        pub const DOC_RELEASE: &str = "Release the resource acquired behind the resource handle.";

        pub const IS_LOCKED: &str = "locked?";
        pub const DOC_IS_LOCKED: &str = "Return true if a resource is locked.";

        pub const RESOURCES: &str = "resources";
        pub const DOC_RESOURCES: &str = "Return the list of resources with their waiting list.";

        //Lambdas
        pub const ACQUIRE: &str = "acquire";
        pub const DOC_ACQUIRE: &str =
            "Wrapper around __acquire__ to make it a blocking interruptible.";
        pub const LAMBDA_ACQUIRE: &str = "(lambda __args__
    (u!
        (await-interrupt (enr (cons '__acquire__ __args__)))))";
        pub const ACQUIRE_IN_LIST: &str = "acquire-in-list";
        pub const DOC_ACQUIRE_IN_LIST: &str =
            "Wrapper around __acquire_in_list__ to make it a blocking interruptible.";
        pub const LAMBDA_ACQUIRE_IN_LIST: &str = "(lambda __args__
    (u!
        (await-interrupt (enr (cons '__acquire_in_list__ __args__)))))";

        //Keywords
        pub const PRIORITY: &str = ":priority";
        pub const LOCKED: &str = "locked";
        pub const MAX_Q: &str = "max-q";
        pub const QUANTITY: &str = "quantity";
        pub const MAX_QUANTITY: i64 = 1000;
    }

    pub mod state {
        pub const MOD_STATE: &str = "state";
        pub const DOC_MOD_STATE: &str = "Collection of functions to read and write state.";

        pub const ASSERT: &str = "assert";
        pub const DOC_ASSERT: &str = "Insert a fact in the inner world.";

        pub const ASSERT_SHORT: &str = "+>";
        pub const DOC_ASSERT_SHORT: &str = "Short version of assert.";

        pub const EFFECT: &str = "effect";
        pub const DOC_EFFECT: &str = "Same as assert.";

        pub const TRANSITIVE_EFFECT: &str = "transitive-effect";
        pub const DOC_TRANSITIVE_EFFECT: &str= "Update of a fact that takes a given duration. During the transition, the fact is considered as unknown.";

        pub const RETRACT: &str = "retract";
        pub const DOC_RETRACT: &str = "Remove a fact from the inner world.";

        pub const RETRACT_SHORT: &str = "->";
        pub const DOC_RETRACT_SHORT: &str = "Short version of retract.";

        pub const READ_STATE: &str = "read-state";
        pub const DOC_READ_STATE: &str = "Read a state variable.";

        pub const READ_STATIC_STATE: &str = "read-static-state";
        pub const DOC_READ_STATIC_STATE: &str = "Read the value of a static state variable.";

        pub const GET_STATE: &str = "get-state";
        pub const DOC_GET_STATE: &str = "Return the state as a map";

        pub const INSTANCE: &str = "instance";
        pub const DOC_INSTANCE: &str = "Check is an objects is of a certain type.";

        pub const INSTANCES: &str = "instances";
        pub const DOC_INSTANCES: &str = "Return all elements of a type.";
        //pub const AWAIT: &str = "rae-await";
        pub const __WAIT_FOR__: &str = "__wait_for__";
        pub const DOC___WAIT_FOR__: &str = "Wait until a dynamic expression becomes true.";

        pub const WAIT_FOR: &str = "wait-for";
        pub const DOC_WAIT_FOR: &str =
            "Wrapper around __wait_for__ to have a blocking interruptible.";
        pub const LAMBDA_WAIT_FOR: &str = "(lambda (e)
    (u! 
        (await-interrupt (__wait_for__ e))))";

        pub const MONITOR: &str = "monitor";
        pub const DOC_MONITOR: &str = "Wait until an dynamic expression becomes false.";
        pub const LAMBDA_MONITOR: &str = "(lambda (e)
        (wait-for `(= ,e nil)))";

        pub const RUN_MONITORING: &str = "run-monitoring";
        pub const DOC_RUN_MONITORING: &str= "Execute an expression until either it finishes or the dynamic expression becomes false.";
        pub const MACRO_RUN_MONITORING: &str = "(lambda (b m)
        `(race ,b (monitor ,m)))";

        //keywords
        pub const STATIC: &str = "static";
        pub const DYNAMIC: &str = "dynamic";
        pub const INNER_STATIC: &str = "inner_static";
        pub const INNER_DYNAMIC: &str = "inner_dynamic";

        pub const UNKNOWN: &str = "unk";
    }

    pub mod acting_context {
        pub const MOD_ACTING_CONTEXT: &str = "acting_context";
        pub const DOC_MOD_CONTEXT: &str =
            "Module that contains the Task Context that contains the id of the parent task.";

        pub const DEF_PROCESS_ID: &str = "def_process_id";
        pub const DOC_DEF_PROCESS_ID: &str = "Define the id of the process";

        pub const DEF_LABEL: &str = "def-label";
        pub const DOC_DEF_LABEL: &str = "Define the label of the context";

        pub const CTX_ARBITRARY: &str = "ctx-arbitrary";
        pub const DOC_CTX_ARBITRARY: &str = "Evaluates an arbitrary in an assigned context";
        pub const LAMBDA_CTX_ARBITRARY: &str = "(lambda args
            (begin
                (def-label 'arbitrary (car args))
                (enr (cons arbitrary (cdr args)))
            )
        )";
        pub const CTX_ACQUIRE: &str = "ctx-acquire";
        pub const DOC_CTX_ACQUIRE: &str = "Evaluates an acquire in an assigned context";
        pub const LAMBDA_CTX_ACQUIRE: &str = "(lambda args
            (begin
                (def-label 'acquire (car args))
                (enr (cons acquire (cdr args)))
            )
        )";

        pub const CTX_EXEC_COMMAND: &str = "ctx-exec-command";
        pub const DOC_CTX_EXEC_COMMAND: &str = "Evaluates a command in an assigned context";
        pub const LAMBDA_CTX_EXEC_COMMAND: &str = "(lambda args
            (begin
                (def-label 'command (car args))
                (enr (cons exec-command (cdr args)))
            )
        )";

        pub const CTX_EXEC_TASK: &str = "ctx-exec-task";
        pub const DOC_CTX_EXEC_TASK: &str = "Evaluates a task in an assigned context";
        pub const LAMBDA_CTX_EXEC_TASK: &str = "(lambda args
            (begin
                (def-label 'task (car args))
                (enr (cons exec-task (cdr args)))
            )
        )";
    }

    pub mod refinement {
        pub const MOD_REFINEMENT: &str = "refinement";
        pub const DOC_MOD_REFINEMENT: &str = "Collection of functions used to execute task.";

        pub const REFINE: &str = "refine";
        pub const DOC_REFINE: &str = "Refine a task and by returning an instantiated method.";

        pub const _RETRY: &str = "_retry";
        pub const DOC__RETRY: &str = "Retry the current task";

        pub const SET_SUCCESS: &str = "set-success-for-task";
        pub const DOC_SET_SUCCESS: &str = "Set the task status as success.";

        pub const IS_SUCCESS: &str = "success?";
        pub const DOC_IS_SUCCESS: &str = "Return true if the LValue is a success";

        pub const IS_FAILURE: &str = "failure?";
        pub const DOC_IS_FAILURE: &str = "Return true if the LValue is a failure";

        //Lambdas
        pub const EXEC_TASK: &str = "exec-task";
        pub const DOC_EXEC_TASK: &str = "Execute a task.";
        pub const LAMBDA_EXEC_TASK: &str = "(lambda __task__
        (begin
            (define _r_ (enr (cons 'refine __task__)))
            (if (err? _r_)
                _r_
                (let ((_m_ (first _r_))
                      (_id_ (second _r_)))
                    (begin
                        (def_process_id _id_)
                        (define r (eval _m_))
                        (if (err? r)
                            (retry r)
                            (set-success-for-task)))))))";

        pub const RETRY: &str = "retry";
        pub const DOC_RETRY: &str = "Retry a given task.";
        pub const LAMBDA_RETRY: &str = "(lambda (r)
	(begin
	    (define _r_ (_retry r))
	    (if (err? _r_)
                _r_
                (let ((_m_ (first _r_))
                      (_id_ (second _r_)))
                    (begin
                        (def_process_id _id_)
                        (define r (eval _m_))
                        (if (err? r)
                            (retry r)
                            (set-success-for-task)))))))";

        pub const __GET_PRECONDITIONS__: &str = "__get_preconditions__";
        pub const DOC___GET_PRECONDITIONS__: &str =
            "Return pre-conditions expression of a given method.";
        pub const LAMBDA___GET_PRECONDITIONS__: &str = "(lambda (__label__)\
        (get __method_pre_conditions_map__ __label__))";

        pub const __GET_SCORE__: &str = "__get_score__";
        pub const DOC___GET_SCORE__: &str = "Return the score expression of a given method.";
        pub const LAMBDA___GET_SCORE__: &str = "(lambda (__label__)\
        (get __method_score_map__ __label__))";

        pub const __GET_COMMAND_MODEL__: &str = "__get_command_model__";
        pub const DOC___GET_COMMAND_MODEL__: &str = "Return the model of a command.";
        pub const LAMBDA___GET_COMMAND_MODEL__: &str = "(lambda (__label__)
        (get __command_model_map__ __label__))";

        pub const __EVAL_PRE_CONDITIONS__: &str = "__eval_pre_conditions__";
        pub const DOC___EVAL_PRE_CONDITIONS__: &str = "Eval the pre-conditions expression.";
        pub const LAMBDA___EVAL_PRE_CONDITIONS__: &str = "(lambda (__method__)
        (sim_block
            (eval (cons (__get_preconditions__ (car __method__)) (quote-list (cdr __method__))))))";

        pub const __COMPUTE_SCORE__: &str = "__compute_score__";
        pub const DOC___COMPUTE_SCORE__: &str = "Eval the score expression.";
        pub const LAMBDA___COMPUTE_SCORE__: &str = "(lambda (__method__)
        (sim_block
            (eval (cons (__get_score__ (car __method__)) (quote-list (cdr __method__))))))";

        pub const IS_APPLICABLE: &str = "applicable?";
        pub const DOC_IS_APPLICABLE: &str = "Return true if a method is applicable.";
        pub const LAMBDA_IS_APPLICABLE: &str = "(lambda (method)
        (sim_block
            (__eval__pre_conditions__ method)))";

        pub const __GENERATE_APPLICABLE_INSTANCES__: &str = "__generate_applicable_instances__";
        pub const DOC___GENERATE_APPLICABLE_INSTANCES__: &str =
            "Generate applicable instances for a given task.";
        pub const LAMBDA___GENERATE_APPLICABLE_INSTANCES__: &str = "(lambda (__task__)
        (let* ((__task_label__ (first __task__))
               (__params__ (cdr __task__))
               (__methods__ (get __tasks_methods_map__ __task_label__)))
            (__r_generate_instances__
                (enr (cons enumerate (cons __methods__ __params__))))))";

        pub const __R_GENERATE_INSTANCES__: &str = "__r_generate_instances__";
        pub const DOC___R_GENERATE_INSTANCES__: &str = "Recursively generate applicable instances";
        pub const LAMBDA___R_GENERATE_INSTANCES__: &str = "(lambda (__methods__)
        (if (null? __methods__)
            nil
            (let* ((__method__ (car __methods__))
                    (__methods__ (cdr __methods__))
                    (__method_label__ (first __method__))
                    (__params__ (cdr __method__)))
                (begin
                    (define __types__ (get __method_types_map__ __method_label__))
                    (if (> (len __types__) (len __params__))
                        (begin
                            (define __instance_types__ (mapf instance (sublist __types__ (len __params__))))
                            (define __instances__ (enr (cons enumerate (append __method__ __instance_types__))))
                            (append (__r_test_method__ __instances__) (__r_generate_instances__ __methods__)))
                        (cons
                            (if (! (err? (__eval_pre_conditions__ __method__)))
                                (list __method__ (__compute_score__ __method__))
                                nil)
                            (r_generate_instances __methods__)))))))";

        pub const __R_TEST_METHOD__: &str = "__r_test_method__";
        pub const DOC___R_TEST_METHOD__: &str = "";
        pub const LAMBDA_R_TEST_METHOD: &str = "(lambda (__instances__)
        (if (null? __instances__)
            nil
            (if (__eval_pre_conditions__ (car __instances__))
                (cons
                    (list (car __instances__) (__compute_score__ (car __instances__)))
                    (__r_test_method__ (cdr __instances__)))
                (__r_test_method___ (cdr __instances__)))))";

        //Keywords
        pub const __TASKS_METHODS_MAP__: &str = "__tasks_methods_map__";
        pub const __TASKS_LIST__: &str = "__tasks_list__";
        pub const __METHODS_LIST__: &str = "__methods_list__";
        pub const __COMMANDS_LIST__: &str = "__commands_list__";
        pub const __STATE_FUNCTION_LIST__: &str = "__state_function_list__";
        pub const __SYMBOL_TYPE__: &str = "__symbol_type__";
        pub const __METHOD_TYPES_MAP__: &str = "__method_types_map__";
        pub const __METHOD_SCORE_MAP__: &str = "__method_score_map__";
        pub const __METHOD_GENERATOR_MAP__: &str = "__method_generator_map__";
        pub const __METHOD_PRE_CONDITIONS_MAP__: &str = "__method_pre_conditions_map__";
        pub const __COMMAND_MODEL_MAP__: &str = "__command_model_map__";
        pub const SUCCESS: &str = "success";
        pub const FAILURE: &str = "failure";
        pub const SELECT: &str = "select";
        pub const GET_NEXT_METHOD: &str = "get-next-method";

        //pub const GET_INSTANTIATED_METHODS: &str = "rae-get-instantiated-methods";
        //pub const GET_BEST_METHOD: &str = "rae-get-best-method";
    }

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

    pub const RAE_SUCCESS: &str = "success";
    pub const LAMBDA_RAE_SUCCESS: &str = "(lambda args
    (begin
        (print args)
        nil))";

    pub const RAE_FAILURE: &str = "failure";
    pub const LAMBDA_RAE_FAILURE: &str = "(lambda args
    (begin
        (print args)
        (err nil)))";
}

pub mod monitor {

    pub const MOD_MONITOR: &str = "monitor";
    pub const DOC_MOD_MONITOR: &str = "Module exposed to the user to configure and launch rae.";

    pub mod debug_conversion {
        pub const MOD_DEBUG_CONVERSION: &str = "debug-conversion";
        pub const DOC_MOD_DEBUG_CONVERSION: &str= "Collection of functions to debug the conversion of SOMPAS expressions and try Aries planning features.";

        pub const CONVERT_DOMAIN: &str = "convert-domain";
        pub const DOC_CONVERT_DOMAIN: &str =
            "Return the planning domain corresponding to the acting domain defined";

        pub const EXPORT_TYPE_LATTICE: &str = "export-type-lattice";
        pub const DOC_EXPORT_TYPE_LATTICE: &str =
            "Exports in a google-chrome page the lattice in a dot form";

        pub const PRE_EVAL_TASK: &str = "pre-eval-task";
        pub const DOC_PRE_EVAL_TASK: &str = "Pre evaluate a task";

        pub const PRE_EVAL_EXPR: &str = "pre-eval-expr";
        pub const DOC_PRE_EVAL_EXPR: &str = "Pre evaluate an expression.";

        pub const ANNOTATE_TASK: &str = "annotate-task";
        pub const DOC_ANNOTATE_TASK: &str =
            "Pre eval a task and annotate each acting process with a unique id";

        pub const TRANSLATE: &str = "translate";
        pub const DOC_TRANSLATE: &str= "Converts an object of the domain, and output a markdown file with explanations of the conversion process.";
    }

    pub mod planning {
        pub const MOD_PLANNING: &str = "planning";
        pub const DOC_MOD_PLANNING: &str =
            "mod used to plan a task regarding the current state of the system.";

        pub const PLAN: &str = "plan";
        pub const DOC_PLAN: &str= "Plan with an optional task using the defined planner with the acting domain defined in the environment";

        pub const PLAN_OPT: &str = "plan-opt";
        pub const DOC_PLAN_OPT: &str= "Plan with an optional task using the defined planner with the acting domain defined in the environment, and returns the optimal solution in terms of makespan";

        pub const NEW_EVENT: &str = "new-event";
        pub const DOC_NEW_EVENT: &str = "Add an event in the planning problem.";
        pub const DOC_NEW_EVENT_VERBOSE: &str = "Example: (new-event alarm true 10.5)";

        pub const NEW_GOAL: &str = "new-goal";
        pub const DOC_NEW_GOAL: &str =
            "Add a goal in the planning problem. The interval is optional";
        pub const DOC_NEW_GOAL_VERBOSE: &str= "Examples: (new-goal '(battery truck) 100)\n(add-goal '(position truck) warehouse '(100 120))";

        pub const NEW_GOAL_TASK: &str = "new-goal-task";
        pub const DOC_NEW_GOAL_TASK: &str = "Add a task in the planning problem.";
        pub const DOC_NEW_GOAL_TASK_VERBOSE: &str =
            "Example: (new-goal-task move r1 bedroom kitchen)";

        pub const NEW_TIMED_GOAL_TASK: &str = "new-timed-goal-task";
        pub const DOC_NEW_TIMED_GOAL_TASK: &str =
            "Add a task in the planning problem. The last arg is the moment the task should start.";
        pub const DOC_NEW_TIMED_GOAL_TASK_VERBOSE: &str =
            "Example: (new-timed-goal-task 10 move r1 bedroom kitchen)";

        pub const GET_GOALS_EVENTS: &str = "get-goals-events";
        pub const DOC_GET_GOALS_EVENTS: &str =
            "Return the list of tasks, goals, and objectives defined in the planning problem.";

        pub const REMOVE_GOAL: &str = "remove-goal";
        pub const DOC_REMOVE_GOAL: &str= "Remove the goal with the given id. Use `get-planning-problem` to get the list of goals and theirs ids.";

        pub const REMOVE_TASK: &str = "remove-task";
        pub const DOC_REMOVE_TASK: &str= "Remove the task with the given id. Use `get-planning-problem` to get the list of tasks and theirs ids.";

        pub const REMOVE_EVENT: &str = "remove-event";
        pub const DOC_REMOVE_EVENT: &str= "Remove the event with the given id. Use `get-planning-problem` to get the list of events and theirs ids.";
    }

    pub mod continuous_planning {
        pub const MOD_CONTINUOUS_PLANNING: &str = "continuous-planning";
        pub const DOC_MOD_CONTINUOUS_PLANNING: &str =
            "Mod to test the continuous planning capabilities of ompas";

        pub const START: &str = "cp.start";
        pub const DOC_START: &str = "Start the continuous planning processes.";

        pub const PLAN: &str = "cp.plan";
        pub const DOC_PLAN: &str = "Plan with the current Acting Tree.";

        pub const NEW_TASK: &str = "cp.new-task";
        pub const DOC_NEW_TASK: &str = "Simulates a new task to face.";

        pub const SET_START: &str = "cp.set-start";
        pub const DOC_SET_START: &str = "Simulates the instantiation of the start of a task.";

        pub const SET_END: &str = "cp.set-end";
        pub const DOC_SET_END: &str = "Simulates the instantiation of the end of a task";

        pub const SET_STATUS: &str = "cp.set-status";
        pub const DOC_SET_STATUS: &str = "Simulates the instantiation of the status of a process";

        pub const NEW_EVENT: &str = "cp.new-event";
        pub const DOC_NEW_EVENT: &str = "Simulates the occurrence of a new event.";
    }

    pub mod model {

        pub const MOD_MODEL: &str = "model";
        pub const DOC_MOD_DOMAIN: &str =
            "Collection of functions and macros to define the domain of the platform.";

        pub const GENERATE_TEST_TYPE_EXPR: &str = "generate_test_type_expr";
        pub const DOC_GENERATE_TEST_TYPE_EXPR : &str ="Generate an expression used in pre-conditions to test types of parameters used for a method as preambule of the body.";

        pub const ADD_STATE_FUNCTION: &str = "add-state-function";
        pub const DOC_ADD_STATE_FUNCTION: &str = "Add a state function to the domain. The state-function is defined by a label, typed parameters and a type for the result.";

        pub const ADD_FUNCTION: &str = "add-function";
        pub const DOC_ADD_FUNCTION: &str = "Add a state function to the domain. The state-function is defined by a label, typed parameters and a type for the result.";

        pub const ADD_COMMAND: &str = "add-command";
        pub const DOC_ADD_COMMAND: &str= "Add a command to the domain. A command is defined by a label, typed parameters and an optional model.";

        pub const ADD_COMMAND_MODEL: &str = "add-command-model";
        pub const DOC_ADD_COMMAND_MODEL: &str = "Add a model to a command.";

        pub const ADD_TASK: &str = "add-task";
        pub const DOC_ADD_TASK: &str= "Add a task to the domain. A task is defined by a label, typed parameters and an optional model.";

        pub const ADD_TASK_MODEL: &str = "add-task-model";
        pub const DOC_ADD_TASK_MODEL: &str = "Add a model to a task.";

        pub const ADD_METHOD: &str = "add-method";
        pub const DOC_ADD_METHOD: &str= "Add a method to the domain. A method is defined by a label, a task, typed parameters, optional pre-conditions, and an optional body";

        /*pub const ADD_METHOD_MODEL: &str = "add-method-model";
        pub const DOC_ADD_METHOD_MODEL: &str = "Add a model to a method";*/

        pub const ADD_LAMBDA: &str = "add-lambda";
        pub const DOC_ADD_LAMBDA: &str = "Add a lambda to the execution environment.";

        pub const ADD_ENV: &str = "add-env";
        pub const DOC_ADD_ENV: &str = "Add a LValue in the execution environment.";

        pub const ADD_FACTS: &str = "add-facts";
        pub const DOC_ADD_FACTS: &str = "Add a list of facts to the inner state of the system.";

        pub const ADD_STATIC_FACTS: &str = "add-static-facts";
        pub const DOC_ADD_STATIC_FACTS: &str =
            "Add a list of facts to the static inner state of the system.";

        pub const ADD_TYPE: &str = "add-type";
        pub const DOC_ADD_TYPE: &str = "Add a new type to the domain.";

        pub const ADD_TYPES: &str = "add-types";
        pub const DOC_ADD_TYPES: &str = "Add a list of new type to the domain.";

        pub const ADD_OBJECT: &str = "add-object";
        pub const DOC_ADD_OBJECT: &str = "Add a new object in the list of instance.";

        pub const ADD_OBJECTS: &str = "add-objects";
        pub const DOC_ADD_OBJECTS: &str = "Add a list of objets in the list of instance.";

        pub const ADD_RESOURCE: &str = "add-resource";
        pub const DOC_ADD_RESOURCE: &str = "Declare a new resource";

        pub const ADD_RESOURCES: &str = "add-resources";
        pub const DOC_ADD_RESOURCES: &str = "Declare a list of new resources";

        pub const ADD_INIT: &str = "add-init";
        pub const DOC_ADD_INIT: &str =
            "Defines a program that should be executed at the launch of ompas.";

        pub const REMOVE_COMMAND: &str = "remove-command";
        pub const DOC_REMOVE_COMMAND: &str = "Removes command from the domain definition.";

        pub const REMOVE_STATE_FUNCTION: &str = "remove-state-function";
        pub const DOC_REMOVE_STATE_FUNCTION: &str =
            "Removes state-function from the domain definition.";

        pub const REMOVE_METHOD: &str = "remove-method";
        pub const DOC_REMOVE_METHOD: &str = "Removes method from the domain definition";

        pub const REMOVE_TASK: &str = "remove-task";
        pub const DOC_REMOVE_TASK: &str = "Removes task and its methods from the domain definition";

        pub const REMOVE_OBJECT: &str = "remove-object";
        pub const DOC_REMOVE_OBJECT: &str = "Removes an object from the instance collection.";

        //Macros

        pub const DEF_STATE_FUNCTION: &str = "def-state-function";
        pub const DOC_DEF_STATE_FUNCTION: &str =
            "Wrapper around add-state-function to ease the definition of new state-function.";
        pub const DOC_DEF_STATE_FUNCTION_VERBOSE: &str =
            "Example: (def-state-function at (:params (?r robot)) (:result location))";
        pub const MACRO_DEF_STATE_FUNCTION: &str = "(lambda attributes
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
                (quote ,(cons (cons ':name label) (__l__ attributes))))))))";

        pub const DEF_FUNCTION: &str = "def-function";
        pub const DOC_DEF_FUNCTION: &str =
            "Wrapper around add-function to ease the definition of new state-function.";
        pub const DOC_DEF_FUNCTION_VERBOSE: &str =
            "Example: (def-state-function at (:params (?r robot)) (:result location))";
        pub const MACRO_DEF_FUNCTION: &str = "(lambda attributes
        (let ((label (car attributes))
                (attributes (cdr attributes)))

        (begin
            (define __l__ (lambda (l)
                (if (null? l)
                nil
                 (cons 
                        (cons (caar l) (list (cdar l)))
                        (__l__ (cdr l))))))
            `(add-function (map 
                (quote ,(cons (cons ':name label) (__l__ attributes))))))))";

        pub const DEF_COMMAND: &str = "def-command";
        pub const DOC_DEF_COMMAND: &str =
            "Wrapper around add-command to ease the definition of new command.";
        pub const DOC_DEF_COMMAND_VERBOSE: &str =
            "Example: (def-command pick (:params (?r robot) (?p package))";
        pub const MACRO_DEF_COMMAND: &str = "(lambda attributes
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
                (quote ,(cons (cons ':name label) (__l__ attributes))))))))";

        pub const DEF_TASK: &str = "def-task";
        pub const DOC_DEF_TASK: &str =
            "Wrapper around add-task to ease the definition of new task.";
        pub const DOC_DEF_TASK_VERBOSE: &str =
            "Example: (def-task move (:params (?r robot) (?to location))";
        pub const MACRO_DEF_TASK: &str = "(lambda attributes
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
                (quote ,(cons (cons ':name label) (__l__ attributes))))))))";

        pub const DEF_METHOD: &str = "def-method";
        pub const DOC_DEF_METHOD: &str =
            "Wrapper around add-method to ease the definition of new method.";
        pub const DOC_DEF_METHOD_VERBOSE: &str = "Example: (def-method m_move\n\
             \t(:task move)\n\
             \t(:params (?r robot) (?to location))\n\
             \t(:pre-conditions (!= (at ?r) ?to))\n\
             \t(:body (...)))";
        pub const MACRO_DEF_METHOD: &str = "(lambda attributes
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
                (quote ,(cons (cons ':name label) (__l__ attributes))))))))";

        pub const DEF_LAMBDA: &str = "def-lambda";
        pub const DOC_DEF_LAMBDA: &str =
            "Wrapper around add-lambda to ease the definition of new lambda.";
        pub const DOC_DEF_LAMBDA_VERBOSE: &str = "Example: (def-lambda f (lambda (x) (+ x 1)))";
        pub const MACRO_DEF_LAMBDA: &str = "(lambda (label lambda)
            `(add-lambda ',label ',lambda))";

        pub const DEF_ENV: &str = "def-env";
        pub const DOC_DEF_ENV: &str =
            "Wrapper around add-env to ease definition of a LValue in the execution environment.";
        pub const DOC_DEF_ENV_VERBOSE: &str = "Example: (def-env x 2)";
        pub const MACRO_DEF_ENV: &str = "(lambda (label value)
            `(add-lambda ',label ',value))";

        pub const DEF_INIT: &str = "def-init";
        pub const DOC_DEF_INIT: &str = "Wrapper around add-init";
        pub const DOC_DEF_INIT_VERBOSE: &str = "Example: (def-init (sleep 1))";
        pub const MACRO_DEF_INIT: &str = "(lambda (body)
            `(add-init ',body ))";

        pub const OM_MODEL: &str = "om-model";
        pub const DOC_OM_MODEL: &str =
            "Ease the definition of command, task and method models in operational model fashion.";
        pub const MACRO_OM_MODEL: &str = "(lambda args
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
                        (quote ,(append (cons (list ':name label) (cons '(:model-type om) nil )) (__l__ args)))))))";

        pub const PDDL_MODEL: &str = "pddl-model";
        pub const DOC_PDDL_MODEL: &str =
            "Ease the definition of command, task and method models in a pddl fashion.";
        pub const DOC_PDDL_MODEL_VERBOSE: &str = "Example: (pddl-model label (:params ))";
        pub const MACRO_PDDL_MODEL: &str = "(lambda args
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
                        (quote ,(append (cons (list ':name label) (cons '(:model-type pddl) nil )) (__l__ args)))))))";

        pub const DEF_COMMAND_OM_MODEL: &str = "def-command-om-model";
        pub const DOC_DEF_COMMAND_OM_MODEL: &str = "Define an operational model for the command.";
        pub const MACRO_DEF_COMMAND_OM_MODEL: &str = "(lambda args
            `(add-command-model ,(cons om-model args)))";

        pub const DEF_COMMAND_PDDL_MODEL: &str = "def-command-pddl-model";
        pub const DOC_DEF_COMMAND_PDDL_MODEL: &str = "Define an pddl model for the command.";
        pub const MACRO_DEF_COMMAND_PDDL_MODEL: &str = "(lambda args
            `(add-command-model ,(cons pddl-model args)))";

        pub const DEF_TASK_OM_MODEL: &str = "def-task-om-model";
        pub const DOC_DEF_TASK_OM_MODEL: &str = "Define an operational model for the task.";
        pub const MACRO_DEF_TASK_OM_MODEL: &str = "(lambda args
            `(add-task-model ,(cons om-model args)))";

        pub const DEF_TASK_PDDL_MODEL: &str = "def-task-pddl-model";
        pub const DOC_DEF_TASK_PDDL_MODEL: &str = "Define a pddl model for the task.";
        pub const MACRO_DEF_TASK_PDDL_MODEL: &str = "(lambda args
            `(add-task-model ,(cons pddl-model args)))";

        pub const DEF_FACTS: &str = "def-facts";
        pub const DOC_DEF_FACTS: &str =
            "Wrapper to ease definition of initial facts for the system.";

        pub const DOC_DEF_FACTS_VERBOSE: &str = "Example: (def-facts\n
    ((at t1) s)\n
    ((at t2) s)\n
    ((connected s l1) yes)\n
    ((connected s l2) yes)\n
    ((connected l1 e) yes)\n
    ((connected l2 e) yes))";
        pub const MACRO_DEF_FACTS: &str = "(lambda args
    `(add-facts (map ',args)))";

        pub const DEF_STATIC_FACTS: &str = "def-static-facts";
        pub const DOC_DEF_STATIC_FACTS: &str =
            "Wrapper to ease definition of initial facts for the system.";
        pub const MACRO_DEF_STATIC_FACTS: &str = "(lambda args
    `(add-static-facts (map ',args)))";

        pub const DEF_TYPES: &str = "def-types";
        pub const DOC_DEF_TYPES: &str= "Wrapper to ease the definition of new types. Types can be defined as subtypes of other types.";
        pub const DOC_DEF_TYPES_VERBOSE: &str = "Example: (def-types (ball door gripper obj))";
        pub const MACRO_DEF_TYPES: &str = "(lambda args
    (cons 'add-types (quote-list args)))";

        pub const DEF_OBJECTS: &str = "def-objects";
        pub const DOC_DEF_OBJECTS: &str =
            "Wrapper to ease the definition of new objects. Objects are defined with their types.";
        pub const DOC_DEF_OBJECTS_VERBOSE: &str =
            "Example: (def-objects (b1 b2 b3 ball) (d1 d2 door) (o1 o2 obj))";
        pub const MACRO_DEF_OBJECTS: &str = "(lambda args
    (cons 'add-objects (quote-list args)))";

        pub const DEF_RESOURCES: &str = "def-resources";
        pub const DOC_DEF_RESOURCES: &str =
            "Wrapper to ease the definition of new objects. Objects are defined with their types.";
        pub const DOC_DEF_RESOURCES_VERBOSE: &str = "Example: (def-resources (b1 4) (b2 5) b3)";
        pub const MACRO_DEF_RESOURCES: &str = "(lambda args
    (cons 'add-resources (quote-list args)))";

        //Fields in the description
        pub const NAME: &str = ":name";
        pub const TASK: &str = ":task";
        pub const PARAMETERS: &str = ":params";
        pub const PRE_CONDITIONS: &str = ":pre-conditions";
        pub const BODY: &str = ":body";
        pub const MODEL: &str = ":model";
        pub const PLANT_MODEL: &str = ":plant-model";
        pub const SIM_MODEL: &str = ":sim-model";
        pub const PLAN_MODEL: &str = ":plan-model";
        pub const MODEL_TYPE: &str = ":model-type";
        pub const EFFECTS: &str = ":effects";
        pub const RESULT: &str = ":result";
        pub const SCORE: &str = ":score";
        pub const COST: &str = ":cost";
    }

    pub mod log {
        pub const MOD_LOG: &str = "log";
        pub const DOC_MOD_LOG: &str = "Collection of functions control the logger.";

        pub const LOG_ROOT: &str = "log-root";
        pub const LOG_PLATFORM: &str = "log-platform";
        pub const LOG_OMPAS: &str = "log-ompas";

        pub const ACTIVATE_LOG: &str = "activate-log";
        pub const DOC_ACTIVATE_LOG: &str =
            "Create a new window where the log of the given topic will be printed.\
         Logs = {log-root, log-platform, log-ompas}";

        pub const DEACTIVATE_LOG: &str = "deactivate-log";
        pub const DOC_DEACTIVATE_LOG: &str= "Kill the windows where the log was printed. Logs = {log-root, log-platform, log-ompas}";

        pub const SET_LOG_LEVEL: &str = "set-log-level";
        pub const DOC_SET_LOG_LEVEL: &str = "Set the log level used by the system to filter logs.\
        LogLevel = {error, warn, info, debug, trace}.";

        pub const GET_LOG_LEVEL: &str = "get-log-level";
        pub const DOC_GET_LOG_LEVEL: &str =
            "Return the actual log level used by the system to filter logs.\
        LogLevel = {error, warn, info, debug, trace}.";
    }

    pub mod control {

        pub const MOD_CONTROL: &str = "control";
        pub const DOC_MOD_CONTROL: &str =
            "Collection of functions to control the execution of the system.";

        pub const START: &str = "start";
        pub const DOC_START: &str = "Start the acting system.";

        pub const START_WITH_PLANNER: &str = "start-with-planner";
        pub const DOC_START_WITH_PLANNER: &str = "Start the acting system.";

        pub const STOP: &str = "stop";
        pub const DOC_STOP: &str = "Stop the acting system.";

        pub const __DEBUG_OMPAS__: &str = "__debug_ompas__";
        pub const DOC___DEBUG_OMPAS___: &str= "Send an expression that will be executed in the execution environment and return its result.";

        pub const EXEC_TASK: &str = "exec-task";
        pub const DOC_EXEC_TASK: &str = "Sends to the system a new task to address.";
        pub const DOC_EXEC_TASK_VERBOSE: &str = "Example: (exec-task t_dumber robot0)";

        pub const EXEC_COMMAND: &str = "exec-command";
        pub const DOC_EXEC_COMMAND: &str = "Sends to RAE a new command to execute";
        pub const DOC_EXEC_COMMAND_VERBOSE: &str = "Example: (exec-command do_move robot1 1 1)";

        pub const _WAIT_TASK: &str = "_wait_task";
        pub const DOC__WAIT_TASK: &str = "Wait on the result of a triggered task.";

        pub const WAIT_TASK: &str = "wait-task";
        pub const LAMBDA_WAIT_TASK: &str = "(lambda (_id_) (await (_wait_task _id_)))";
        pub const DOC_WAIT_TASK: &str = "wrapper of _wait_task";

        pub const GET_TASK_ID: &str = "get-task-id";
        pub const DOC_GET_TASK_ID: &str = "Get the internal id of the triggered task.";

        pub const CANCEL_TASK: &str = "cancel-task";
        pub const DOC_CANCEL_TASK: &str = "Cancel a triggered task.";

        pub const SET_CONFIG_PLATFORM: &str = "set-config-platform";
        pub const DOC_SET_CONFIG_PLATFORM: &str = "Configure the platform options.";

        pub const GET_CONFIG_PLATFORM: &str = "get-config-platform";
        pub const DOC_GET_CONFIG_PLATFORM: &str =
            "Return the actual value of the config of the platform";

        pub const SET_SELECT: &str = "set-select";
        pub const DOC_SET_SELECT: &str =
            "Set the select engine: greedy, aries, upom, c_choice, etc.";

        pub const SET_PLANNER_REACTIVITY: &str = "set-planner-reactivity";
        pub const DOC_SET_PLANNER_REACTIVITY: &str= "Set the time that will define the reactivity of the planner. The value can be the symbol \"inf\", or a number in secs";

        pub const GET_PLANNER_REACTIVITY: &str = "get-planner-reactivity";
        pub const DOC_GET_PLANNER_REACTIVITY: &str = "Returns the reactivity of the planner.";

        pub const GET_SELECT: &str = "get-select";
        pub const DOC_GET_SELECT: &str = "Return the select algorithm used.";

        pub const GET_STATE: &str = "get-state";
        pub const DOC_GET_STATE: &str = "Returns the current state";

        pub const GET_TYPE_HIERARCHY: &str = "get-type-hierarchy";
        pub const DOC_GET_TYPE_HIERARCHY: &str = "Return the type hierarchy defined in the domain.";

        pub const GET_AGENDA: &str = "get-agenda";
        pub const DOC_GET_AGENDA: &str = "Return the actual agenda with the status of all tasks.";
        pub const DOC_GET_AGENDA_VERBOSE: &str= "The agenda can be filtered in function of the kind\
        of the action {task, command} and their status {pending, accepted, rejected, success, failure, cancelled}";

        pub const GET_RESOURCES: &str = "get-resource-state";
        pub const DOC_GET_RESOURCES: &str =
            "Return the resources defined in the system and their waiting list.";

        pub const GET_MONITORS: &str = "get-monitored-fluents";
        pub const DOC_GET_MONITORS: &str =
            "Return the list of all dynamic expressions currently checked.";

        pub const GET_COMMANDS: &str = "get-command-list";
        pub const DOC_GET_COMMANDS: &str = "Return the list of all commands defined in the domain.";

        pub const GET_TASK_LIST: &str = "get-task-list";
        pub const DOC_GET_TASK_LIST: &str =
            "Return the list of all commands defined in the domain.";

        pub const GET_METHOD_LIST: &str = "get-method-list";
        pub const DOC_GET_METHOD_LIST: &str =
            "Return the list of all methods defined in the domain.";

        pub const GET_STATE_FUNCTIONS: &str = "get-state-function-list";
        pub const DOC_GET_STATE_FUNCTIONS: &str =
            "Return the list of all state-functions defined in the domain.";

        pub const GET_DOMAIN: &str = "get-domain";
        pub const DOC_RAE_GET_ENV: &str = "Return the domain.";

        pub const GET_STATS: &str = "get-stats";
        pub const DOC_GET_STATS: &str = "Return the statistics of the execution of the system";

        pub const EXPORT_STATS: &str = "export-stats";
        pub const DOC_EXPORT_STATS: &str =
            "Export the stats of the current run in either yaml or json format";

        pub const EXPORT_TO_CSV: &str = "export-to-csv";
        pub const DOC_EXPORT_TO_CSV: &str = "Export the statistics in csv format in a given file.";

        pub const DUMP_ACTING_TREE: &str = "dump-acting-tree";
        pub const DOC_DUMP_ACTING_TREE: &str =
            "Dump in a markdown the graph representing the execution trace.\n[Unstable] The markdown is shown in google-chrome if the right extension is installed.";

        pub const DEBUG_OMPAS: &str = "debug-ompas";
        pub const DOC_DEBUG_OMPAS: &str = "Wrapper around __debug_ompas__";
        pub const MACRO_DEBUG_OMPAS: &str = "(lambda (arg)
    `(__debug_ompas__ ',arg)))";

        pub const START_ACTING_TREE_DISPLAY: &str = "start-acting-tree-display";
        pub const DOC_START_ACTING_TREE_DISPLAY: &str =
            "Display the acting tree as a svg file in the default svg viewer of the system.";

        pub const STOP_ACTING_TREE_DISPLAY: &str = "stop-acting-tree-display";
        pub const DOC_STOP_ACTING_TREE_DISPLAY: &str = "Stop the display of the acting tree";

        pub const EXPORT_REPORT: &str = "export-report";
        pub const DOC_EXPORT_REPORT: &str =
            "Exports the acting tree and the state of the current run.";

        pub const WAIT_END_ALL: &str = "wait-end-all";
        pub const DOC_WAIT_END_ALL: &str = "Wait that all current high-level tasks are terminated.";
    }
}

pub mod supervisor {
    pub const STATUS_PENDING: &str = "pending";
    pub const STATUS_ACCEPTED: &str = "accepted";
    pub const STATUS_REJECTED: &str = "rejected";
    pub const STATUS_RUNNING: &str = "running";
    pub const STATUS_SUCCESS: &str = "success";
    pub const STATUS_FAILURE: &str = "failure";
    pub const STATUS_CANCELLED: &str = "cancelled";

    pub const TASK: &str = "task";
    pub const ROOT_TASK: &str = "root_task";
    pub const METHOD: &str = "method";
    pub const ARBITRARY: &str = "arbitrary";
    pub const ACQUIRE: &str = "acquire";
    pub const COMMAND: &str = "command";

    pub const ABSTRACT_MODEL: &str = "abstract_model";
}

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
    pub const RANDOM: &str = "random";
    pub const SCORE: &str = "score";
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

    pub const PROCESS_STOP_OMPAS: &str = "__PROCESS_STOP_OMPAS__";
    pub const PROCESS_CHECK_WAIT_FOR: &str = "__PROCESS_CHECK_WAIT_FOR__";
}

pub mod interface {

    pub const PROCESS_GET_UPDATES: &str = "__PROCESS_GET_UPDATES__";
    pub const PROCESS_SEND_COMMANDS: &str = "__PROCESS_SEND_COMMANDS__";
    pub const PROCESS_START_PLATFORM: &str = "__PROCESS_START_PLATFORM__";

    pub const DEFAULT_PLATFORM_SERVICE_IP: &str = "127.0.0.1";
    pub const DEFAULT_PLATFROM_SERVICE_PORT: u16 = 8257;
    //pub const PROCESS_TOPIC_OMPAS: &str = "__PROCESS_TOPIC_PLATFORM__";
    pub const LOG_TOPIC_PLATFORM: &str = "__LOG_TOPIC_PLATFORM__";
    pub const PLATFORM_CLIENT: &str = "PLATFORM_CLIENT";
}

pub mod sym_table {

    pub const RESULT_PREFIX: &str = "_r_";
    pub const HANDLE_PREFIX: &str = "_h_";
    pub const IF_PREFIX: &str = "if";
    pub const TIMEPOINT_PREFIX: &str = "_t_";
    pub const START_PREFIX: &str = "_s_";
    pub const END_PREFIX: &str = "_e_";
    pub const START_TASK_PREFIX: &str = "_st_";
    pub const END_TASK_PREFIX: &str = "_et_";
    pub const PRESENCE_PREFIX: &str = "_p_";
    pub const COND_PREFIX: &str = "_c_";
    pub const CHRONICLE_RESULT_PREFIX: &str = "_cr_";
    pub const ARBITRARY_PREFIX: &str = "_a_";

    pub const START: &str = "start";
    pub const END: &str = "end";
    pub const PREZ: &str = "prez";
    pub const RESULT: &str = "result";
    pub const COND: &str = "cond";
    pub const IF_TASK_PROTOTYPE: &str = "t_if";

    pub const TYPE_TIMEPOINT: &str = "*Timepoint*";
    pub const TYPE_PRESENCE: &str = "*Presence*";
    pub const TYPE_TASK: &str = "*Task*";
    pub const TYPE_METHOD: &str = "*Method*";
    pub const TYPE_ABSTRACT_TASK: &str = "*AbstractTask*";
    pub const TYPE_COMMAND: &str = "*Action*";
    pub const TYPE_PREDICATE: &str = "*Predicate*";
    pub const TYPE_STATE_FUNCTION: &str = "*StateFunctionType*";
    pub const TYPE_OBJECT_TYPE: &str = "*ObjectType*";
    pub const TYPE_OBJECT: &str = "*Object*";
    pub const TYPE_RESOURCE_HANDLE: &str = "*ResourceHandle*";

    pub const EPSILON: &str = "*eps*";
}

pub mod output {
    pub const OMPAS_STATS: &str = "stats";
    pub const CSV_FORMAT: &str = "csv";
    pub const YAML_FORMAT: &str = "yml";
    pub const JSON_FORMAT: &str = "json";
}
