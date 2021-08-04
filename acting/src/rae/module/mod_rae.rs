//! Module containing the Scheme library to setup RAE environment

use crate::rae::context::*;
use crate::rae::module::domain::{GENERATE_TASK_SIMPLE, LABEL_GENERATE_METHOD_PARAMETERS};
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::rae_run;
use crate::rae::state::{LState, StateType, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_STATIC};
use ompas_lisp::async_await;
use ompas_lisp::core::{eval, expand, load_module, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::*;
use ompas_modules::doc::{Documentation, LHelp};
use ompas_modules::math::CtxMath;
use ompas_utils::blocking_async;
use ompas_utils::log;
use std::convert::TryInto;
use std::mem;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::task::block_in_place;

//LANGUAGE
const MOD_RAE: &str = "mod-rae";

//const RAE_ADD_ACTION: &str = "rae-add-action";
//const RAE_ADD_METHOD: &str = "rae-add-method";
//const RAE_ADD_TASK: &str = "rae-add-task";
//const RAE_ADD_STATE_FUNCTION: &str = "rae-add-state-function";
const RAE_GET_METHODS: &str = "rae-get-methods";
const RAE_GET_ACTIONS: &str = "rae-get-actions";
const RAE_GET_SYMBOL_TYPE: &str = "rae-get-symbol-type";
const RAE_GET_TASKS: &str = "rae-get-tasks";
const RAE_GET_STATE_FUNCTIONS: &str = "rae-get-state-functions";
const RAE_GET_METHODS_PARAMETERS: &str = "rae-get-methods-parameters";
const RAE_GET_ENV: &str = "rae-get-env";
const RAE_LAUNCH: &str = "rae-launch";
const RAE_GET_STATE: &str = "rae-get-state";
const RAE_GET_STATUS: &str = "rae-get-status";

const RAE_DEF_STATE_FUNCTION: &str = "def-state-function";
const RAE_DEF_ACTION: &str = "def-action";
const RAE_DEF_TASK: &str = "def-task";
const RAE_DEF_METHOD: &str = "def-method";
const RAE_DEF_LAMBDA: &str = "def-lambda";
const RAE_DEF_METHOD_PARAMETERS: &str = "def-method-parameters";
const RAE_DEF_INITIAL_STATE: &str = "def-initial-state";
const RAE_CONFIGURE_PLATFORM: &str = "rae-configure-platform";
const RAE_GET_CONFIG_PLATFORM: &str = "rae-get-config-platform";

//DOCUMENTATION
const DOC_RAE_GET_METHODS: &str = "Returns the list of all defined methods in RAE environment";
const DOC_RAE_GET_ACTIONS: &str = "Returns the list of all defined actions in RAE environment";
const DOC_RAE_GET_SYMBOL_TYPE: &str =
    "Returns the type of the symbol as defined in RAE environment";
const DOC_RAE_GET_SYMBOL_TYPE_VERBOSE: &str = "Types:\n\
                                           \t-state-function\n\
                                           \t-action\n\
                                           \t-task\n\
                                           \t-method";
const DOC_RAE_GET_TASKS: &str = "Returns the list of all defined tasks in RAE environment";
const DOC_RAE_GET_STATE_FUNCTIONS: &str =
    "Returns the list of all defined state-functions in RAE environment";
const DOC_RAE_GET_ENV: &str = "Returns the whole environment.";
const DOC_RAE_LAUNCH: &str = "Launch the main rae loop in an asynchronous task.";
const DOC_RAE_GET_STATE: &str = "Returns the current state";
const DOC_RAE_GET_STATUS: &str = "Returns the current status of actions";

const DOC_DEF_STATE_FUNCTION: &str = "Insert a state function in RAE environment.";
const DOC_DEF_STATE_FUNCTION_VERBOSE: &str = "Example:\n(def-state-function robot.coordinates ?r)";
const DOC_DEF_ACTION: &str = "Insert an action in RAE environment.";
const DOC_DEF_ACTION_VERBOSE: &str = "Example:\n(def-action pick ?r)";
const DOC_DEF_TASK: &str = "Insert a task in RAE environment";
const DOC_DEF_TASK_VERBOSE: &str = "Example:\n(def-task t_navigate_to ?r ?x ?y)";
const DOC_DEF_METHOD: &str = "Insert a method in RAE environment.";
const DOC_DEF_METHOD_VERBOSE: &str =
    "Example:\n(def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin\n\
        \t(rae-await (navigate_to ?r ?x ?y))\n\
        \t(rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))";
const DOC_DEF_LAMBDA: &str = "Add a lambda to RAE environment";
const DOC_DEF_INITIAL_STATE: &str = "Add initial facts in the state. Most of the time it is general knowledge and not initialisation of facts.";
const DOC_RAE_CONFIGURE_PLATFROM: &str = "Set the options of the platform when it will be runned";
const DOC_RAE_GET_CONFIG_PLATFORM: &str = "Get the actual value of the config of the platform";

#[derive(Default)]
pub struct CtxRae {
    pub log: String,
    pub options: RAEOptions,
    pub env: RAEEnv,
    pub domain: InitLisp,
}

impl GetModule for CtxRae {
    fn get_module(self) -> Module {
        let domain = self.domain.clone();
        //let domain = Default::default();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: domain,
            label: MOD_RAE,
        };

        module.add_mut_fn_prelude(RAE_LAUNCH, launch_rae);

        module.add_fn_prelude(RAE_GET_METHODS, get_methods);
        module.add_fn_prelude(RAE_GET_STATE_FUNCTIONS, get_state_function);
        module.add_fn_prelude(RAE_GET_ACTIONS, get_actions);
        module.add_fn_prelude(RAE_GET_TASKS, get_tasks);
        module.add_fn_prelude(RAE_GET_METHODS_PARAMETERS, get_methods_parameters);
        module.add_fn_prelude(RAE_GET_SYMBOL_TYPE, get_symbol_type);
        module.add_fn_prelude(RAE_GET_ENV, get_env);
        module.add_mut_fn_prelude(RAE_CONFIGURE_PLATFORM, configure_platform);
        module.add_fn_prelude(RAE_GET_CONFIG_PLATFORM, get_config_platform);

        module.add_mut_fn_prelude(RAE_DEF_STATE_FUNCTION, def_state_function);
        module.add_mut_fn_prelude(RAE_DEF_ACTION, def_action);
        module.add_mut_fn_prelude(RAE_DEF_TASK, def_task);
        module.add_mut_fn_prelude(RAE_DEF_METHOD, def_method);
        module.add_mut_fn_prelude(RAE_DEF_LAMBDA, def_lambda);
        module.add_mut_fn_prelude(RAE_DEF_METHOD_PARAMETERS, def_method_parameters);
        module.add_mut_fn_prelude(RAE_DEF_INITIAL_STATE, def_initial_state);

        //functions to debug the functionnement of rae
        module.add_fn_prelude(RAE_GET_STATE, get_state);
        module.add_fn_prelude(RAE_GET_STATUS, get_status);

        /*module.add_mut_fn_prelude(RAE_ADD_ACTION, add_action);
        module.add_mut_fn_prelude(RAE_ADD_STATE_FUNCTION, add_state_function);
        module.add_mut_fn_prelude(RAE_ADD_TASK, add_task);
        module.add_mut_fn_prelude(RAE_ADD_METHOD, add_method);*/

        module
    }
}

impl Documentation for CtxRae {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(RAE_GET_METHODS, DOC_RAE_GET_METHODS),
            LHelp::new(RAE_GET_ACTIONS, DOC_RAE_GET_ACTIONS),
            LHelp::new_verbose(
                RAE_GET_SYMBOL_TYPE,
                DOC_RAE_GET_SYMBOL_TYPE,
                DOC_RAE_GET_SYMBOL_TYPE_VERBOSE,
            ),
            LHelp::new(RAE_GET_TASKS, DOC_RAE_GET_TASKS),
            LHelp::new(RAE_GET_STATE_FUNCTIONS, DOC_RAE_GET_STATE_FUNCTIONS),
            LHelp::new(RAE_GET_ENV, DOC_RAE_GET_ENV),
            LHelp::new(RAE_LAUNCH, DOC_RAE_LAUNCH),
            LHelp::new(RAE_GET_STATE, DOC_RAE_GET_STATE),
            LHelp::new(RAE_GET_STATUS, DOC_RAE_GET_STATUS),
            LHelp::new_verbose(
                RAE_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION_VERBOSE,
            ),
            LHelp::new_verbose(RAE_DEF_ACTION, DOC_DEF_ACTION, DOC_DEF_ACTION_VERBOSE),
            LHelp::new_verbose(RAE_DEF_TASK, DOC_DEF_TASK, DOC_DEF_TASK_VERBOSE),
            LHelp::new_verbose(RAE_DEF_METHOD, DOC_DEF_METHOD, DOC_DEF_METHOD_VERBOSE),
            LHelp::new(RAE_DEF_LAMBDA, DOC_DEF_LAMBDA),
            LHelp::new(RAE_DEF_INITIAL_STATE, DOC_DEF_INITIAL_STATE),
            LHelp::new(RAE_CONFIGURE_PLATFORM, DOC_RAE_CONFIGURE_PLATFROM),
            LHelp::new(RAE_GET_CONFIG_PLATFORM, DOC_RAE_GET_CONFIG_PLATFORM),
        ]
    }
}

///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_symbol(RAE_METHOD_LIST).unwrap())
}

///Get the list of actions in the environment
pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_symbol(RAE_ACTION_LIST).unwrap())
}

///Get the list of tasks in the environment
pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_symbol(RAE_TASK_LIST).unwrap())
}

///Get the list of state functions in the environment
pub fn get_state_function(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx
        .env
        .domain_env
        .get_symbol(RAE_STATE_FUNCTION_LIST)
        .unwrap())
}

/// Returns a map method: parameters
pub fn get_methods_parameters(_: &[LValue], _: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx
        .env
        .domain_env
        .get_symbol(RAE_METHOD_PARAMETERS_MAP)
        .unwrap())
}

/// Returns the map of symbol: type or the type of the symbol in args.
pub fn get_symbol_type(args: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    match args.len() {
        0 => Ok(ctx.env.domain_env.get_symbol(RAE_SYMBOL_TYPE).unwrap()),
        1 => {
            if let LValue::Symbol(_) = &args[0] {
                let map: im::HashMap<LValue, LValue> = ctx
                    .env
                    .domain_env
                    .get_symbol(RAE_SYMBOL_TYPE)
                    .unwrap()
                    .try_into()?;
                Ok(map.get(&args[0]).unwrap_or(&LValue::Nil).clone())
            } else {
                Err(WrongType(
                    RAE_GET_SYMBOL_TYPE,
                    args[0].clone(),
                    args[0].clone().into(),
                    NameTypeLValue::Symbol,
                ))
            }
        }
        _ => Err(WrongNumberOfArgument(
            RAE_GET_SYMBOL_TYPE,
            args.into(),
            args.len(),
            0..1,
        )),
    }
}

/// Returns the whole RAE environment if no arg et the entry corresponding to the symbol passed in args.
pub fn get_env(args: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(WrongType(
                    RAE_GET_ENV,
                    args[0].clone(),
                    args[0].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                RAE_GET_ENV,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };

    Ok(LValue::String(ctx.env.pretty_debug(key)))
}

/// Defines a lambda in RAE environment.
pub fn def_lambda(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    //println!("def_lambda");
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    tokio::runtime::Handle::current();
    let list: LValue = args[0].clone();
    //println!("list: {}", list);
    let lvalue = cons(&["define".into(), list], env, &())?;
    //println!("lvalue: {}", lvalue);
    let expanded = expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?;
    /*match &expanded {
        Ok(l) => println!("ok: {}",l),
        Err(e) => println!("err: {}", e),
    }*/
    eval(&expanded, &mut ctx.env.env, &mut ctx.env.ctxs)
    //println!("result: {:?}", result);
}

/// Defines a state function in RAE environment.
pub fn def_state_function(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_STATE_FUNCTION,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    let lvalue = cons(&["generate-state-function".into(), args.into()], env, &())?;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?,
        &mut ctx.env.env,
        &mut ctx.env.ctxs,
    )?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_STATE_FUNCTION,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_state_function(action_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    RAE_DEF_STATE_FUNCTION,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_STATE_FUNCTION,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines an action in RAE environment.
pub fn def_action(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&["generate-action".into(), args.into()], env, &())?;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?,
        &mut ctx.env.env,
        &mut ctx.env.ctxs,
    )?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_ACTION,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_action(action_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    RAE_DEF_ACTION,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_ACTION,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines a method in RAE environment.
pub fn def_method(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_METHOD,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&["generate-method".into(), args.into()], env, &())?;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?,
        &mut ctx.env.env,
        &mut ctx.env.ctxs,
    )?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 3 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_METHOD,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(method_label) = &list[0] {
            if let LValue::Symbol(task_label) = &list[1] {
                if let LValue::Lambda(_) = &list[2] {
                    ctx.env.add_method(
                        method_label.to_string(),
                        task_label.to_string(),
                        list[2].clone(),
                    )?;
                } else {
                    return Err(WrongType(
                        RAE_DEF_METHOD,
                        list[2].clone(),
                        list[2].clone().into(),
                        NameTypeLValue::Lambda,
                    ));
                }
            } else {
                return Err(WrongType(
                    RAE_DEF_METHOD,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_METHOD,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines a method parameter in RAE environment.
pub fn def_method_parameters(
    args: &[LValue],
    env: &LEnv,
    ctx: &mut CtxRae,
) -> Result<LValue, LError> {
    //println!("in {}", RAE_DEF_METHOD_PARAMETERS);
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_METHOD_PARAMETERS,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    let lv = cons(args, env, &())?;
    let lv = cons(&[LABEL_GENERATE_METHOD_PARAMETERS.into(), lv], env, &())?;
    //println!("In {}: lv before eval: {}", RAE_DEF_METHOD_PARAMETERS, lv);
    let lv = expand(&lv, true, &mut ctx.env.env, &mut ctx.env.ctxs)?;
    //println!("In {}: lv after expand: {}", RAE_DEF_METHOD_PARAMETERS, lv);
    let lv = eval(&lv, &mut ctx.env.env, &mut ctx.env.ctxs)?;
    /*println!(
        "In {}: result of the macro: {}",
        RAE_DEF_METHOD_PARAMETERS, lv
    );*/

    if let LValue::List(list) = &lv {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_METHOD_PARAMETERS,
                lv.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(_) = &list[0] {
            ctx.env
                .add_parameters_to_method(list[0].clone(), list[1].clone())?;
        } else {
            return Err(WrongType(
                RAE_DEF_METHOD_PARAMETERS,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    } else {
        panic!("{:?} should be a list", lv)
    };

    Ok(Nil)
}

pub fn def_task(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    //println!("in {}", RAE_DEF_TASK);
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_TASK,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_TASK_SIMPLE.into(), args.into()], env, &())?;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?,
        &mut ctx.env.env,
        &mut ctx.env.ctxs,
    )?;

    //log::send(format!("new_task: {}", lvalue));

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_TASK,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(task_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env.add_task(task_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    RAE_DEF_TASK,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_TASK,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    } else {
        return Err(WrongType(
            RAE_DEF_TASK,
            lvalue.clone(),
            lvalue.into(),
            NameTypeLValue::List,
        ));
    }

    Ok(Nil)
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
pub fn def_initial_state(args: &[LValue], _: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_INITIAL_STATE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        let mut state: LState = LState {
            inner: Default::default(),
            _type: Some(StateType::InnerWorld),
        };
        for fact in list {
            if let LValue::List(k_v) = fact {
                if k_v.len() == 3 && k_v[1] == LValue::Symbol(".".to_string()) {
                    state.insert((&k_v[0]).into(), (&k_v[2]).into())
                }
            } else {
                return Err(WrongType(
                    RAE_DEF_INITIAL_STATE,
                    fact.clone(),
                    fact.into(),
                    NameTypeLValue::List,
                ));
            }
        }
        ctx.env.state.update_state(state);
    } else {
        return Err(WrongType(
            RAE_DEF_INITIAL_STATE,
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::List,
        ));
    }

    Ok(Nil)
}

/// Returns all the status of the actions pretty printed
pub fn get_status(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    let status = ctx.env.actions_progress.status.read().unwrap().clone();
    let mut string = "Actions Status:\n".to_string();
    for element in status {
        string.push_str(format!("{}:{}\n", element.0, element.1).as_str())
    }

    Ok(string.into())
}

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
pub fn get_state(args: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    let _type = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(sym) = &args[0] {
                match sym.as_str() {
                    KEY_STATIC => Some(StateType::Static),
                    KEY_DYNAMIC => Some(StateType::Dynamic),
                    KEY_INNER_WORLD => Some(StateType::InnerWorld),
                    _ => {
                        return Err(SpecialError(
                            RAE_GET_STATE,
                            format!(
                                "was expecting keys {}, {}, {}",
                                KEY_STATIC, KEY_DYNAMIC, KEY_INNER_WORLD
                            ),
                        ))
                    }
                }
            } else {
                return Err(WrongType(
                    RAE_GET_STATE,
                    args[0].clone(),
                    (&args[0]).into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                RAE_GET_STATE,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };
    let c_state = ctx.env.state.clone();
    let state = blocking_async!(c_state.get_state(_type).await).unwrap();
    Ok(state.into_map())
}

/// Launch main loop of rae in an other asynchronous task.
pub fn launch_rae(_: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    let options = ctx.options.clone();
    let rae_env = RAEEnv {
        job_receiver: None,
        status_watcher: None,
        agenda: Default::default(),
        actions_progress: ctx.env.actions_progress.clone(),
        state: ctx.env.state.clone(),
        env: ctx.env.env.clone(),
        domain_env: ctx.env.domain_env.clone(),
        ctxs: Default::default(),
        init_lisp: Default::default(),
    };
    let context = mem::replace(&mut ctx.env, rae_env);
    tokio::spawn(async move {
        rae_run(context, &options, "rae-log.txt".to_string()).await;
    });
    Ok(LValue::String("rae launched succesfully".to_string()))
}

pub fn configure_platform(args: &[LValue], _: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_CONFIGURE_PLATFORM,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }
    let mut string = String::default();
    for arg in args {
        string.push_str(format!("{} ", arg).as_str())
    }
    ctx.options.set_platform_config(string);
    Ok(LValue::Nil)
}

pub fn get_config_platform(args: &[LValue], _: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    if !args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_GET_CONFIG_PLATFORM,
            args.into(),
            args.len(),
            0..0,
        ));
    }
    Ok(LValue::String(
        ctx.options
            .get_platfrom_config()
            .unwrap_or(String::from("no options")),
    ))
}
/*
///Add an action to RAE env
pub fn add_action(args: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    if let LValue::Symbol(action_label) = &args[0] {
        if let LValue::Lambda(_) = &args[1] {
            ctx.env
                .add_action(action_label.to_string(), args[1].clone())?;
        } else {
            return Err(WrongType(
                args[1].clone(),
                args[1].clone().into(),
                NameTypeLValue::Lambda,
            ));
        }
    } else {
        return Err(WrongType(
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::Symbol,
        ));
    }

    Ok(Nil)
}

///Add a method to RAE env
pub fn add_method(args: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.len() != 3 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 3..3));
    }

    if let LValue::Symbol(method_label) = &args[0] {
        if let LValue::Symbol(task_label) = &args[1] {
            if let LValue::Lambda(_) = &args[2] {
                ctx.env.add_method(
                    method_label.to_string(),
                    task_label.to_string(),
                    args[2].clone(),
                )?;
            } else {
                return Err(WrongType(
                    args[2].clone(),
                    args[2].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                args[1].clone(),
                args[1].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    } else {
        return Err(WrongType(
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::Symbol,
        ));
    }

    Ok(Nil)
}

///Add a task to RAE env
pub fn add_task(args: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    if let LValue::Symbol(task_label) = &args[0] {
        if let LValue::Lambda(_) = &args[1] {
            ctx.env.add_task(task_label.to_string(), args[1].clone())?;
        } else {
            return Err(WrongType(
                args[1].clone(),
                args[1].clone().into(),
                NameTypeLValue::Lambda,
            ));
        }
    } else {
        return Err(WrongType(
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::Symbol,
        ));
    }

    Ok(Nil)
}



pub fn add_state_function(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    if let LValue::Symbol(action_label) = &args[0] {
        if let LValue::Lambda(_) = &args[1] {
            ctx.env
                .add_state_function(action_label.to_string(), args[1].clone())?;
        } else {
            return Err(WrongType(
                args[1].clone(),
                args[1].clone().into(),
                NameTypeLValue::Lambda,
            ));
        }
    } else {
        return Err(WrongType(
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::Symbol,
        ));
    }

    Ok(Nil)
}*/
