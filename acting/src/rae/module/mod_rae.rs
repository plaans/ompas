//! Module containing the Scheme library to setup RAE environment

use crate::rae::context::rae_env::{Action, RAEEnv, StateFunction};
use crate::rae::context::rae_state::{LState, StateType, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_STATIC};
use crate::rae::module::mod_rae_description::*;
use crate::rae::module::rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::{rae_log, rae_run, RAEOptions};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::root_module::list::cons;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::lvalues::LValueS;
use ompas_lisp::core::structs::module::{GetModule, InitLisp, Module};
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_lisp::core::{eval, expand};
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::static_eval::{PureFonction, PureFonctionCollection};
use ompas_utils::dyn_async;
use std::convert::TryInto;
use std::mem;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::task::block_in_place;

//LANGUAGE
const MOD_RAE: &str = "rae";
const DOC_MOD_RAE: &str = "Module exposed to the user to configure and launch rae.";
const DOC_MOD_RAE_VERBOSE: &str = "functions:\n\
-getters : get-methods, get-actions, get-symbol-type, get-tasks, get-state-functions, get-env,\n\
    get-state, get-status, get-agenda, get-config-platform\n\
-definitions : def-state-function, def-actions, def-action-model, def-action-operational-model,\n\
    def-task, def-method, def-initial-state\n\
-configuration: configure-platform\n\
-launch: launch";

const RAE_GET_METHODS: &str = "get-methods";
const RAE_GET_ACTIONS: &str = "get-actions";
const RAE_GET_SYMBOL_TYPE: &str = "get-symbol-type";
const RAE_GET_TASKS: &str = "get-tasks";
const RAE_GET_STATE_FUNCTIONS: &str = "get-state-functions";
const RAE_GET_ENV: &str = "get-env";
const RAE_LAUNCH: &str = "launch";
const RAE_GET_STATE: &str = "get-state";
const RAE_GET_STATUS: &str = "get-status";
const RAE_GET_AGENDA: &str = "get-agenda";

const RAE_DEF_STATE_FUNCTION: &str = "def-state-function";
const RAE_DEF_ACTION: &str = "def-action";
const RAE_DEF_ACTION_MODEL: &str = "def-action-model";
const RAE_DEF_ACTION_OPERATIONAL_MODEL: &str = "def-action-operational-model";
const RAE_DEF_TASK: &str = "def-task";
const RAE_DEF_METHOD: &str = "def-method";
const RAE_DEF_LAMBDA: &str = "def-lambda";
const RAE_DEF_INITIAL_STATE: &str = "def-initial-state";
const RAE_CONFIGURE_PLATFORM: &str = "configure-platform";
const RAE_GET_CONFIG_PLATFORM: &str = "get-config-platform";

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
const DOC_RAE_CONFIGURE_PLATFORM: &str = "Set the options of the platform when it will be runned";
const DOC_RAE_GET_CONFIG_PLATFORM: &str = "Get the actual value of the config of the platform";
const DOC_RAE_GET_AGENDA: &str =
    "Get the actual agenda with for each task the current refinement stack.";

pub struct CtxRae {
    pub log: Option<PathBuf>,
    pub options: RAEOptions,
    pub env: RAEEnv,
    pub domain: InitLisp,
}

impl Default for CtxRae {
    fn default() -> Self {
        Self {
            log: None,
            options: Default::default(),
            env: RAEEnv {
                job_receiver: None,
                status_watcher: None,
                agenda: Default::default(),
                actions_progress: Default::default(),
                state: Default::default(),
                env: Default::default(),
                domain_env: Default::default(),
                ctxs: Default::default(),
            },
            domain: Default::default(),
        }
    }
}

impl GetModule for CtxRae {
    fn get_module(self) -> Module {
        let domain = self.domain.clone();
        //let domain = Default::default();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: domain,
            label: MOD_RAE.to_string(),
        };

        module.add_mut_fn_prelude(RAE_LAUNCH, rae_launch);

        module.add_fn_prelude(RAE_GET_METHODS, get_methods);
        module.add_fn_prelude(RAE_GET_STATE_FUNCTIONS, get_state_function);
        module.add_fn_prelude(RAE_GET_ACTIONS, get_actions);
        module.add_fn_prelude(RAE_GET_TASKS, get_tasks);
        //module.add_fn_prelude(RAE_GET_METHODS_PARAMETERS, get_methods_parameters);
        //module.add_fn_prelude(RAE_GET_SYMBOL_TYPE, get_symbol_type);
        module.add_fn_prelude(RAE_GET_ENV, get_env);
        module.add_mut_fn_prelude(RAE_CONFIGURE_PLATFORM, configure_platform);
        module.add_fn_prelude(RAE_GET_CONFIG_PLATFORM, get_config_platform);

        module.add_async_mut_fn_prelude(RAE_DEF_STATE_FUNCTION, def_state_function);
        module.add_async_mut_fn_prelude(RAE_DEF_ACTION, def_action);
        module.add_async_mut_fn_prelude(RAE_DEF_ACTION_MODEL, def_action_model);
        module.add_async_mut_fn_prelude(
            RAE_DEF_ACTION_OPERATIONAL_MODEL,
            def_action_operational_model,
        );
        module.add_async_mut_fn_prelude(RAE_DEF_TASK, def_task);
        module.add_async_mut_fn_prelude(RAE_DEF_METHOD, def_method);
        module.add_async_mut_fn_prelude(RAE_DEF_LAMBDA, def_lambda);
        //module.add_mut_fn_prelude(RAE_DEF_METHOD_PARAMETERS, def_method_parameters);
        module.add_async_mut_fn_prelude(RAE_DEF_INITIAL_STATE, def_initial_state);

        //functions to debug the functionnement of rae
        module.add_async_fn_prelude(RAE_GET_STATE, get_state);
        module.add_async_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_async_fn_prelude(RAE_GET_AGENDA, get_agenda);

        /*module.add_mut_fn_prelude(RAE_ADD_ACTION, add_action);
        module.add_mut_fn_prelude(RAE_ADD_STATE_FUNCTION, add_state_function);
        module.add_mut_fn_prelude(RAE_ADD_TASK, add_task);
        module.add_mut_fn_prelude(RAE_ADD_METHOD, add_method);*/

        module
    }
}

impl PureFonction for CtxRae {
    fn get_pure_fonctions_symbols(&self) -> PureFonctionCollection {
        vec![].into()
    }
}

impl Documentation for CtxRae {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(MOD_RAE, DOC_MOD_RAE, DOC_MOD_RAE_VERBOSE),
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
            LHelp::new(RAE_CONFIGURE_PLATFORM, DOC_RAE_CONFIGURE_PLATFORM),
            LHelp::new(RAE_GET_CONFIG_PLATFORM, DOC_RAE_GET_CONFIG_PLATFORM),
            LHelp::new(RAE_GET_AGENDA, DOC_RAE_GET_AGENDA),
        ]
    }
}

///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_list_methods())
}

///Get the list of actions in the environment
pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_list_actions())
}

///Get the list of tasks in the environment
pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_list_tasks())
}

///Get the list of state functions in the environment
pub fn get_state_function(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.domain_env.get_list_state_functions())
}

/// Returns a map method: parameters
/*pub fn get_methods_parameters(_: &[LValue], _: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx
        .env
        .domain_env
        .get_symbol(RAE_METHOD_PARAMETERS_GENERATOR_MAP)
        .unwrap())
}*/

/// Returns the map of symbol: type or the type of the symbol in args.
/*pub fn get_symbol_type(args: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
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
                    TypeLValue::Symbol,
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
}*/

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
                    TypeLValue::Symbol,
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

    match key {
        None => Ok(ctx.env.domain_env.to_string().into()),
        Some(key) => Ok(ctx.env.domain_env.get_element_description(key).into()),
    }
}

/// Defines a lambda in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_lambda<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(label) = &list[0] {
            let expanded = expand(&list[1], true, &mut ctx.env.env, &mut ctx.env.ctxs).await?;
            let (mut e, mut c) = LEnv::root().await;
            let result = eval(&expanded, &mut e, &mut c).await?;
            if let LValue::Lambda(_) = &result {
                ctx.env.add_lambda(label.clone(), result);
            }
        }
    }
    Ok(LValue::Nil)
}

/// Defines a state function in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_state_function<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_STATE_FUNCTION,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let lvalue = cons(&[GENERATE_STATE_FUNCTION.into(), args.into()], env, &())?;
    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 3 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_STATE_FUNCTION,
                lvalue.clone(),
                list.len(),
                3..3,
            ));
        } else if let LValue::Symbol(sf_label) = &list[0] {
            if let LValue::List(_) | LValue::Nil = &list[1] {
                if let LValue::Lambda(_) = &list[2] {
                    ctx.env.add_state_function(
                        sf_label.to_string(),
                        StateFunction::new((&list[1]).try_into()?, list[2].clone()),
                    )?;
                } else {
                    return Err(WrongType(
                        RAE_DEF_STATE_FUNCTION,
                        list[2].clone(),
                        list[2].clone().into(),
                        TypeLValue::Lambda,
                    ));
                }
            } else {
                return Err(WrongType(
                    RAE_DEF_STATE_FUNCTION,
                    list[1].clone(),
                    (&list[1]).into(),
                    TypeLValue::List,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_STATE_FUNCTION,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    }

    Ok(LValue::Nil)
}

/// Defines an action in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action_model<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION_MODEL,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION_MODEL.into(), args.into()], env, &())?;
    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_ACTION_MODEL,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_action_sample_fn(action_label.into(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    RAE_DEF_ACTION_MODEL,
                    list[1].clone(),
                    list[1].clone().into(),
                    TypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_ACTION_MODEL,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    }

    Ok(LValue::Nil)
}

/// Defines an action in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action_operational_model<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION_OPERATIONAL_MODEL,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(
        &[GENERATE_ACTION_OPERATIONAL_MODEL.into(), args.into()],
        env,
        &(),
    )?;
    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_ACTION_OPERATIONAL_MODEL,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_action_sample_fn(action_label.into(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    RAE_DEF_ACTION_OPERATIONAL_MODEL,
                    list[1].clone(),
                    list[1].clone().into(),
                    TypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_ACTION_OPERATIONAL_MODEL,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    }

    Ok(LValue::Nil)
}

/// Defines an action in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 3 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_ACTION,
                lvalue.clone(),
                list.len(),
                3..3,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::List(_) | LValue::Nil = &list[1] {
                if let LValue::Lambda(_) = &list[2] {
                    ctx.env.add_action(
                        action_label.to_string(),
                        Action::new((&list[1]).try_into()?, list[2].clone(), LValue::Nil),
                    )?;
                } else {
                    return Err(WrongType(
                        RAE_DEF_ACTION,
                        list[2].clone(),
                        list[2].clone().into(),
                        TypeLValue::Lambda,
                    ));
                }
            } else {
                return Err(WrongType(
                    RAE_DEF_ACTION,
                    list[1].clone(),
                    list[1].clone().into(),
                    TypeLValue::List,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_ACTION,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    }

    Ok(LValue::Nil)
}

/// Defines a method in RAE environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_method<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_METHOD,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_METHOD.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 6 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_METHOD,
                lvalue.clone(),
                list.len(),
                6..6,
            ));
        } else if let LValue::Symbol(method_label) = &list[0] {
            if let LValue::Symbol(task_label) = &list[1] {
                match &list[2] {
                    LValue::List(_) | LValue::Nil => {
                        if let LValue::Lambda(_) = &list[3] {
                            if let LValue::Lambda(_) = &list[4] {
                                if let LValue::Lambda(_) = &list[5] {
                                    ctx.env.add_method(
                                        method_label.to_string(),
                                        task_label.to_string(),
                                        list[2].clone().try_into()?,
                                        list[3].clone(),
                                        list[4].clone(),
                                        list[5].clone(),
                                    )?;
                                } else {
                                    return Err(WrongType(
                                        RAE_DEF_METHOD,
                                        list[5].clone(),
                                        list[5].clone().into(),
                                        TypeLValue::Lambda,
                                    ));
                                }
                            } else {
                                return Err(WrongType(
                                    RAE_DEF_METHOD,
                                    list[4].clone(),
                                    list[4].clone().into(),
                                    TypeLValue::Lambda,
                                ));
                            }
                        } else {
                            return Err(WrongType(
                                RAE_DEF_METHOD,
                                list[3].clone(),
                                list[3].clone().into(),
                                TypeLValue::Lambda,
                            ));
                        }
                    }
                    _ => {
                        return Err(WrongType(
                            RAE_DEF_METHOD,
                            list[2].clone(),
                            list[2].clone().into(),
                            TypeLValue::List,
                        ))
                    }
                }
            } else {
                return Err(WrongType(
                    RAE_DEF_METHOD,
                    list[1].clone(),
                    list[1].clone().into(),
                    TypeLValue::Symbol,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_METHOD,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    }

    Ok(LValue::Nil)
}

#[macro_rules_attribute(dyn_async!)]
async fn def_task<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_TASK,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_TASK_SIMPLE.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    //println!("new_task: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 3 {
            return Err(WrongNumberOfArgument(
                RAE_DEF_TASK,
                lvalue.clone(),
                list.len(),
                3..3,
            ));
        } else if let LValue::Symbol(task_label) = &list[0] {
            if let LValue::Lambda(_) = &list[2] {
                ctx.env.add_task(
                    task_label.to_string(),
                    list[2].clone(),
                    (&list[1]).try_into()?,
                )?;
            } else {
                return Err(WrongType(
                    RAE_DEF_TASK,
                    list[2].clone(),
                    list[2].clone().into(),
                    TypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                RAE_DEF_TASK,
                list[0].clone(),
                list[0].clone().into(),
                TypeLValue::Symbol,
            ));
        }
    } else {
        return Err(WrongType(
            RAE_DEF_TASK,
            lvalue.clone(),
            lvalue.into(),
            TypeLValue::List,
        ));
    }

    Ok(LValue::Nil)
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
#[macro_rules_attribute(dyn_async!)]
async fn def_initial_state<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a mut CtxRae,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_INITIAL_STATE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::Map(map) = &args[0] {
        let state: LState = LState {
            inner: {
                let mut map_2: im::HashMap<LValueS, LValueS> = Default::default();
                for (k, v) in map {
                    map_2.insert(k.into(), v.into());
                }
                map_2
            },
            _type: Some(StateType::InnerWorld),
        };

        ctx.env.state.update_state(state).await;
        Ok(LValue::Nil)
    } else {
        Err(WrongType(
            RAE_DEF_INITIAL_STATE,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Map,
        ))
    }
}

/// Returns all the status of the actions pretty printed
#[macro_rules_attribute(dyn_async!)]
async fn get_status<'a>(
    _: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRae,
) -> Result<LValue, LError> {
    let status = ctx.env.actions_progress.status.read().await;
    let mut string = "Actions Status:\n".to_string();
    for element in status.iter() {
        string.push_str(format!("{}:{}\n", element.0, element.1).as_str())
    }

    Ok(string.into())
}

/// Returns the whole state if no args, or specific part of it ('static', 'dynamic', 'inner world')
#[macro_rules_attribute(dyn_async!)]
async fn get_state<'a>(
    args: &'a [LValue],
    _env: &'a LEnv,
    ctx: &'a CtxRae,
) -> Result<LValue, LError> {
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
                    TypeLValue::Symbol,
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
    let state = ctx.env.state.get_state(_type).await;
    Ok(state.into_map())
}

/// Launch main loop of rae in an other asynchronous task.
pub fn rae_launch(_: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    let options = ctx.options.clone();
    let rae_env = RAEEnv {
        job_receiver: None,
        status_watcher: None,
        agenda: ctx.env.agenda.clone(),
        actions_progress: ctx.env.actions_progress.clone(),
        state: ctx.env.state.clone(),
        env: ctx.env.env.clone(),
        domain_env: ctx.env.domain_env.clone(),
        ctxs: Default::default(),
    };
    let context = mem::replace(&mut ctx.env, rae_env);
    rae_log::init(ctx.log.clone()).expect("Error while initiating logger.");

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
            .get_platform_config()
            .unwrap_or_else(|| String::from("no options")),
    ))
}
#[macro_rules_attribute(dyn_async!)]
async fn get_agenda<'a>(_: &'a [LValue], _: &'a LEnv, ctx: &'a CtxRae) -> Result<LValue, LError> {
    let string = ctx.env.agenda.display().await;
    Ok(string.into())
}
