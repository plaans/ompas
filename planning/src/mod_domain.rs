//! Module containing the Scheme library to setup DOMAIN environment

use crate::algo::translate_lvalue_to_expression_chronicle_r;
use crate::structs::{FormatWithSymTable, SymTable};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_acting::rae::context::rae_env::*;
use ompas_acting::rae::context::rae_state::{LState, RAEState, StateType};
use ompas_acting::rae::module::mod_rae_description::*;
use ompas_lisp::core::ImportType::WithoutPrefix;
use ompas_lisp::core::{eval, expand, import, ContextCollection, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::modules::doc::{Documentation, LHelp};
use ompas_lisp::modules::math::CtxMath;
use ompas_lisp::modules::utils::CtxUtils;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::*;
use ompas_utils::dyn_async;
use std::sync::Arc;

//LANGUAGE
const MOD_DOMAIN: &str = "domain";
const DOC_MOD_DOMAIN: &str =
    "Module exposed to the user to declare a domain and translate it into chronicles.";
const DOC_MOD_DOMAIN_VERBOSE: &str = "functions:\n\
-getters : get-methods, get-actions, get-symbol-type, get-tasks, get-state-functions, get-env,\n\
    get-state, get-status, get-agenda, get-config-platform\n\
-definitions : def-state-function, def-actions, def-action-model, def-action-operational-model,\n\
    def-task, def-method, def-initial-state\n\
-configuration: configure-platform\n\
-launch: launch";

const DOMAIN_GET_METHODS: &str = "get-methods";
const DOMAIN_GET_ACTIONS: &str = "get-actions";
const DOMAIN_GET_SYMBOL_TYPE: &str = "get-symbol-type";
const DOMAIN_GET_TASKS: &str = "get-tasks";
const DOMAIN_GET_STATE_FUNCTIONS: &str = "get-state-functions";
const DOMAIN_GET_ENV: &str = "get-env";

const DOMAIN_DEF_STATE_FUNCTION: &str = "def-state-function";
const DOMAIN_DEF_ACTION: &str = "def-action";
const DOMAIN_DEF_ACTION_MODEL: &str = "def-action-model";
const DOMAIN_DEF_ACTION_OPERATIONAL_MODEL: &str = "def-action-operational-model";
const DOMAIN_DEF_TASK: &str = "def-task";
const DOMAIN_DEF_METHOD: &str = "def-method";
const DOMAIN_DEF_LAMBDA: &str = "def-lambda";
const DOMAIN_DEF_INITIAL_STATE: &str = "def-initial-state";

const DOMAIN_TRANSLATE_EXPR: &str = "translate-expr";
const DOMAIN_TRANSLATE_DOMAIN: &str = "translate-domain";

//DOCUMENTATION
const DOC_DOMAIN_GET_METHODS: &str =
    "Returns the list of all defined methods in DOMAIN environment";
const DOC_DOMAIN_GET_ACTIONS: &str =
    "Returns the list of all defined actions in DOMAIN environment";
const DOC_DOMAIN_GET_SYMBOL_TYPE: &str =
    "Returns the type of the symbol as defined in DOMAIN environment";
const DOC_DOMAIN_GET_SYMBOL_TYPE_VERBOSE: &str = "Types:\n\
                                           \t-state-function\n\
                                           \t-action\n\
                                           \t-task\n\
                                           \t-method";
const DOC_DOMAIN_GET_TASKS: &str = "Returns the list of all defined tasks in DOMAIN environment";
const DOC_DOMAIN_GET_STATE_FUNCTIONS: &str =
    "Returns the list of all defined state-functions in DOMAIN environment";
const DOC_DOMAIN_GET_ENV: &str = "Returns the whole environment.";

const DOC_DEF_STATE_FUNCTION: &str = "Insert a state function in DOMAIN environment.";
const DOC_DEF_STATE_FUNCTION_VERBOSE: &str = "Example:\n(def-state-function robot.coordinates ?r)";
const DOC_DEF_ACTION: &str = "Insert an action in DOMAIN environment.";
const DOC_DEF_ACTION_VERBOSE: &str = "Example:\n(def-action pick ?r)";
const DOC_DEF_TASK: &str = "Insert a task in DOMAIN environment";
const DOC_DEF_TASK_VERBOSE: &str = "Example:\n(def-task t_navigate_to ?r ?x ?y)";
const DOC_DEF_METHOD: &str = "Insert a method in DOMAIN environment.";
const DOC_DEF_METHOD_VERBOSE: &str =
    "Example:\n(def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin\n\
        \t(DOMAIN-await (navigate_to ?r ?x ?y))\n\
        \t(DOMAIN-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))";
const DOC_DEF_LAMBDA: &str = "Add a lambda to DOMAIN environment";
const DOC_DEF_INITIAL_STATE: &str = "Add initial facts in the state. Most of the time it is general knowledge and not initialisation of facts.";

pub struct CtxDomain {
    pub domain: DomainEnv,
    pub env: LEnv,
    pub ctxs: ContextCollection,
    pub state: RAEState,
}

impl CtxDomain {
    pub async fn new() -> Self {
        let (mut env, mut ctxs) = LEnv::root().await;

        //Math
        import(&mut env, &mut ctxs, CtxUtils::default(), WithoutPrefix)
            .await
            .expect("error loading ctx utils");
        import(&mut env, &mut ctxs, CtxMath::default(), WithoutPrefix)
            .await
            .expect("error loading ctx math");
        import(
            &mut env,
            &mut ctxs,
            CtxRaeDescription::default(),
            WithoutPrefix,
        )
        .await
        .expect("error loading ctx rae description");

        Self {
            domain: Default::default(),
            env,
            ctxs,
            state: Default::default(),
        }
    }
}

impl GetModule for CtxDomain {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec!["(read godot_domain/translate.lisp)"].into(),
            label: MOD_DOMAIN.to_string(),
        };

        module.add_fn_prelude(DOMAIN_GET_METHODS, get_methods);
        module.add_fn_prelude(DOMAIN_GET_STATE_FUNCTIONS, get_state_function);
        module.add_fn_prelude(DOMAIN_GET_ACTIONS, get_actions);
        module.add_fn_prelude(DOMAIN_GET_TASKS, get_tasks);
        module.add_fn_prelude(DOMAIN_GET_ENV, get_env);

        module.add_fn_prelude(DOMAIN_TRANSLATE_EXPR, translate_expr);
        module.add_fn_prelude(DOMAIN_TRANSLATE_DOMAIN, translate_domain);

        module.add_async_mut_fn_prelude(DOMAIN_DEF_STATE_FUNCTION, def_state_function);
        module.add_async_mut_fn_prelude(DOMAIN_DEF_ACTION, def_action);
        module.add_async_mut_fn_prelude(DOMAIN_DEF_ACTION_MODEL, def_action_model);
        module.add_async_mut_fn_prelude(
            DOMAIN_DEF_ACTION_OPERATIONAL_MODEL,
            def_action_operational_model,
        );
        module.add_async_mut_fn_prelude(DOMAIN_DEF_TASK, def_task);
        module.add_async_mut_fn_prelude(DOMAIN_DEF_METHOD, def_method);
        module.add_async_mut_fn_prelude(DOMAIN_DEF_LAMBDA, def_lambda);
        module.add_async_mut_fn_prelude(DOMAIN_DEF_INITIAL_STATE, def_initial_state);

        module
    }
}

impl Documentation for CtxDomain {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(MOD_DOMAIN, DOC_MOD_DOMAIN, DOC_MOD_DOMAIN_VERBOSE),
            LHelp::new(DOMAIN_GET_METHODS, DOC_DOMAIN_GET_METHODS),
            LHelp::new(DOMAIN_GET_ACTIONS, DOC_DOMAIN_GET_ACTIONS),
            LHelp::new_verbose(
                DOMAIN_GET_SYMBOL_TYPE,
                DOC_DOMAIN_GET_SYMBOL_TYPE,
                DOC_DOMAIN_GET_SYMBOL_TYPE_VERBOSE,
            ),
            LHelp::new(DOMAIN_GET_TASKS, DOC_DOMAIN_GET_TASKS),
            LHelp::new(DOMAIN_GET_STATE_FUNCTIONS, DOC_DOMAIN_GET_STATE_FUNCTIONS),
            LHelp::new(DOMAIN_GET_ENV, DOC_DOMAIN_GET_ENV),
            LHelp::new_verbose(
                DOMAIN_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION,
                DOC_DEF_STATE_FUNCTION_VERBOSE,
            ),
            LHelp::new_verbose(DOMAIN_DEF_ACTION, DOC_DEF_ACTION, DOC_DEF_ACTION_VERBOSE),
            LHelp::new_verbose(DOMAIN_DEF_TASK, DOC_DEF_TASK, DOC_DEF_TASK_VERBOSE),
            LHelp::new_verbose(DOMAIN_DEF_METHOD, DOC_DEF_METHOD, DOC_DEF_METHOD_VERBOSE),
            LHelp::new(DOMAIN_DEF_LAMBDA, DOC_DEF_LAMBDA),
            LHelp::new(DOMAIN_DEF_INITIAL_STATE, DOC_DEF_INITIAL_STATE),
        ]
    }
}

pub fn translate_expr(args: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            DOMAIN_TRANSLATE_EXPR,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let lv = &args[0];

    let mut symbol_table = SymTable::default();
    let chronicle = translate_lvalue_to_expression_chronicle_r(lv, &mut symbol_table);
    let string = chronicle.format_with_sym_table(&symbol_table);
    Ok(string.into())
}

pub fn translate_domain(_: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    Ok("Not implemented yet".into())
}
///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    Ok(ctx.domain.get_list_methods())
}

///Get the list of actions in the environment
pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    Ok(ctx.domain.get_list_actions())
}

///Get the list of tasks in the environment
pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    Ok(ctx.domain.get_list_tasks())
}

///Get the list of state functions in the environment
pub fn get_state_function(_: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    Ok(ctx.domain.get_list_state_functions())
}

/// Returns the whole DOMAIN environment if no arg et the entry corresponding to the symbol passed in args.
pub fn get_env(args: &[LValue], _env: &LEnv, ctx: &CtxDomain) -> Result<LValue, LError> {
    let key = match args.len() {
        0 => None,
        1 => {
            if let LValue::Symbol(key) = args[0].clone() {
                Some(key)
            } else {
                return Err(WrongType(
                    DOMAIN_GET_ENV,
                    args[0].clone(),
                    args[0].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => {
            return Err(WrongNumberOfArgument(
                DOMAIN_GET_ENV,
                args.into(),
                args.len(),
                0..1,
            ))
        }
    };

    match key {
        None => Ok(ctx.domain.to_string().into()),
        Some(key) => Ok(ctx.domain.get_element_description(key).into()),
    }
}

/// Defines a lambda in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_lambda<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(label) = &list[0] {
            let expanded = expand(&list[1], true, &mut ctx.env, &mut ctx.ctxs).await?;
            let (mut e, mut c) = LEnv::root().await;
            let result = eval(&expanded, &mut e, &mut c).await?;
            if let LValue::Lambda(_) = &result {
                ctx.domain.add_lambda(label.clone(), result);
            }
        }
    }
    Ok(LValue::Nil)
}

/// Defines a state function in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_state_function<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_STATE_FUNCTION,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let lvalue = cons(&[GENERATE_STATE_FUNCTION.into(), args.into()], env, &())?;
    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 3 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_STATE_FUNCTION,
                lvalue.clone(),
                list.len(),
                3..3,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                if let LValue::Lambda(_) = &list[2] {
                    ctx.domain.add_state_function(
                        action_label.to_string(),
                        StateFunction::new(list[1].clone(), list[2].clone()),
                    );
                } else {
                    return Err(WrongType(
                        DOMAIN_DEF_STATE_FUNCTION,
                        list[2].clone(),
                        list[2].clone().into(),
                        NameTypeLValue::Lambda,
                    ));
                }
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_STATE_FUNCTION,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_STATE_FUNCTION,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines an action in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action_model<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_ACTION_MODEL,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION_MODEL.into(), args.into()], env, &())?;
    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_ACTION_MODEL,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.domain
                    .add_action_sample_fn(action_label.into(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_ACTION_MODEL,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_ACTION_MODEL,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines an action in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action_operational_model<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_ACTION_OPERATIONAL_MODEL,
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
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_ACTION_OPERATIONAL_MODEL,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.domain
                    .add_action_sample_fn(action_label.into(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_ACTION_OPERATIONAL_MODEL,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_ACTION_OPERATIONAL_MODEL,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines an action in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_action<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_ACTION,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_ACTION,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.domain
                    .add_action(action_label.to_string(), Action::new(list[1].clone(), Nil));
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_ACTION,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_ACTION,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

/// Defines a method in DOMAIN environment.
#[macro_rules_attribute(dyn_async!)]
async fn def_method<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_METHOD,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_METHOD.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 7 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_METHOD,
                lvalue.clone(),
                list.len(),
                7..7,
            ));
        } else if let LValue::Symbol(method_label) = &list[0] {
            if let LValue::Symbol(task_label) = &list[1] {
                match &list[2] {
                    LValue::List(_) | LValue::Nil => {
                        let mut types = vec![];
                        if let LValue::List(lv_types) = &list[2] {
                            for e in lv_types {
                                if let LValue::Symbol(s) = e {
                                    types.push(s.clone());
                                } else {
                                    return Err(WrongType(
                                        DOMAIN_DEF_METHOD,
                                        e.clone(),
                                        e.into(),
                                        NameTypeLValue::Symbol,
                                    ));
                                }
                            }
                        }
                        if let LValue::Lambda(_) = &list[3] {
                            if let LValue::Lambda(_) = &list[4] {
                                if let LValue::Lambda(_) = &list[5] {
                                    if let LValue::Lambda(_) = &list[6] {
                                        ctx.domain.add_method(
                                            method_label.to_string(),
                                            Method::new(
                                                task_label.to_string(),
                                                types,
                                                list[3].clone(),
                                                list[4].clone(),
                                                list[5].clone(),
                                                list[6].clone(),
                                            ),
                                        )?;
                                    } else {
                                        return Err(WrongType(
                                            DOMAIN_DEF_METHOD,
                                            list[6].clone(),
                                            list[6].clone().into(),
                                            NameTypeLValue::Lambda,
                                        ));
                                    }
                                } else {
                                    return Err(WrongType(
                                        DOMAIN_DEF_METHOD,
                                        list[5].clone(),
                                        list[5].clone().into(),
                                        NameTypeLValue::Lambda,
                                    ));
                                }
                            } else {
                                return Err(WrongType(
                                    DOMAIN_DEF_METHOD,
                                    list[4].clone(),
                                    list[4].clone().into(),
                                    NameTypeLValue::Lambda,
                                ));
                            }
                        } else {
                            return Err(WrongType(
                                DOMAIN_DEF_METHOD,
                                list[3].clone(),
                                list[3].clone().into(),
                                NameTypeLValue::Lambda,
                            ));
                        }
                    }
                    _ => {
                        return Err(WrongType(
                            DOMAIN_DEF_METHOD,
                            list[2].clone(),
                            (&list[2]).into(),
                            NameTypeLValue::List,
                        ))
                    }
                }
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_METHOD,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_METHOD,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

#[macro_rules_attribute(dyn_async!)]
async fn def_task<'a>(
    args: &'a [LValue],
    env: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_TASK,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_TASK_SIMPLE.into(), args.into()], env, &())?;

    let (mut e, mut c) = LEnv::root().await;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env, &mut ctx.ctxs).await?,
        &mut e,
        &mut c,
    )
    .await?;

    //println!("new_task: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(
                DOMAIN_DEF_TASK,
                lvalue.clone(),
                list.len(),
                2..2,
            ));
        } else if let LValue::Symbol(task_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.domain
                    .add_task(task_label.to_string(), Task::new(list[1].clone()));
            } else {
                return Err(WrongType(
                    DOMAIN_DEF_TASK,
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                DOMAIN_DEF_TASK,
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    } else {
        return Err(WrongType(
            DOMAIN_DEF_TASK,
            lvalue.clone(),
            lvalue.into(),
            NameTypeLValue::List,
        ));
    }

    Ok(Nil)
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
#[macro_rules_attribute(dyn_async!)]
async fn def_initial_state<'a>(
    args: &'a [LValue],
    _: &'a LEnv,
    ctx: &'a mut CtxDomain,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            DOMAIN_DEF_INITIAL_STATE,
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
                    DOMAIN_DEF_INITIAL_STATE,
                    fact.clone(),
                    fact.into(),
                    NameTypeLValue::List,
                ));
            }
        }
        let c_state = ctx.state.clone();
        c_state.update_state(state).await;
    } else {
        return Err(WrongType(
            DOMAIN_DEF_INITIAL_STATE,
            args[0].clone(),
            args[0].clone().into(),
            NameTypeLValue::List,
        ));
    }

    Ok(Nil)
}
