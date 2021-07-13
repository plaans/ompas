use crate::rae::context::*;
use crate::rae::module::domain::GENERATE_TASK_SIMPLE;
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::rae_run;
use crate::rae::state::{StateType, KEY_DYNAMIC, KEY_INNER_WORLD, KEY_STATIC};
use ompas_lisp::async_await;
use ompas_lisp::async_await::task_watcher;
use ompas_lisp::core::{eval, expand, load_module, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::*;
use ompas_modules::doc::{Documentation, LHelp};
use ompas_modules::math::CtxMath;
use ompas_utils::log;
use std::mem;
use std::sync::Arc;
use tokio::sync::Mutex;

/*
LANGUAGE
 */
pub const MOD_RAE: &str = "mod-rae";

pub const RAE_ADD_ACTION: &str = "rae-add-action";
pub const RAE_ADD_METHOD: &str = "rae-add-method";
pub const RAE_ADD_TASK: &str = "rae-add-task";
pub const RAE_ADD_STATE_FUNCTION: &str = "rae-add-state-function";
pub const RAE_GET_METHODS: &str = "rae-get-methods";
pub const RAE_GET_ACTIONS: &str = "rae-get-actions";
pub const RAE_GET_SYMBOL_TYPE: &str = "rae-get-symbol-type";
pub const RAE_GET_TASKS: &str = "rae-get-tasks";
pub const RAE_GET_STATE_FUNCTION: &str = "rae-get-state-function";
pub const RAE_GET_ENV: &str = "rae-get-env";
pub const RAE_LAUNCH: &str = "rae-launch";
pub const RAE_GET_STATE: &str = "rae-get-state";
pub const RAE_GET_STATUS: &str = "rae-get-status";

pub const DOC_RAE_LAUNCH: &str = "todo!";
pub const DOC_RAE_ADD_ACTION: &str = "todo!";
pub const DOC_RAE_ADD_METHOD: &str = "todo!";
pub const DOC_RAE_ADD_TASK: &str = "todo!";
pub const DOC_RAE_GET_METHODS: &str = "todo!";
pub const DOC_RAE_GET_ENV: &str = "todo!";

pub const DOC_DEF_TASK: &str = "todo!";
pub const DOC_DEF_METHOD: &str = "todo!";
pub const DOC_DEF_ACTION: &str = "todo!";

pub const RAE_DEF_STATE_FUNCTION: &str = "def-state-function";
pub const RAE_DEF_ACTION: &str = "def-action";
pub const RAE_DEF_TASK: &str = "def-task";
pub const RAE_DEF_METHOD: &str = "def-method";
pub const RAE_DEF_LAMBDA: &str = "def-lambda";
pub const RAE_DEF_METHOD_PARAMETERS: &str = "def-method-parameters";
pub const RAE_DEF_INITIAL_STATE: &str = "def-initial-state";

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
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: domain,
            label: MOD_RAE,
        };

        module.add_mut_fn_prelude(RAE_LAUNCH, launch_rae);

        /*module.add_mut_fn_prelude(RAE_ADD_ACTION, add_action);
        module.add_mut_fn_prelude(RAE_ADD_STATE_FUNCTION, add_state_function);
        module.add_mut_fn_prelude(RAE_ADD_TASK, add_task);
        module.add_mut_fn_prelude(RAE_ADD_METHOD, add_method);*/
        module.add_fn_prelude(RAE_GET_METHODS, get_methods);
        module.add_fn_prelude(RAE_GET_STATE_FUNCTION, get_state_function);
        module.add_fn_prelude(RAE_GET_ACTIONS, get_actions);
        module.add_fn_prelude(RAE_GET_TASKS, get_tasks);
        module.add_fn_prelude(RAE_GET_SYMBOL_TYPE, get_symbol_type);
        module.add_fn_prelude(RAE_GET_ENV, get_env);

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

        module
    }
}

//TODO: doc ctx rae
impl Documentation for CtxRae {
    fn documentation() -> Vec<LHelp> {
        vec![]
    }
}

///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_METHOD_LIST).unwrap())
}

///Get the list of actions in the environment
pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_ACTION_LIST).unwrap())
}

///Get the list of tasks in the environment
pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_TASK_LIST).unwrap())
}

///Get the list of state functions in the environment
pub fn get_state_function(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_STATE_FUNCTION_LIST).unwrap())
}

pub fn get_symbol_type(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_SYMBOL_TYPE).unwrap())
}

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
    let lvalue = cons(&["define".into(), list], &env, &())?;
    //println!("lvalue: {}", lvalue);
    let expanded = expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?;
    /*match &expanded {
        Ok(l) => println!("ok: {}",l),
        Err(e) => println!("err: {}", e),
    }*/
    eval(&expanded, &mut ctx.env.env, &mut ctx.env.ctxs)
    //println!("result: {:?}", result);
}

pub fn def_state_function(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_STATE_FUNCTION,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    let lvalue = cons(&["generate-state-function".into(), args.into()], &env, &())?;

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

pub fn def_action(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&["generate-action".into(), args.into()], &env, &())?;

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

pub fn def_method(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_METHOD,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&["generate-method".into(), args.into()], &env, &())?;

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

pub fn def_method_parameters(
    args: &[LValue],
    env: &LEnv,
    ctx: &mut CtxRae,
) -> Result<LValue, LError> {
    todo!()
}

pub fn def_task(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_TASK,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_TASK_SIMPLE.into(), args.into()], &env, &())?;

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

pub fn def_initial_state(_: &[LValue], _: &LEnv, _: &mut CtxRae) -> Result<LValue, LError> {
    todo!()
}

pub fn get_status(_: &[LValue], _env: &LEnv, ctx: &CtxRae) -> Result<LValue, LError> {
    let status = ctx.env.actions_progress.status.read().unwrap().clone();
    let mut string = "Actions Status:\n".to_string();
    for element in status {
        string.push_str(format!("{}:{}\n", element.0, element.1).as_str())
    }

    Ok(string.into())
}

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

    let state = ctx.env.state.get_state(_type);
    Ok(state.into_map())
}

/*
launch the main in a thread
 */
pub fn launch_rae(_: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    let options = SelectOption::new(0, 0);
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
