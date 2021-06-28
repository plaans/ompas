use crate::rae::context::*;
use crate::rae::module::domain::{
    MACRO_DEF_ACTION, MACRO_DEF_METHOD, MACRO_DEF_STATE_FUNCTION, MACRO_DEF_TASK,
};
use crate::rae::module::mod_rae_exec::{CtxRaeExec, RAEInterface};
use crate::rae::rae_run;
use ompas_lisp::core::{eval, expand, load_module, LEnv};
use ompas_lisp::functions::cons;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::*;
use ompas_modules::doc::{Documentation, LHelp};
use ompas_modules::math::CtxMath;
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

        module.add_mut_fn_prelude(RAE_ADD_ACTION, add_action);
        module.add_mut_fn_prelude(RAE_ADD_STATE_FUNCTION, add_state_function);
        module.add_mut_fn_prelude(RAE_ADD_TASK, add_task);
        module.add_mut_fn_prelude(RAE_ADD_METHOD, add_method);
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
                    args[0].clone(),
                    args[0].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        }
        _ => return Err(WrongNumberOfArgument(args.into(), args.len(), 0..1)),
    };

    Ok(LValue::String(ctx.env.pretty_debug(key)))
}

pub fn def_state_function(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
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
            return Err(WrongNumberOfArgument(lvalue.clone(), list.len(), 2..2));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_state_function(action_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

pub fn add_state_function(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRae,
) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..std::usize::MAX));
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
}


pub fn def_action(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..std::usize::MAX));
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
            return Err(WrongNumberOfArgument(lvalue.clone(), list.len(), 2..2));
        } else if let LValue::Symbol(action_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env
                    .add_action(action_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
    }

    Ok(Nil)
}

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


pub fn def_method(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..std::usize::MAX));
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
            return Err(WrongNumberOfArgument(lvalue.clone(), list.len(), 2..2));
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
                        list[2].clone(),
                        list[2].clone().into(),
                        NameTypeLValue::Lambda,
                    ));
                }
            } else {
                return Err(WrongType(
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Symbol,
                ));
            }
        } else {
            return Err(WrongType(
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
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


pub fn def_task(args: &[LValue], env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..std::usize::MAX));
    }

    let lvalue = cons(&["generate-task".into(), args.into()], &env, &())?;

    let lvalue = eval(
        &expand(&lvalue, true, &mut ctx.env.env, &mut ctx.env.ctxs)?,
        &mut ctx.env.env,
        &mut ctx.env.ctxs,
    )?;

    //println!("lvalue: {}", lvalue);

    if let LValue::List(list) = &lvalue {
        if list.len() != 2 {
            return Err(WrongNumberOfArgument(lvalue.clone(), list.len(), 2..2));
        } else if let LValue::Symbol(task_label) = &list[0] {
            if let LValue::Lambda(_) = &list[1] {
                ctx.env.add_task(task_label.to_string(), list[1].clone())?;
            } else {
                return Err(WrongType(
                    list[1].clone(),
                    list[1].clone().into(),
                    NameTypeLValue::Lambda,
                ));
            }
        } else {
            return Err(WrongType(
                list[0].clone(),
                list[0].clone().into(),
                NameTypeLValue::Symbol,
            ));
        }
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

/*
launch the main in a thread
 */
pub fn launch_rae(_: &[LValue], _env: &LEnv, ctx: &mut CtxRae) -> Result<LValue, LError> {
    let options = SelectOption::new(0, 0);
    let rae_env = RAEEnv {
        receiver: None,
        agenda: Default::default(),
        actions_progress: Default::default(),
        state: Default::default(),
        env: ctx.env.env.clone(),
        ctxs: Default::default(),
        init_lisp: Default::default()
    };
    let context = mem::replace(&mut ctx.env, rae_env);
    tokio::spawn(async move {
        rae_run(context, &options, "rae-log.txt".to_string()).await;
    });
    Ok(LValue::String("rae launched succesfully".to_string()))
}
