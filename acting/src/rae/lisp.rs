use crate::rae::context::{
    ActionId, ActionsProgress, Agenda, RAEEnv, RAEEnvBis, RAEOptions, SelectOption, Status,
    RAE_ACTION_LIST, RAE_METHOD_LIST, RAE_TASK_LIST,
};
use crate::rae::job::Job;
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use tokio::sync::mpsc::Receiver;

//Others functions could be add to interogate and launch rae.

/*
LANGUAGE
 */

pub const MOD_RAE: &str = "mod-rae";

pub const RAE_LAUNCH: &str = "rae-launch";
pub const RAE_ADD_ACTION: &str = "rae-add-action";
pub const RAE_ADD_METHOD: &str = "rae-add-method";
pub const RAE_ADD_TASK: &str = "rae-add-task";
pub const RAE_GET_METHODS: &str = "rae-get-methods";
pub const RAE_GET_ACTIONS: &str = "rae-get-actions";
pub const RAE_GET_TASKS: &str = "rae-get-tasks";

pub const RAE_GET_ENV: &str = "rae-get-env";
pub const RAE_SET_EXEC_COMMAND: &str = "rae-set-exec-command";
pub const RAE_GET_EXEC_COMMAND: &str = "rae-get-exec-command";
pub const RAE_TRIGGER_EVENT: &str = "rae-trigger-event";
pub const RAE_TRIGGER_TASK: &str = "rae-trigger-task";

pub const LAMBDA_DEF_TASK: &str = "(defmacro deftask \
                                        (lambda (l body) \
                                            (quasiquote (rae-add-task (unquote l) (lambda (unquote (cdar body)) \
                                                (if (unquote (cadadr body)) \
                                                    (unquote (cadaddr body)) \
                                                    (quote (task is not applicable in the given state))))))))";
pub const DEF_TASK: &str = "deftask";

pub const LAMBDA_DEF_METHOD: &str = "(defmacro defmethod \
                                          (lambda (l body) \
                                            (let ((task-label (cadar body)) \
                                                  (params (cdadr body)) \
                                                  (body (cadaddr body))) \
                                                 (quasiquote (rae-add-method (unquote l) \
                                                                    (unquote task-label) \
                                                                    (lambda (unquote params) \
                                                                            (unquote body)))))))";
pub const DEF_METHOD: &str = "defmethod";

pub const LAMBDA_DEF_ACTION: &str ="(defmacro defaction \
                                        (lambda args \
                                            (let ((label (car args)) \
                                                  (params (cdr args))) \
                                                 (quasiquote (rae-add-action (unquote label) \
                                                                    (lambda (unquote params) \
                                                                            (unquote (cons (rae-get-exec-command) \
                                                                                     (cons label params)))))))))";
pub const DEF_ACTION: &str = "defmethod";

/*
DOCUMENTATION
 */

pub const DOC_RAE_LAUNCH: &str = "todo!";
pub const DOC_RAE_ADD_ACTION: &str = "todo!";
pub const DOC_RAE_ADD_METHOD: &str = "todo!";
pub const DOC_RAE_ADD_TASK: &str = "todo!";
pub const DOC_RAE_GET_METHODS: &str = "todo!";
pub const DOC_RAE_GET_ENV: &str = "todo!";
pub const DOC_RAE_SET_EXEC_COMMAND: &str = "todo!";
pub const DOC_RAE_GET_EXEC_COMMAND: &str = "todo!";
pub const DOC_RAE_TRIGGER_EVENT: &str = "todo!";
pub const DOC_RAE_TRIGGER_TASK: &str = "todo!";
pub const DOC_DEF_TASK: &str = "todo!";
pub const DOC_DEF_METHOD: &str = "todo!";
pub const DOC_DEF_ACTION: &str = "todo!";

pub struct CtxRAE {
    pub stream: Option<Receiver<Job>>,
    pub log: String,
    pub actions_progress: ActionsProgress,
    pub agenda: Agenda,
    pub options: RAEOptions,
    //pub env: RAEEnv,
    pub env: RAEEnvBis,
}

impl Default for CtxRAE {
    fn default() -> Self {
        Self {
            stream: None,
            log: "".to_string(),
            actions_progress: Default::default(),
            agenda: Default::default(),
            options: RAEOptions::new(SelectOption::new(0, 0), "exec".into()),
            env: Default::default(),
        }
    }
}

impl CtxRAE {
    pub fn get_execution_status(&self, action_id: &ActionId) -> Option<&Status> {
        self.actions_progress.get_status(action_id)
    }
}

impl GetModule for CtxRAE {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp: vec![LAMBDA_DEF_TASK, LAMBDA_DEF_ACTION, LAMBDA_DEF_METHOD].into(),
            label: MOD_RAE,
        };

        module.add_fn_prelude(RAE_LAUNCH, Box::new(launch_rae));
        module.add_mut_fn_prelude(RAE_ADD_ACTION, Box::new(add_action));
        module.add_mut_fn_prelude(RAE_ADD_TASK, Box::new(add_task));
        module.add_mut_fn_prelude(RAE_ADD_METHOD, Box::new(add_method));
        module.add_fn_prelude(RAE_GET_METHODS, Box::new(get_methods));
        module.add_fn_prelude(RAE_GET_ACTIONS, Box::new(get_actions));
        module.add_fn_prelude(RAE_GET_TASKS, Box::new(get_tasks));
        module.add_fn_prelude(RAE_GET_ENV, Box::new(rae_get_env));
        module.add_mut_fn_prelude(RAE_SET_EXEC_COMMAND, Box::new(set_exec_command));
        module.add_fn_prelude(RAE_GET_EXEC_COMMAND, Box::new(get_exec_command));
        module.add_fn_prelude(RAE_TRIGGER_EVENT, Box::new(trigger_event));
        module.add_fn_prelude(RAE_TRIGGER_TASK, Box::new(trigger_task));

        module
    }
}

impl Documentation for CtxRAE {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(RAE_LAUNCH, DOC_RAE_LAUNCH, None),
            LHelp::new(RAE_ADD_ACTION, DOC_RAE_ADD_ACTION, None),
            LHelp::new(RAE_ADD_TASK, DOC_RAE_ADD_TASK, None),
            LHelp::new(RAE_ADD_METHOD, DOC_RAE_ADD_METHOD, None),
            LHelp::new(RAE_GET_METHODS, DOC_RAE_GET_METHODS, None),
            LHelp::new(RAE_GET_ENV, DOC_RAE_GET_ENV, None),
            LHelp::new(RAE_SET_EXEC_COMMAND, DOC_RAE_SET_EXEC_COMMAND, None),
            LHelp::new(RAE_GET_EXEC_COMMAND, DOC_RAE_GET_EXEC_COMMAND, None),
            LHelp::new(RAE_TRIGGER_TASK, DOC_RAE_TRIGGER_TASK, None),
            LHelp::new(RAE_TRIGGER_EVENT, DOC_RAE_TRIGGER_EVENT, None),
            LHelp::new(DEF_ACTION, DOC_DEF_ACTION, None),
            LHelp::new(DEF_TASK, DOC_DEF_TASK, None),
            LHelp::new(DEF_METHOD, DOC_DEF_METHOD, None),
        ]
    }
}

/*
launch the main in a thread
 */
pub fn launch_rae(_: &[LValue], _env: &LEnv, _: &CtxRAE) -> Result<LValue, LError> {
    todo!()
}

//Add an event to the stream of RAE
//access asynchronously to the stream
pub fn trigger_event(_: &[LValue], _env: &LEnv, _: &CtxRAE) -> Result<LValue, LError> {
    todo!()
}

pub fn trigger_task(_: &[LValue], _env: &LEnv, _: &CtxRAE) -> Result<LValue, LError> {
    todo!()
}

///Add an action to RAE env

pub fn add_action(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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
pub fn add_method(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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
pub fn add_task(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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
pub fn add_action(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    if let LValue::Symbol(action_label) = &args[0] {
        if let LValue::Lambda(body) = &args[1] {
            ctx.env.add_action(action_label.to_string(), body.clone());
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
pub fn add_method(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    if args.len() != 3 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 3..3));
    }

    if let LValue::Symbol(method_label) = &args[0] {
        if let LValue::Symbol(task_label) = &args[1] {
            if let LValue::Lambda(body) = &args[2] {
                ctx.env.add_method(
                    method_label.to_string(),
                    &task_label.to_string(),
                    body.clone(),
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
pub fn add_task(args: &[LValue], _env: &mut LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
    }

    if let LValue::Symbol(task_label) = &args[0] {
        if let LValue::Lambda(body) = &args[1] {
            ctx.env.add_task(task_label.to_string(), body.clone());
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

///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_METHOD_LIST).unwrap())
}

pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_ACTION_LIST).unwrap())
}

pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_TASK_LIST).unwrap())
}

///Add a task to RAE env
/*pub fn set_exec_command(
    args: &[LValue],
    _env: &mut LEnv,
    ctx: &mut CtxRAE,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    if let LValue::Symbol(exec) = &args[0] {
        ctx.options.set_exec_command(exec.clone())
    }
    Ok(LValue::Nil)
}

pub fn get_exec_command(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    if !args.is_empty() {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 0..0));
    }
    Ok(LValue::Symbol(ctx.options.get_exec_command().clone()))
}

pub fn rae_get_env(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
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

    Ok(ctx.env.get_env(key).into())
}*/

///Add a task to RAE env
pub fn set_exec_command(
    args: &[LValue],
    _env: &mut LEnv,
    ctx: &mut CtxRAE,
) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }
    if let LValue::Symbol(_) = &args[0] {
        ctx.env.set_exec_command(args[0].clone());
    }
    Ok(LValue::Nil)
}

pub fn get_exec_command(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_exec_command())
}

pub fn rae_get_env(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
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
    //Ok(ctx.env.get_env(key).into())
}
