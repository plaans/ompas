use crate::rae::context::*;
use crate::rae::domain::*;
use crate::rae::job::*;
use crate::rae::state::{ActionStatus, RAEState};
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::LValue::Nil;
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue, LNumber};
use ompas_modules::doc::{Documentation, LHelp};
use std::any::Any;
use tokio::sync::mpsc::{Receiver, Sender};
use ompas_lisp::functions::union_map;
use tokio::sync::{mpsc, Mutex};
use crate::rae::{TOKIO_CHANNEL_SIZE, rae_run};
use std::thread;
use std::sync::Arc;

//Others functions could be add to interogate and launch rae.

/*
LANGUAGE
 */

pub const MOD_RAE: &str = "mod-rae";

pub const RAE_LAUNCH: &str = "rae-launch";
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
pub const RAE_DO: &str ="rae-do";

//RAE Interface with a platform
pub const RAE_EXEC_COMMAND: &str = "rae-exec-command";
pub const RAE_GET_STATE: &str = "rae-get-state";
pub const RAE_GET_STATE_VARIBALE: &str = "rae-get-state-variable";
pub const RAE_LAUNCH_PLATFORM: &str = "rae-launch-platform";
pub const RAE_GET_STATUS: &str = "rae-get-status";
pub const RAE_CANCEL_COMMAND: &str = "rae-cancel-command";

//manage facts
pub const RAE_ASSERT: &str = "assert";
pub const RAE_ASSERT_SHORT: &str = "+>";
pub const RAE_RETRACT: &str = "retract";
pub const RAE_RETRACT_SHORT: &str ="->";

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

pub struct JobStream {
    sender : Sender<Job>,
    receiver: Receiver<Job>,
}

impl JobStream {
    pub fn new(sender: Sender<Job>, receiver: Receiver<Job>) -> Self{
        Self {
            sender,
            receiver
        }
    }

    pub fn get_sender(&self) -> Sender<Job> {
        self.sender.clone()
    }

    pub fn get_ref_receiver(&mut self) -> &mut Receiver<Job> {
        &mut self.receiver
    }
}

pub struct CtxRAE {
    pub stream: JobStream,
    pub log: String,
    pub actions_progress: ActionsProgress,
    pub agenda: Agenda,
    pub options: RAEOptions,
    //pub env: RAEEnv,
    pub env: RAEEnv,
    pub state: RAEState,
    pub platform_interface: Box<dyn RAEInterface>,
}

impl Default for CtxRAE {
    fn default() -> Self {
        let (sender, receiver) = mpsc::channel(TOKIO_CHANNEL_SIZE);

        Self {
            stream: JobStream::new(sender, receiver),
            log: "".to_string(),
            actions_progress: Default::default(),
            agenda: Default::default(),
            options: RAEOptions::new(SelectOption::new(0, 0)),
            env: Default::default(),
            state: Default::default(),
            platform_interface: Box::new(()),
        }
    }
}

pub trait RAEInterface: Any+Send+Sync {
    ///Execute a command on the platform
    fn exec_command(&self, args: &[LValue], command_id: usize) -> Result<LValue, LError>;

    fn cancel_command(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get the whole state of the platform
    fn get_state(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Get a specific state variable
    fn get_state_variable(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Return the status of all the actions
    fn get_status(&self, args: &[LValue]) -> Result<LValue, LError>;

    ///Launch the platform (such as the simulation in godot)
    fn launch_platform(&mut self, args: &[LValue]) -> Result<LValue, LError>;

    fn get_action_status(&self, action_id: &usize) -> Status;
    fn set_status(&self, action_id: usize, status: Status);

    fn domain(&self) -> &'static str;
}

impl RAEInterface for () {
    fn exec_command(&self, _args: &[LValue], _: usize) -> Result<LValue, LError> {
        todo!()
    }

    fn cancel_command(&self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_state(&self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_state_variable(&self, _args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_status(&self, _args: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn launch_platform(&mut self, _: &[LValue]) -> Result<LValue, LError> {
        todo!()
    }

    fn get_action_status(&self, _action_id: &usize) -> Status {
        todo!()
    }

    fn set_status(&self, _action_id: usize, _status: Status) {
        todo!()
    }

    fn domain(&self) -> &'static str {
        todo!()
    }
}
impl CtxRAE {
    pub fn get_execution_status(&self, action_id: &ActionId) -> Option<&Status> {
        self.actions_progress.get_status(action_id)
    }

    pub fn add_platform(&mut self, platform: Box<dyn RAEInterface>) {
        self.platform_interface = platform;
    }
}

impl GetModule for CtxRAE {
    fn get_module(self) -> Module {
        let domain = self.platform_interface.domain();
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_DEF_STATE_FUNCTION,
                MACRO_DEF_TASK,
                MACRO_DEF_ACTION,
                MACRO_DEF_METHOD,
                domain,
            ]
            .into(),
            label: MOD_RAE,
        };

        //primitives for RAE Domain
        module.add_fn_prelude(RAE_LAUNCH, launch_rae);
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


        //Interface with platform
        module.add_fn_prelude(RAE_GET_STATE, get_state);
        module.add_fn_prelude(RAE_GET_STATE_VARIBALE, get_state_variable);
        module.add_mut_fn_prelude(RAE_EXEC_COMMAND, exec_command);
        module.add_mut_fn_prelude(RAE_LAUNCH_PLATFORM, launch_platform);
        module.add_fn_prelude(RAE_GET_STATUS, get_status);
        module.add_fn_prelude(RAE_CANCEL_COMMAND, cancel_command);

        //Manage facts:
        module.add_mut_fn_prelude(RAE_ASSERT, assert_fact);
        module.add_mut_fn_prelude(RAE_ASSERT_SHORT, assert_fact);
        module.add_mut_fn_prelude(RAE_RETRACT, retract_fact);
        module.add_mut_fn_prelude(RAE_RETRACT_SHORT, retract_fact);
        module.add_fn_prelude(RAE_DO, fn_do);

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
            LHelp::new(DEF_ACTION, DOC_DEF_ACTION, None),
            LHelp::new(DEF_TASK, DOC_DEF_TASK, None),
            LHelp::new(DEF_METHOD, DOC_DEF_METHOD, None),
        ]
    }
}

/*
launch the main in a thread
 */
pub fn launch_rae(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    let options = SelectOption::new(0,0);
    let mutex = Arc::new(Mutex::new(ctx));
    /*tokio::spawn(async move {
        rae_run(mutex, &options, "rae-log.txt".to_string());
    });*/
    Ok(LValue::String("rae launched succesfully".to_string()))
}


///Monitor the status of an action that has been triggered
/// Return true if the action is a success, false otherwise
pub fn fn_do(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1))
    }

    let action_id = &args[0];

    if let LValue::Number(LNumber::Usize(action_id)) = action_id {
        loop {
            let status = ctx.platform_interface.get_action_status(action_id);
            match status {
                Status::NotTriggered => {
                    //println!("not triggered");
                }
                Status::Running => {
                    //println!("running");
                }
                Status::Failure => {
                    println!("command is a failure");
                    return Ok(false.into())
                }
                Status::Done => {
                    println!("command is a success");
                    return Ok(true.into())
                }
            }
        }
    }else {
        return Err(WrongType(action_id.clone(), action_id.into(), NameTypeLValue::Usize))
    }
}


///Retract a fact to state
pub fn retract_fact(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2))
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    ctx.state.retract_fact(key, value)?;
    Ok(Nil)
}

///Add a fact to fact state
pub fn assert_fact(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2))
    }
    let key = args[0].clone().into();
    let value = args[1].clone().into();
    ctx.state.add_fact(key, value);
    Ok(Nil)
}

pub fn add_state_function(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRAE,
) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 2..2));
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

///Add an action to RAE env
pub fn add_action(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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
pub fn add_method(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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
pub fn add_task(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
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

//RAEInterface with platform

pub fn launch_platform(
    args: &[LValue],
    _env: &LEnv,
    ctx: &mut CtxRAE,
) -> Result<LValue, LError> {
    ctx.platform_interface.launch_platform(args)
}

pub fn exec_command(args: &[LValue], _env: &LEnv, ctx: &mut CtxRAE) -> Result<LValue, LError> {
    let command_id = ctx.actions_progress.get_new_id();
    ctx.platform_interface.exec_command(args, command_id)?;
    Ok(command_id.into())
}

pub fn get_state(args: &[LValue], env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    //TODO: à revoir
    let platform_state = ctx.platform_interface.get_state(args).unwrap();
    let state = ctx.state.get_state().into_map();
    union_map(&[platform_state, state], env, &())
}

pub fn get_state_variable(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    ctx.platform_interface.get_state_variable(args)
}

pub fn get_status(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    ctx.platform_interface.get_status(args)
}

pub fn cancel_command(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    ctx.platform_interface.cancel_command(args)
}


///Get the methods of a given task
pub fn get_methods(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_METHOD_LIST).unwrap())
}

///Get the list of actions in the environment
pub fn get_actions(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_ACTION_LIST).unwrap())
}

///Get the list of tasks in the environment
pub fn get_tasks(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_TASK_LIST).unwrap())
}

///Get the list of state functions in the environment
pub fn get_state_function(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_STATE_FUNCTION_LIST).unwrap())
}

pub fn get_symbol_type(_: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
    Ok(ctx.env.get_element(RAE_SYMBOL_TYPE).unwrap())
}

pub fn get_env(args: &[LValue], _env: &LEnv, ctx: &CtxRAE) -> Result<LValue, LError> {
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