use crate::context::rae_env::{Action, StateFunction};
use crate::context::rae_state::{LState, StateType};
use crate::module::{CtxRae, MOD_RAE};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::language::*;
use ompas_lisp::core::root_module::list::cons;
use ompas_lisp::core::root_module::predicate::language::*;
use ompas_lisp::core::structs::contextcollection::Context;
use ompas_lisp::core::structs::documentation::Documentation;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::lvalues::LValueS;
use ompas_lisp::core::structs::module::{IntoModule, Module};
use ompas_lisp::core::structs::purefonction::PureFonctionCollection;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_lisp::core::{eval, expand};
use ompas_utils::dyn_async;
use std::convert::TryInto;

/*
LANGUAGE
 */
pub const MOD_RAE_DESCRIPTION: &str = "rae-description";

pub const GENERATE_TASK: &str = "generate-task";
pub const GENERATE_STATE_FUNCTION: &str = "generate-state-function";
pub const GENERATE_ACTION: &str = "generate-action";
pub const GENERATE_ACTION_MODEL: &str = "generate-action-model";
pub const GENERATE_ACTION_OPERATIONAL_MODEL: &str = "generate-action-operational-model";
pub const GENERATE_METHOD: &str = "generate-method";

pub const RAE_DEF_STATE_FUNCTION: &str = "def-state-function";
pub const RAE_DEF_ACTION: &str = "def-action";
pub const RAE_DEF_ACTION_MODEL: &str = "def-action-model";
pub const RAE_DEF_ACTION_OPERATIONAL_MODEL: &str = "def-action-operational-model";
pub const RAE_DEF_TASK: &str = "def-task";
pub const RAE_DEF_METHOD: &str = "def-method";
pub const RAE_DEF_LAMBDA: &str = "def-lambda";
pub const RAE_DEF_INITIAL_STATE: &str = "def-initial-state";

pub const DOC_DEF_STATE_FUNCTION: &str = "Insert a state function in RAE environment.";
pub const DOC_DEF_STATE_FUNCTION_VERBOSE: &str =
    "Example:\n(def-state-function robot.coordinates ?r)";
pub const DOC_DEF_ACTION: &str = "Insert an action in RAE environment.";
pub const DOC_DEF_ACTION_VERBOSE: &str = "Example:\n(def-action pick ?r)";
pub const DOC_DEF_TASK: &str = "Insert a task in RAE environment";
pub const DOC_DEF_TASK_VERBOSE: &str = "Example:\n(def-task t_navigate_to ?r ?x ?y)";
pub const DOC_DEF_METHOD: &str = "Insert a method in RAE environment.";
pub const DOC_DEF_METHOD_VERBOSE: &str =
    "Example:\n(def-method m_navigate_to '((:task t_navigate_to)(:params ?r ?x ?y)(:body (begin\n\
        \t(rae-await (navigate_to ?r ?x ?y))\n\
        \t(rae-await (navigate_to ?r (+ ?x 1) (+ ?y 1)))))))";
pub const DOC_DEF_LAMBDA: &str = "Add a lambda to RAE environment";
pub const DOC_DEF_INITIAL_STATE: &str = "Add initial facts in the state.\
Most of the time it is general knowledge and not initialisation of facts.";

/// Macro used to generate code to define a task in the simplified representation in RAE environment.
pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task 
    (lambda args
    (let* ((label (car args))
          (p_expr (cdr args))
          (params (car (unzip p_expr))))
         `(list ,label 
            (quote ,p_expr)
            (lambda ,params
                ,(cons 'progress (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a state function in RAE environment.
pub const MACRO_GENERATE_STATE_FUNCTION: &str = "(defmacro generate-state-function (lambda args
    (let* ((label (car args))
          (p_expr (cdr args))
          (params (car (unzip p_expr))))
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
                    ,(cons 'rae-exec-command (cons `(quote ,label) params)))))))";

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
                        (check ,(gtpc p_expr))
                        (check ,conds)
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
                        (check ,(gtpc p_expr))
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
                        (check ,(gtpc p_expr))
                        (check ,conds)))
                (lambda ,params ,score)
                (lambda ,params ,body))))))";

/// Macro used to generate code to define a method in RAE environment.
/*pub const MACRO_GENERATE_METHOD_PARAMETERS: &str =
    "(defmacro generate-method-parameters (lambda args
    (let ((label (car args))
            (args_enum (cdr args)))

        (quasiquote (quote (unquote
            (list label
            (let ((p_enum (car args_enum))
                (p_labels (caadr args_enum))
                (conds (cadadr args_enum)))

                (quasiquote
                    ((unquote begin)
                        (define eval_params ((unquote lambda) args
                            (let ((params (car args)))
                                (if (not (null? params))
                                    (if (eval (cons ((unquote lambda) (unquote p_labels) (unquote conds)) params))
                                        (cons params (eval_params (cdr args)))
                                        (eval_params (cdr args)))
                                    nil))))
                        (eval_params (unquote (cons enumerate p_enum)))))))))))))";

pub const GENERATE_METHOD_PARAMETERS: &str = "generate-method-parameters";

/// Macro to define lambda that will evaluates set of parameters that can instantiate a method in a given state.
pub const MACRO_ENUMERATE_PARAMS: &str = "(defmacro enumerate-params (lambda args
    (let ((p_enum (car args))
        (p_labels (caadr args))
        (conds (cadadr args)))

        (quasiquote
            (begin
                (define eval_params (lambda args
                    (let ((params (car args)))
                        (if (not (null? params))
                            (if (eval (cons (lambda (unquote p_labels) (unquote conds)) params))
                                (cons params (eval_params (cdr args)))
                                (eval_params (cdr args)))
                            nil))))
                (eval_params (unquote (cons enumerate p_enum))))))))";*/

const GENERATE_TYPE_TEST_EXPR: &str = "generate-type-test-expr";

const LAMBDA_GENERATE_TYPE_PRE_CONDITIONS: &str =
    "(define gtpc (lambda (l) (parse (generate-type-test-expr l))))";

#[derive(Default)]
pub struct CtxRaeDescription {}

impl IntoModule for CtxRaeDescription {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(()),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_GENERATE_TASK,
                MACRO_GENERATE_STATE_FUNCTION,
                MACRO_GENERATE_ACTION,
                MACRO_GENERATE_ACTION_MODEL,
                MACRO_GENERATE_ACTION_OPERATIONAL_MODEL,
                MACRO_GENERATE_METHOD,
                //MACRO_ENUMERATE_PARAMS,
                LAMBDA_GENERATE_TYPE_PRE_CONDITIONS,
            ]
            .into(),
            label: MOD_RAE_DESCRIPTION.to_string(),
        };

        module.add_fn_prelude(GENERATE_TYPE_TEST_EXPR, generate_type_test_expr);
        module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![].into()
    }
}

/// Takes as input a p_expr of the form ((p1 p1_type) ... (p_n pn_type))
pub fn generate_type_test_expr(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            GENERATE_TYPE_TEST_EXPR,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(params) = &args[0] {
        let mut str = "(and ".to_string();

        for param in params {
            if let LValue::List(param) = &param {
                if param.len() == 2 {
                    if let LValue::Symbol(par) = &param[0] {
                        if let LValue::Symbol(tpe) = &param[1] {
                            match tpe.as_str() {
                                LIST => str.push_str(format!("({} {})", IS_LIST, par).as_str()),
                                BOOL => str.push_str(format!("({} {})", IS_BOOL, par).as_str()),
                                INT => str.push_str(format!("({} {})", IS_INT, par).as_str()),
                                FLOAT => str.push_str(format!("({} {})", IS_FLOAT, par).as_str()),
                                NUMBER => str.push_str(format!("({} {})", IS_NUMBER, par).as_str()),
                                SYMBOL => str.push_str(format!("({} {})", IS_SYMBOL, par).as_str()),
                                _ => str.push_str(format!("(instance {} {})", par, tpe).as_str()),
                            }
                        } else {
                            return Err(WrongType(
                                GENERATE_TYPE_TEST_EXPR,
                                param[1].clone(),
                                (&param[1]).into(),
                                TypeLValue::Symbol,
                            ));
                        }
                    } else {
                        return Err(WrongType(
                            GENERATE_TYPE_TEST_EXPR,
                            param[0].clone(),
                            (&param[0]).into(),
                            TypeLValue::Symbol,
                        ));
                    }
                } else {
                    return Err(WrongNumberOfArgument(
                        GENERATE_TYPE_TEST_EXPR,
                        param.into(),
                        param.len(),
                        2..2,
                    ));
                }
            } else {
                return Err(WrongType(
                    GENERATE_TYPE_TEST_EXPR,
                    param.clone(),
                    param.into(),
                    TypeLValue::List,
                ));
            }
        }
        str.push(')');

        Ok(LValue::String(str))
    } else if let LValue::Nil = &args[0] {
        Ok(LValue::String("true".to_string()))
    } else {
        Err(WrongType(
            GENERATE_TYPE_TEST_EXPR,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

/// Defines a lambda in RAE environment.
#[macro_rules_attribute(dyn_async!)]
pub async fn def_lambda<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_LAMBDA,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    if let LValue::List(list) = &args[0] {
        if let LValue::Symbol(label) = &list[0] {
            let expanded = expand(&list[1], true, &mut env).await?;
            let mut e = LEnv::root().await;
            let result = eval(&expanded, &mut e).await?;
            if let LValue::Lambda(_) = &result {
                ctx.get_rae_env()
                    .write()
                    .await
                    .add_lambda(label.clone(), result);
            }
        }
    }
    Ok(LValue::Nil)
}

/// Defines a state function in RAE environment.
#[macro_rules_attribute(dyn_async!)]
pub async fn def_state_function<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_STATE_FUNCTION,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let lvalue = cons(&[GENERATE_STATE_FUNCTION.into(), args.into()], env)?;
    let mut e = LEnv::root().await;

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                    ctx.get_rae_env().write().await.add_state_function(
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
pub async fn def_action_model<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION_MODEL,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION_MODEL.into(), args.into()], env)?;
    let mut e = LEnv::root().await;

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                ctx.get_rae_env()
                    .write()
                    .await
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
pub async fn def_action_operational_model<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
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
    )?;
    let mut e = LEnv::root().await;

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                ctx.get_rae_env()
                    .write()
                    .await
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
pub async fn def_action<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_ACTION,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_ACTION.into(), args.into()], env)?;
    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let mut e = LEnv::root().await;
    let mut env = ctx.get_rae_env().read().await.env.clone();
    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                    ctx.get_rae_env().write().await.add_action(
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
pub async fn def_method<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_METHOD,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_METHOD.into(), args.into()], env)?;

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let mut e = LEnv::root().await;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                                    ctx.get_rae_env().write().await.add_method(
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
pub async fn def_task<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            RAE_DEF_TASK,
            args.into(),
            args.len(),
            1..std::usize::MAX,
        ));
    }

    let lvalue = cons(&[GENERATE_TASK.into(), args.into()], env)?;

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

    let mut e = LEnv::root().await;
    let mut env = ctx.get_rae_env().read().await.env.clone();

    let lvalue = eval(&expand(&lvalue, true, &mut env).await?, &mut e).await?;

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
                ctx.get_rae_env().write().await.add_task(
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
pub async fn def_initial_state<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_DEF_INITIAL_STATE,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let ctx = env.get_context::<CtxRae>(MOD_RAE)?;

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

        ctx.get_rae_env()
            .write()
            .await
            .state
            .update_state(state)
            .await;
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
/// TODO: Test des macros
#[cfg(test)]
mod test {
    use crate::module::rae_description::*;
    use crate::module::rae_exec::CtxRaeExec;
    use crate::module_exec::CtxRaeExec;
    use ompas_lisp::core::structs::contextcollection::ContextCollection;
    use ompas_lisp::core::structs::lenv::ImportType::WithoutPrefix;
    use ompas_lisp::core::structs::lenv::LEnv;
    use ompas_lisp::core::structs::lerror::LError;
    use ompas_lisp::modules::advanced_math::CtxMath;
    use ompas_lisp::modules::io::CtxIo;
    use ompas_lisp::modules::utils::CtxUtils;
    use ompas_lisp::test_utils::{test_expression, test_expression_with_env, TestExpression};

    async fn init_env_and_ctxs() -> LEnv {
        let mut env = LEnv::root().await;

        env.import(CtxUtils::default(), WithoutPrefix)
            .await
            .expect("error loading utils");

        env.import(CtxMath::default(), WithoutPrefix)
            .await
            .expect("error loading math");

        env.import(CtxRaeExec::default(), WithoutPrefix)
            .await
            .expect("error loading rae exec");

        env.import(CtxRaeDescription::default(), WithoutPrefix)
            .await
            .expect("error loading rae description");

        env.import(CtxIo::default(), WithoutPrefix)
            .await
            .expect("error loading io");

        env
    }

    #[tokio::test]
    async fn test_macro_generate_task_simple() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_TASK_SIMPLE,
            dependencies: vec![],
            expression: "(generate-task-simple t_navigate_to (?r robot) (?x int) (?y int))",
            expanded: "(list \
                        t_navigate_to
                        '((?r robot) (?x int) (?y int))
                        (lambda (?r ?x ?y)
                            (progress 't_navigate_to ?r ?x ?y)))",
            result: "(list \
                        t_navigate_to
                        '((?r robot) (?x int) (?y int))
                        (lambda (?r ?x ?y)
                            (progress 't_navigate_to ?r ?x ?y)))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_state_function() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_STATE_FUNCTION,
            dependencies: vec![],
            expression: "(generate-state-function sf (?a object) (?b object) (?c object))",
            expanded: "(list sf
                            '((?a object) (?b object) (?c object))
                            (lambda (?a ?b ?c)
                                (if (= rae-mode exec-mode)
                                (rae-get-state-variable 'sf ?a ?b ?c)
                                (get-map state (list 'sf ?a ?b ?c)))))",
            result: "(list sf
                        '((?a object) (?b object) (?c object))
                        (lambda (?a ?b ?c)
                            (if (= rae-mode exec-mode)
                            (rae-get-state-variable 'sf ?a ?b ?c)
                            (get-map state (list 'sf ?a ?b ?c)))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await?;

        let macro_to_test_2 = TestExpression {
            inner: MACRO_GENERATE_STATE_FUNCTION,
            dependencies: vec![],
            expression: "(generate-state-function sf)",
            expanded: "(list sf
                            'nil
                            (lambda nil 
                                (if (= rae-mode exec-mode)
                                    (rae-get-state-variable 'sf)
                                    (get-map state 'sf))))",
            result: "(list sf
                        'nil
                        (lambda nil 
                            (if (= rae-mode exec-mode)
                                (rae-get-state-variable 'sf)
                                (get-map state 'sf))))",
        };
        test_expression_with_env(macro_to_test_2, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION,
            dependencies: vec![],
            expression: "(generate-action pick_package (?r robot) (?p package))",
            expanded: "(list pick_package
                            '((?r robot) (?p package))
                            (lambda (?r ?p)
                                (if (= rae-mode exec-mode) 
                                    (begin 
                                        (if (rae-platform?)
                                            (rae-exec-command (quote pick_package) ?r ?p)
                                            ((get-action-model (quote pick_package)) ?r ?p))
                                        (print \"action \" (quote pick_package) params \"executed !\"))
                                    ((get-action-model (quote pick_package)) ?r ?p))))",
            result: "(list pick_package
                        '((?r robot) (?p package))
                        (lambda (?r ?p)
                            (if (= rae-mode exec-mode) 
                                (begin 
                                    (if (rae-platform?)
                                        (rae-exec-command (quote pick_package) ?r ?p)
                                        ((get-action-model (quote pick_package)) ?r ?p))
                                    (print \"action \" (quote pick_package) params \"executed !\"))
                                ((get-action-model (quote pick_package)) ?r ?p))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action_model() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION_MODEL,
            dependencies: vec![],
            expression: "(generate-action-model pick
                ((:params (?r robot))
                  (:pre-conditions (> (robot.battery ?r) 0.4))
                  (:effects
                        (assert (robot.busy ?r) true))))",
            expanded: "(list pick
                            (lambda (?r)
                                (if (instance ?r robot)
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) true)))
                                    )))",
            result: "(list pick
                            (lambda (?r)
                                (if (instance ?r robot)
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) true)))
                                    )))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action_operational_model() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION_OPERATIONAL_MODEL,
            dependencies: vec![],
            expression: "(generate-action-operational-model place
                        ((:params (?r robot))
                          (:body
                            (if (> (robot.battery ?r) 0.4)
                                (assert (robot.busy ?r) false)
                                (failure)))))",
            expanded: "(list place
                            (lambda (?r)
                                (if (instance ?r robot)
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) false)
                                        (failure)))
                                    ))",
            result: "(list place
                            (lambda (?r)
                                (if (instance ?r robot)
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) false)
                                        (failure)))
                                    ))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_lambda_generate_type_pre_conditions() -> Result<(), LError> {
        let lambda_test = TestExpression {
            inner: LAMBDA_GENERATE_TYPE_PRE_CONDITIONS,
            dependencies: vec![],
            expression:
                "(gtpc '((?r robot) (?f float ) (?i int) (?b bool) (?s symbol) (?n number) (?l list)))",
            expanded: "(gtpc '((?r robot) (?f float ) (?i int) (?b bool) (?s symbol) (?n number) (?l list)))",
            result: "(if (instance ?r robot)
                        (if (float? ?f)
                            (if (int? ?i)
                                (if (bool? ?b)
                                    (if (symbol? ?s)
                                        (if (number? ?n)
                                            (list? ?l)))))))",
        };
        let mut env = init_env_and_ctxs().await;

        test_expression_with_env(lambda_test, &mut env, false).await
    }

    #[tokio::test]
    async fn test_macro_generate_method() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_METHOD,
            dependencies: vec![LAMBDA_GENERATE_TYPE_PRE_CONDITIONS],
            expression: "(generate-method m_navigate_to ((:task t_navigate_to)
            (:params (?r robot) (?x float) (?y float))
            (:pre-conditions (and (robot.available ?r) (< ?x 10) (< ?y 10)))
            (:score 0)
            (:body
            (begin
                (navigate_to ?r ?x ?y)))))",
            expanded: "(list m_navigate_to
    't_navigate_to
    '((?r robot) (?x float) (?y float))
    (lambda (?r ?x ?y)
        (if (if (instance ?r robot)
                (if (float? ?x)
                    (float? ?y)))
            (if (if (robot.available ?r)
                    (if (< ?x 10)
                        (< ?y 10)))
                true)))
    (lambda (?r ?x ?y) 0 )
    (lambda (?r ?x ?y)
        (begin
            (navigate_to ?r ?x ?y))))",
            result: "(list m_navigate_to
    't_navigate_to
    '((?r robot) (?x float) (?y float))
    (lambda (?r ?x ?y)
        (if (if (instance ?r robot)
                (if (float? ?x)
                    (float? ?y)))
            (if (if (robot.available ?r)
                    (if (< ?x 10)
                        (< ?y 10)))
                true)))
    (lambda (?r ?x ?y) 0 )
    (lambda (?r ?x ?y)
        (begin
            (navigate_to ?r ?x ?y))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }
}
