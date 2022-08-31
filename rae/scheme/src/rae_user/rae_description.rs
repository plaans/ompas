use crate::rae_user::{CtxRaeUser, MOD_RAE_USER};
use ompas_rae_language::*;
use ompas_rae_structs::domain::command::Command;
use ompas_rae_structs::domain::method::Method;
use ompas_rae_structs::domain::parameters::Parameters;
use ompas_rae_structs::domain::state_function::StateFunction;
use ompas_rae_structs::domain::task::Task;
use ompas_rae_structs::state::partial_state::PartialState;
use ompas_rae_structs::state::world_state::StateType;
use sompas_core::modules::list::{car, cons};
use sompas_core::{eval, expand, get_root_env, parse};
use sompas_language::*;
use sompas_macros::*;
use sompas_structs::kindlvalue::KindLValue;
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use sompas_structs::{lruntimeerror, wrong_n_args, wrong_type};
use std::convert::TryInto;

const NAME: &str = ":name";
const TASK: &str = ":task";
const PARAMETERS: &str = ":params";
const PRE_CONDITIONS: &str = ":pre-conditions";
const BODY: &str = ":body";
const MODEL: &str = ":model";
const MODEL_TYPE: &str = ":model-type";
const EFFECTS: &str = ":effects";
const RESULT: &str = ":result";
const SCORE: &str = ":score";

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

pub const MACRO_DEF_INITIAL_STATE: &str = "(defmacro def-initial-state (lambda args
    `(add-facts (map ',args))))";

pub const MACRO_DEF_TYPES: &str = "(defmacro def-types (lambda args
    (cons 'add-types (quote-list args))))";
pub const MACRO_DEF_OBJECTS: &str = "(defmacro def-objects (lambda args
    (cons 'add-objects (quote-list args))))";

/// Takes as input a p_expr of the form ((p1 p1_type) ... (p_n pn_type))
#[async_scheme_fn]
pub async fn generate_test_type_expr(env: &LEnv, params: Vec<LValue>) -> LResult {
    if params.is_empty() {
        Ok(true.into())
    } else {
        let mut str = "(do ".to_string();

        for param in params {
            if let LValue::List(param) = &param {
                if param.len() == 2 {
                    if let LValue::Symbol(par) = &param[0] {
                        if let LValue::Symbol(tpe) = &param[1] {
                            let test = match tpe.as_str() {
                                LIST => {
                                    format!("({} {})", IS_LIST, par)
                                }
                                BOOL => format!("({} {})", IS_BOOL, par),
                                INT => format!("({} {})", IS_INT, par),
                                FLOAT => format!("({} {})", IS_FLOAT, par),
                                NUMBER => format!("({} {})", IS_NUMBER, par),
                                SYMBOL => format!("({} {})", IS_SYMBOL, par),
                                _ => format!("(instance {} {})", par, tpe),
                            };

                            str.push_str(format!("(check {})", test).as_str())
                        } else {
                            return Err(wrong_type!(
                                GENERATE_TEST_TYPE_EXPR,
                                &param[1],
                                KindLValue::Symbol
                            ));
                        }
                    } else {
                        return Err(wrong_type!(
                            GENERATE_TEST_TYPE_EXPR,
                            &param[0],
                            KindLValue::Symbol
                        ));
                    }
                } else {
                    return Err(wrong_n_args!(GENERATE_TEST_TYPE_EXPR, param, 2));
                }
            } else {
                return Err(wrong_type!(
                    GENERATE_TEST_TYPE_EXPR,
                    &param,
                    KindLValue::List
                ));
            }
        }
        str.push(')');

        let mut env = env.clone();
        expand(&parse(&str, &mut env).await?, true, &mut env).await
    }
}

/// Defines a lambda in RAE environment.
#[async_scheme_fn]
pub async fn add_lambda(env: &LEnv, label: String, lambda: &LValue) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();
    let mut env = ctx.get_empty_env();
    let expanded = expand(lambda, true, &mut env).await?;
    let mut e = get_root_env().await;
    let result = eval(&expanded, &mut e, None).await?;
    if let LValue::Lambda(_) = &result {
        ctx.rae_domain.write().await.add_lambda(label, result);
    }
    Ok(())
}

/// Defines a state function in RAE environment.
#[async_scheme_fn]
pub async fn add_state_function(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut new_env = ctx.get_empty_env();
    let label = map.get(&NAME.into()).unwrap();
    let params: Parameters = map
        .get(&PARAMETERS.into())
        .unwrap_or(&Default::default())
        .try_into()?;
    let result = car(
        env,
        &[map
            .get(&RESULT.into())
            .ok_or_else(|| {
                LRuntimeError::new(
                    RAE_ADD_STATE_FUNCTION,
                    format!("No a :result for {}", label),
                )
            })?
            .clone()],
    )?
    .try_into()?;
    let expr = format!(
        "(lambda {}
                (read-state '{} {})))",
        params.get_params_as_lvalue(),
        label,
        {
            let mut str = String::new();
            for p in params.get_params() {
                str.push_str(p.as_str());
                str.push(' ');
            }
            str
        }
    );
    let body = eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?;
    let state_function = StateFunction::new(label.to_string(), params, result, body);
    ctx.rae_domain
        .write()
        .await
        .add_state_function(label.to_string(), state_function)?;
    Ok(())
}

/// Defines an action in RAE environment.
#[async_scheme_fn]
pub async fn add_command(
    env: &LEnv,
    map: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_ADD_COMMAND,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut env = ctx.get_empty_env();
    let mut command = Command::default();
    command.set_label(map.get(&NAME.into()).unwrap().to_string());
    command.set_parameters(map.get(&PARAMETERS.into()).unwrap().try_into()?);
    let params = command.get_parameters().get_params_as_lvalue();
    let params_list = command.get_parameters().get_params();
    let lv_exec: LValue = parse(
        &format!(
            "(lambda {} (await (exec-command '{} {})))",
            params,
            command.get_label(),
            {
                let mut str = String::new();
                for p in params_list {
                    str.push_str(p.as_str());
                    str.push(' ');
                }
                str
            }
        ),
        &mut env,
    )
    .await?;

    let exec = eval(&expand(&lv_exec, true, &mut env).await?, &mut env, None).await?;

    command.set_body(exec);
    let lv_model: LValue = match map.get(&MODEL.into()) {
        None => parse(&format!("(lambda {} nil)", params), &mut env).await?,
        Some(model) => parse(&format!("(lambda {} {})", params, model), &mut env).await?,
    };
    let model = eval(&expand(&lv_model, true, &mut env).await?, &mut env, None).await?;
    command.set_model(model);
    ctx.rae_domain
        .write()
        .await
        .add_command(command.get_label().to_string(), command)?;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_task(env: &LEnv, map: im::HashMap<LValue, LValue>) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_ADD_TASK,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut env = ctx.get_empty_env();

    let mut task = Task::default();
    task.set_label(map.get(&NAME.into()).unwrap().to_string());
    task.set_parameters(
        map.get(&PARAMETERS.into())
            .unwrap_or(&LValue::Nil)
            .try_into()?,
    );
    let params = task.get_parameters().get_params_as_lvalue();
    let params_list = task.get_parameters().get_params();
    let lv_exec: LValue = parse(
        &format!(
            "(lambda {} (exec-task '{} {}))",
            params,
            task.get_label(),
            {
                let mut str = String::new();
                for p in params_list {
                    str.push_str(p.as_str());
                    str.push(' ');
                }
                str
            }
        ),
        &mut env,
    )
    .await?;
    let exec = eval(&expand(&lv_exec, true, &mut env).await?, &mut env, None).await?;

    task.set_body(exec);
    let lv_model: LValue = match map.get(&MODEL.into()) {
        None => parse(&format!("(lambda {} nil)", params), &mut env).await?,
        Some(model) => parse(&format!("(lambda {} {})", params, model), &mut env).await?,
    };
    let model = eval(&expand(&lv_model, true, &mut env).await?, &mut env, None).await?;
    task.set_model(model);
    ctx.rae_domain
        .write()
        .await
        .add_task(task.get_label().to_string(), task)?;

    Ok(())
}

/// Defines a method in RAE environment.
#[async_scheme_fn]
pub async fn add_method(env: &LEnv, map: im::HashMap<LValue, LValue>) -> Result<(), LRuntimeError> {
    if map.is_empty() {
        return Err(LRuntimeError::wrong_number_of_args(
            RAE_ADD_METHOD,
            &[map.into()],
            1..usize::MAX,
        ));
    }
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut new_env = ctx.get_empty_env();

    //Definition of the method
    let mut method = Method {
        label: map.get(&NAME.into()).unwrap().to_string(),
        task_label: car(env, &[map.get(&TASK.into()).unwrap().clone()])?.to_string(),
        parameters: map.get(&PARAMETERS.into()).unwrap().try_into()?,
        ..Default::default()
    };
    let conds = match map.get(&PRE_CONDITIONS.into()) {
        None => {
            let test =
                generate_test_type_expr(env, &[map.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            let expr = format!(
                "(lambda {} (do {}))",
                method.parameters.get_params_as_lvalue(),
                test
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
        Some(conds) => {
            let test =
                generate_test_type_expr(env, &[map.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            let conds = cons(&LEnv::default(), &[LCoreOperator::Do.into(), conds.clone()])?;
            let expr = format!(
                "(lambda {} (do {} {}))",
                method.parameters.get_params_as_lvalue(),
                test,
                conds
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
    };
    method.lambda_pre_conditions = conds;

    let score = match map.get(&SCORE.into()) {
        None => {
            let expr = format!("(lambda {} 0)", method.parameters.get_params_as_lvalue(),);
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
        Some(score) => {
            let expr = format!(
                "(lambda {} {})",
                method.parameters.get_params_as_lvalue(),
                car(env, &[score.clone()])?
            );
            eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?
        }
    };
    method.lambda_score = score;

    let conds = cons(
        &LEnv::default(),
        &[
            method.lambda_pre_conditions.clone(),
            method.parameters.get_params_as_lvalue(),
        ],
    )?;

    let expr = format!(
        "(lambda {} (do {} {}))",
        method.parameters.get_params_as_lvalue(),
        conds,
        car(
            &new_env,
            &[map.get(&BODY.into()).unwrap_or(&LValue::Nil).clone()]
        )?
    );

    method.lambda_body = eval(&parse(&expr, &mut new_env).await?, &mut new_env, None).await?;

    ctx.rae_domain
        .write()
        .await
        .add_method(method.label.clone(), method)?;

    Ok(())
}

pub enum ModelType {
    PDDL,
    OM,
}

impl TryFrom<&str> for ModelType {
    type Error = LRuntimeError;

    fn try_from(s: &str) -> Result<Self, Self::Error> {
        match s {
            "pddl" => Ok(Self::PDDL),
            "om" => Ok(Self::OM),
            _ => Err(LRuntimeError::default()),
        }
    }
}

async fn create_model(env: &mut LEnv, model: im::HashMap<LValue, LValue>) -> LResult {
    let model_type: ModelType = model
        .get(&MODEL_TYPE.into())
        .unwrap()
        .to_string()
        .as_str()
        .try_into()?;
    let str = match model_type {
        ModelType::PDDL => {
            let params: Parameters = model.get(&PARAMETERS.into()).unwrap().try_into()?;
            let conds = model.get(&PRE_CONDITIONS.into()).unwrap();
            let mut str_conds = "(do".to_string();
            if let LValue::List(conds) = conds {
                for cond in conds.iter() {
                    str_conds.push_str(format!("(check {})", cond).as_str());
                }
                str_conds.push(')');
            } else {
                return Err(LRuntimeError::default());
            }
            let effects = model.get(&EFFECTS.into()).unwrap();
            let effects = cons(env, &[LCoreOperator::Do.into(), effects.clone()])?;
            let test =
                generate_test_type_expr(env, &[model.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            format!(
                "(lambda {} (do {} {} {}))",
                params.get_params_as_lvalue(),
                test,
                str_conds,
                effects
            )
        }
        ModelType::OM => {
            let params: Parameters = model.get(&PARAMETERS.into()).unwrap().try_into()?;
            let body = car(env, &[model.get(&BODY.into()).unwrap().clone()])?;
            let test =
                generate_test_type_expr(env, &[model.get(&PARAMETERS.into()).unwrap().clone()])
                    .await?;
            format!(
                "(lambda {} (do {} {}))",
                params.get_params_as_lvalue(),
                test,
                body
            )
        }
    };

    eval(&parse(&str, env).await?, env, None).await
}

#[async_scheme_fn]
pub async fn add_command_model(
    env: &LEnv,
    model: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut env = ctx.get_empty_env();
    let label: String = model.get(&NAME.into()).unwrap().try_into()?;
    let model = create_model(&mut env, model).await?;
    ctx.rae_domain
        .write()
        .await
        .add_command_model(label, model)?;
    Ok(())
}

#[async_scheme_fn]
pub async fn add_task_model(
    env: &LEnv,
    model: im::HashMap<LValue, LValue>,
) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER)?;
    let mut env = ctx.get_empty_env();
    let label: String = model.get(&NAME.into()).unwrap().try_into()?;
    let model = create_model(&mut env, model).await?;
    ctx.rae_domain.write().await.add_task_model(label, model)?;
    Ok(())
}

///Takes in input a list of initial facts that will be stored in the inner world part of the State.
#[async_scheme_fn]
pub async fn add_facts(env: &LEnv, map: im::HashMap<LValue, LValue>) {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let mut inner_world = PartialState {
        inner: Default::default(),
        _type: Some(StateType::InnerWorld),
    };
    let mut instance = PartialState {
        inner: Default::default(),
        _type: Some(StateType::Instance),
    };

    for (k, v) in &map {
        let mut is_instance = false;
        if let LValue::List(list) = k {
            if list[0] == LValue::from(RAE_INSTANCE) {
                instance.insert(k.into(), v.into());
                is_instance = true;
            }
        }
        if !is_instance {
            inner_world.insert(k.into(), v.into());
        }
    }

    ctx.interface.state.update_state(inner_world).await;

    ctx.interface.state.update_state(instance).await;
}
#[async_scheme_fn]
pub async fn add_types(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    for arg in args {
        match arg {
            LValue::List(list) => {
                if list.len() < 2 {
                    return Err(lruntimeerror!(
                        RAE_ADD_TYPES,
                        format!("an objects is defined by a symbol and a type, got {}", arg)
                    ));
                }
                let last = list.last().unwrap();
                for t in &list[0..list.len() - 1] {
                    //println!("new type: {}", t);
                    add_type(env, &[t.clone(), last.clone()]).await?;
                }
            }
            lv => {
                add_type(env, &[lv.clone()]).await?;
            }
        }
    }
    Ok(())
}
#[async_scheme_fn]
pub async fn add_objects(env: &LEnv, args: Vec<Vec<LValue>>) -> Result<(), LRuntimeError> {
    for list in args {
        if list.len() < 2 {
            return Err(lruntimeerror!(
                RAE_ADD_OBJECTS,
                format!(
                    "an objects is defined by a symbol and a type, got {}",
                    LValue::from(list)
                )
            ));
        }
        let last = list.last().unwrap();
        for obj in &list[0..list.len() - 1] {
            add_object(env, &[obj.clone(), last.clone()]).await?;
        }
    }
    Ok(())
}

#[async_scheme_fn]
pub async fn add_type(env: &LEnv, args: &[LValue]) -> Result<(), LRuntimeError> {
    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let (t, parent) = match args.len() {
        1 => (args[0].to_string(), None),
        2 => (args[0].to_string(), Some(args[1].to_string())),
        _ => {
            return Err(LRuntimeError::wrong_number_of_args(
                RAE_ADD_TYPE,
                args,
                1..2,
            ))
        }
    };

    let mut instance = PartialState {
        inner: Default::default(),
        _type: Some(StateType::Instance),
    };

    ctx.rae_domain
        .write()
        .await
        .add_type(t.clone(), parent.clone());

    instance.insert(
        vec![LValueS::from(RAE_INSTANCE), LValue::from(&t).into()].into(),
        LValueS::List(vec![]),
    );

    if let Some(p) = &parent {
        let parent_instance: LValueS =
            vec![LValueS::from(RAE_INSTANCE), LValue::from(p).into()].into();
        if !instance.inner.contains_key(&parent_instance) {
            instance.insert(parent_instance, LValueS::List(vec![]))
        }
    }

    ctx.interface.state.update_state(instance).await;

    Ok(())
}

#[async_scheme_fn]
pub async fn add_object(env: &LEnv, constant: LValue, t: LValue) -> Result<(), LRuntimeError> {
    let constant: LValueS = constant.into();
    let t: LValueS = t.into();

    let ctx = env.get_context::<CtxRaeUser>(MOD_RAE_USER).unwrap();

    let mut instances: PartialState = ctx
        .interface
        .state
        .get_state(Some(StateType::Instance))
        .await;
    let key = vec![RAE_INSTANCE.into(), t.clone()].into();

    let objects: &mut LValueS = match instances.get_mut(&key) {
        Some(obj) => obj,
        None => {
            return Err(lruntimeerror!(
                RAE_ADD_OBJECT,
                format!("type {} is undefined", t)
            ))
        }
    };

    if let LValueS::List(l) = objects {
        if !l.contains(&constant) {
            l.push(constant)
        } else {
            return Err(lruntimeerror!(
                RAE_ADD_OBJECT,
                format!("{} already defined", constant)
            ));
        }
    }

    instances._type = Some(StateType::Instance);

    ctx.interface.state.set_state(instances).await;

    Ok(())
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::rae_exec::CtxRaeExec;
    use crate::rae_user::rae_description::CtxRaeDescription;
    use sompas_core::test_utils::{test_expression_with_env, TestExpression};
    use sompas_core::{eval_init, get_root_env};
    use sompas_modules::advanced_math::CtxMath;
    use sompas_modules::io::CtxIo;
    use sompas_modules::utils::CtxUtils;
    use sompas_structs::lenv::ImportType::WithoutPrefix;
    use sompas_structs::lenv::LEnv;

    async fn init_env_and_ctxs() -> LEnv {
        let mut env = get_root_env().await;

        env.import(CtxUtils::default(), WithoutPrefix);

        env.import(CtxMath::default(), WithoutPrefix);

        env.import(CtxRaeExec::default(), WithoutPrefix);

        env.import(CtxRaeDescription::default(), WithoutPrefix);

        env.import(CtxIo::default(), WithoutPrefix);
        eval_init(&mut env).await;
        env
    }

    #[tokio::test]
    async fn test_macro_generate_task() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_TASK,
            dependencies: vec![],
            expression: "(generate-task t_navigate_to (?r robot) (?x int) (?y int))",
            expected: "(list \
                        t_navigate_to
                        '((?r robot) (?x int) (?y int))
                        (lambda (?r ?x ?y)
                            (rae-exec-task 't_navigate_to ?r ?x ?y)))",
            result: "(list \
                        t_navigate_to
                        '((?r robot) (?x int) (?y int))
                        (lambda (?r ?x ?y)
                            (rae-exec-task 't_navigate_to ?r ?x ?y)))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_state_function() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_STATE_FUNCTION,
            dependencies: vec![],
            expression: "(generate-state-function sf (?a object) (?b object) (?c object))",
            expected: "(list sf
                            '((?a object) (?b object) (?c object))
                            (lambda (?a ?b)
                                (rae-get-state-variable 'sf ?a ?b)))",
            result: "(list sf
                            '((?a object) (?b object) (?c object))
                            (lambda (?a ?b)
                                (rae-get-state-variable 'sf ?a ?b)))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await?;

        let macro_to_test_2 = TestExpression {
            inner: MACRO_GENERATE_STATE_FUNCTION,
            dependencies: vec![],
            expression: "(generate-state-function sf)",
            expected: "(list sf
                            'nil
                            (lambda nil
                                (rae-get-state-variable 'sf)))",
            result: "(list sf
                            'nil
                            (lambda nil
                                (rae-get-state-variable 'sf)))",
        };
        test_expression_with_env(macro_to_test_2, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION,
            dependencies: vec![],
            expression: "(generate-action pick_package (?r robot) (?p package))",
            expected: "(list pick_package
                            '((?r robot) (?p package))
                            (lambda (?r ?p)
                                (rae-exec-command (quote pick_package) ?r ?p)))",
            result: "(list pick_package
                            '((?r robot) (?p package))
                            (lambda (?r ?p)
                                (rae-exec-command (quote pick_package) ?r ?p)))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION_MODEL,
            dependencies: vec![],
            expression: "(generate-action-model pick
                ((:params (?r robot))
                  (:pre-conditions (check (> (robot.battery ?r) 0.4)))
                  (:effects
                        (assert (robot.busy ?r) true))))",
            expected: "(list pick
                            (lambda (?r)
                                (do
                                    (do
                                        (check (instance ?r robot)))
                                    
                                        (check (> (robot.battery ?r) 0.4))
                                  (assert (robot.busy ?r) true))))",
            result: "(list pick
                            (lambda (?r)
                                (do
                                    (do
                                        (check (instance ?r robot)))
                                    
                                        (check (> (robot.battery ?r) 0.4))
                                  (assert (robot.busy ?r) true))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_macro_generate_action_operational_model() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_ACTION_OPERATIONAL_MODEL,
            dependencies: vec![],
            expression: "(generate-action-operational-model place
                        ((:params (?r robot))
                          (:body
                            (if (> (robot.battery ?r) 0.4)
                                (assert (robot.busy ?r) false)
                                (err 0)))))",
            expected: "(list place
                            (lambda (?r)
                                (do
                                    (do
                                        (check (instance ?r robot)))
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) false)
                                        (err 0)))))",
            result: "(list place
                            (lambda (?r)
                                (do
                                    (do
                                        (check (instance ?r robot)))
                                    (if (> (robot.battery ?r) 0.4)
                                        (assert (robot.busy ?r) false)
                                        (err 0)))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }

    #[tokio::test]
    async fn test_lambda_generate_type_pre_conditions() -> Result<(), LRuntimeError> {
        let lambda_test = TestExpression {
            inner: LAMBDA_GENERATE_TYPE_PRE_CONDITIONS,
            dependencies: vec![],
            expression:
                "(gtpc '((?r robot) (?f float ) (?i int) (?b bool) (?s symbol) (?n number) (?l list)))",
            expected: "(gtpc '((?r robot) (?f float ) (?i int) (?b bool) (?s symbol) (?n number) (?l list)))",
            result: "(do 
                        (check (instance ?r robot))
                        (check (float? ?f))
                        (check (int? ?i))
                        (check (bool? ?b))
                        (check (symbol? ?s))
                        (check (number? ?n))
                        (check (list? ?l)))",
        };
        let mut env = init_env_and_ctxs().await;

        test_expression_with_env(lambda_test, &mut env, false).await
    }

    #[tokio::test]
    async fn test_macro_generate_method() -> Result<(), LRuntimeError> {
        let macro_to_test = TestExpression {
            inner: MACRO_GENERATE_METHOD,
            dependencies: vec![LAMBDA_GENERATE_TYPE_PRE_CONDITIONS],
            expression: "(generate-method m_navigate_to ((:task t_navigate_to)
            (:params (?r robot) (?x float) (?y float))
            (:pre-conditions (and-cond (robot.available ?r) (< ?x 10) (< ?y 10)))
            (:score 0)
            (:body
            (begin
                (navigate_to ?r ?x ?y)))))",
            expected: "(list m_navigate_to
    't_navigate_to
    '((?r robot) (?x float) (?y float))
    (lambda (?r ?x ?y)
    (do
        (do 
            (check (instance ?r robot))
            (check (float? ?x))
            (check (float? ?y)))
        (do 
            (check (robot.available ?r))
            (check (< ?x 10))
            (check (< ?y 10))))) 
    (lambda (?r ?x ?y) 0 )
    (lambda (?r ?x ?y)
        (begin
            (navigate_to ?r ?x ?y))))",
            result: "(list m_navigate_to
    't_navigate_to
    '((?r robot) (?x float) (?y float))
    (lambda (?r ?x ?y)
    (do
        (do 
            (check (instance ?r robot))
            (check (float? ?x))
            (check (float? ?y)))
        (do 
            (check (robot.available ?r))
            (check (< ?x 10))
            (check (< ?y 10))))) 
    (lambda (?r ?x ?y) 0 )
    (lambda (?r ?x ?y)
        (begin
            (navigate_to ?r ?x ?y))))",
        };

        let mut env = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, true).await
    }
}
