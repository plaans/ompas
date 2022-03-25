use crate::module::rae_exec::algorithms::select::SelectMode;
use crate::module::rae_exec::error::RaeExecError;
use crate::module::rae_exec::{
    CtxRaeExec, MOD_RAE_EXEC, RAE_GET_NEXT_METHOD, RAE_SET_SUCCESS_FOR_TASK,
};
use ::macro_rules_attribute::macro_rules_attribute;
use log::info;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::core::structs::lerror::{LError, LResult};
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::core::structs::typelvalue::TypeLValue;
use ompas_lisp::modules::utils::enr;
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const RAE_PROGRESS: &str = "progress";

/*const LAMBDA_PROGRESS: &str = "
(define progress (lambda task
    (let* ((result (select task))
            (first_m (first result))
            (task_id (second result)))

            (if (null? first_m)
                (err err::no-applicable-method)
                (begin
                    (print \"trying \" first_m)
                    (define result (enr first_m))
                    (print \"tried fist method of \" task_id)
                    (if (err? result)
                        (retry task_id)
                        (rae-set-success-for-task task_id)))))))";*/

//Access part of the environment

/*const LAMBDA_GET_METHODS: &str = "\
(define get-methods\
    (lambda (label)\
        (get rae-task-methods-map label)))";*/
#[macro_rules_attribute(dyn_async !)]
pub async fn progress<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    let result: Vec<LValue> = select(args, env).await?.try_into()?;
    assert_eq!(result.len(), 2);
    let first_m = &result[0];
    let task_id = &result[1];

    let result: LValue = if first_m == &LValue::Nil {
        RaeExecError::NoApplicableMethod.into()
    } else {
        info!("Trying {} for task {}", first_m, task_id);
        let result = enr(&[first_m.clone()], env).await?;
        if matches!(result, LValue::Err(_)) {
            retry(&[task_id.clone()], env).await?
        } else {
            true.into()
        }
    };

    Ok(result)
}

/*const LAMBDA_RETRY: &str = "
(define retry (lambda (task_id)
    (let ((new_method (rae-get-next-method task_id)))
        (begin
            (print \"Retrying task \" task_id)
            (if (null? new_method) ; if there is no method applicable
            nil
            (if (enr new_method)
                (rae-set-success-for-task task_id)
                (rae-retry task_id)))))))";*/

#[macro_rules_attribute(dyn_async !)]
pub async fn retry<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    assert_eq!(args.len(), 1);
    let task_id = &args[0];
    let new_method = get_next_method(args, env).await?;
    info!("Retrying task {}", task_id);
    let result: LValue = if new_method == LValue::Nil {
        RaeExecError::NoApplicableMethod.into()
    } else {
        let result = enr(&[new_method], env).await?;
        if result == LValue::Nil {
            retry(args, env).await?
        } else {
            set_success_for_task(&[task_id.clone()], env).await?
        }
    };

    Ok(result)
}

#[macro_rules_attribute(dyn_async !)]
async fn get_next_method<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_GET_NEXT_METHOD,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if let LValue::Number(LNumber::Int(task_id)) = &args[0] {
        if task_id.is_positive() {
            let next_method = ctx
                .agenda
                .get_next_applicable_method(&(*task_id as usize))
                .await;
            Ok(next_method)
        } else {
            Err(SpecialError(
                RAE_GET_NEXT_METHOD,
                "task_id is not a positive integer".to_string(),
            ))
        }
    } else {
        Err(WrongType(
            RAE_GET_NEXT_METHOD,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Usize,
        ))
    }
}

/*const LAMBDA_SELECT: &str = "
(define select
  (lambda (task)
    (sim_block
    (rae-select task (generate_applicable_instances task)))))))";*/

const SELECT_MODE: SelectMode = SelectMode::Greedy;
pub async fn select(args: &[LValue], env: &LEnv) -> LResult {
    /*
    Each function return an ordered list of methods
     */

    let task: LValue = args.into();
    info!("Add task {} to agenda", task);
    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;
    let task_id = ctx.agenda.add_task(task.clone()).await;

    let methods: LValue = match SELECT_MODE {
        SelectMode::Greedy => {
            /*
            Returns all applicable methods sorted by their score
             */
            info!("select greedy for {}", LValue::from(args));
            select::greedy_select(args, env).await?
        }
        SelectMode::Planning => {
            /*
            Returns a unique function
             */
            todo!()
        }
        SelectMode::Heuristic => {
            /*
            Use an heuristic to sort the methods
             */
            todo!()
        }
        SelectMode::Learned => {
            /*
            Use a learning algorithms to select the best method.
             */
            todo!()
        }
    };

    if let LValue::List(methods) = methods {
        info!("sorted_methods: {}", LValue::from(&methods));
        if !methods.is_empty() {
            let mut stack = ctx.agenda.get_stack(task_id).await.unwrap();
            stack.set_current_method(methods[0].clone());
            stack.set_applicable_methods(methods[1..].into());
            ctx.agenda.update_stack(stack).await?;

            //info!("agenda: {}", ctx.agenda);

            Ok(vec![methods[0].clone(), task_id.into()].into())
        } else {
            Ok(vec![LValue::Nil, task_id.into()].into())
        }
    } else {
        Ok(vec![LValue::Nil, task_id.into()].into())
    }
}

mod select {
    use crate::context::rae_env::{
        RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP,
        RAE_TASK_METHODS_MAP,
    };
    use crate::module::rae_exec::platform::instance;
    use crate::module::rae_exec::{get_facts, STATE};
    use ::macro_rules_attribute::macro_rules_attribute;
    use ompas_lisp::core::eval;
    use ompas_lisp::core::root_module::get;
    use ompas_lisp::core::root_module::list::cons;
    use ompas_lisp::core::structs::lenv::LEnv;
    use ompas_lisp::core::structs::lerror::LResult;
    use ompas_lisp::core::structs::lvalue::LValue;
    use ompas_lisp::modules::utils::enumerate;
    use ompas_utils::dyn_async;
    use std::convert::TryInto;

    //pub const GREEDY_SELECT: &str = "greedy_select";
    pub enum SelectMode {
        Greedy,
        Planning,
        Heuristic,
        Learned,
    }
    //Returns the method to do.
    #[macro_rules_attribute(dyn_async !)]
    pub async fn planning_select<'a>(_: &'a [LValue], _: &'a LEnv) -> LResult {
        todo!()
    }

    #[macro_rules_attribute(dyn_async !)]
    pub async fn greedy_select<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
        /*
        Steps:
        - Create a new entry in the agenda
        - Generate all instances of applicable methods
        - Select the best method
        - Store the stack
        - Return (best_method, task_id)
         */
        let task_label = &args[0];
        let params: Vec<LValue> = args[1..]
            .iter()
            .map(|lv| LValue::List(vec![lv.clone()]))
            .collect();

        let mut applicable_methods: Vec<(LValue, i32)> = vec![];
        let state: LValue = get_facts(&[], env).await?;

        let mut env = env.clone();

        env.insert(STATE, state);
        let env = &env;

        let methods_template: Vec<LValue> = get(
            &[
                env.get_symbol(RAE_TASK_METHODS_MAP).unwrap(),
                task_label.clone(),
            ],
            env,
        )?
        .try_into()?;

        for template in methods_template {
            let types: Vec<LValue> = get(
                &[
                    env.get_symbol(RAE_METHOD_TYPES_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?
            .try_into()?;

            let score_lambda = get(
                &[
                    env.get_symbol(RAE_METHOD_SCORE_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?;

            let pre_conditions_lambda = get(
                &[
                    env.get_symbol(RAE_METHOD_PRE_CONDITIONS_MAP).unwrap(),
                    template.clone(),
                ],
                env,
            )?;

            let mut instances_template = vec![template.clone()];
            instances_template.append(&mut params.clone());

            for t in &types[params.len()..] {
                instances_template.push(instance(&[t.clone()], env).await?);
            }

            let mut instances_template: Vec<LValue> =
                enumerate(&instances_template, env)?.try_into()?;

            println!(
                "instances for template {}: {}",
                template,
                LValue::from(instances_template.clone())
            );

            let iter = instances_template.drain(..);

            for i in iter {
                let i_vec: Vec<LValue> = (&i).try_into()?;
                let arg = cons(&[pre_conditions_lambda.clone(), i_vec[1..].into()], env)?;
                let lv: LValue = eval(&arg, &mut env.clone()).await?;
                if !matches!(lv, LValue::Err(_)) {
                    let arg = cons(&[score_lambda.clone(), i_vec[1..].into()], env)?;
                    let score: i32 = eval(&arg, &mut env.clone()).await?.try_into()?;
                    applicable_methods.push((i, score))
                }
            }
        }

        /*
        Sort the list
         */

        applicable_methods.sort_by(|a, b| a.1.cmp(&b.1));
        let methods: Vec<_> = applicable_methods.drain(..).map(|a| a.0).collect();

        Ok(methods.into())
    }
}

#[macro_rules_attribute(dyn_async!)]
async fn set_success_for_task<'a>(args: &'a [LValue], env: &'a LEnv) -> Result<LValue, LError> {
    /*
    Steps:
    - Remove the stack from the agenda
    - Return true
     */

    let ctx = env.get_context::<CtxRaeExec>(MOD_RAE_EXEC)?;

    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_SET_SUCCESS_FOR_TASK,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::Number(LNumber::Int(task_id)) = &args[0] {
        if task_id.is_positive() {
            ctx.agenda.remove_task(&(*task_id as usize)).await?;
            Ok(LValue::True)
        } else {
            Err(SpecialError(
                RAE_SET_SUCCESS_FOR_TASK,
                "index is not a positive integer".to_string(),
            ))
        }
    } else {
        Err(WrongType(
            RAE_SET_SUCCESS_FOR_TASK,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Usize,
        ))
    }
}

#[allow(non_snake_case, dead_code)]
pub fn RAEPlan(_: LValue) {}
