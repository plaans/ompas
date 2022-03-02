use crate::context::rae_env::{
    RAE_METHOD_PRE_CONDITIONS_MAP, RAE_METHOD_SCORE_MAP, RAE_METHOD_TYPES_MAP, RAE_TASK_METHODS_MAP,
};
use crate::module::rae_exec::platform::instance;
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_lisp::core::eval;
use ompas_lisp::core::root_module::get;
use ompas_lisp::core::root_module::list::cons;
use ompas_lisp::core::structs::lenv::LEnv;
use ompas_lisp::core::structs::lerror::LError::WrongNumberOfArgument;
use ompas_lisp::core::structs::lerror::LResult;
use ompas_lisp::core::structs::lvalue::LValue;
use ompas_lisp::modules::utils::enumerate;
use ompas_utils::dyn_async;
use std::convert::TryInto;

pub const MACRO_SIM_BLOCK: &str = "(defmacro sim_block (lambda (body)
`(begin
    (define state (rae-get-facts))
    (define rae-mode simu-mode)
    ,body)))";

pub const LAMBDA_GET_PRECONDITIONS: &str = "(define get-preconditions\
    (lambda (label)\
        (get rae-method-pre-conditions-map label)))";

pub const LAMBDA_GET_SCORE: &str = "(define get-score\
    (lambda (label)\
        (get rae-method-score-map label)))";

pub const GET_ACTION_MODEL: &str = "get-action-model";
pub const LAMBDA_GET_ACTION_MODEL: &str = "(define get-action-model 
    (lambda (label)
        (get rae-action-model-map label)))";

pub const LAMBDA_EVAL_PRE_CONDITIONS: &str = "(define eval-pre-conditions
    (lambda (method)
        (sim_block
            (eval (cons (get-preconditions (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_COMPUTE_SCORE: &str = "(define compute-score 
    (lambda (method)
        (sim_block
            (eval (cons (get-score (car method)) (quote-list (cdr method)))))))";

pub const LAMBDA_IS_APPLICABLE: &str = "(define applicable?
    (lambda (method)
        (sim_block
            (eval-pre-conditions method))))";

pub const LAMBDA_GENERATE_APPLICABLE_INSTANCES: &str = "(define generate_applicable_instances
    (lambda (task)
        (let* ((task_label (first task))
               (params (cdr task))
               (methods (get rae-task-methods-map task_label)))
            (r_generate_instances 
                (enr (cons enumerate (cons methods params)))))))";

pub const LAMBDA_R_GENERATE_INSTANCES: &str = "(define r_generate_instances
    (lambda (methods)
        (if (null? methods)
            nil
            (let* ((method (car methods))
                    (methods (cdr methods))
                    (method_label (first method))
                    (params (cdr method)))
                (begin
                    (define types (get rae-method-types-map method_label))
                    (if (> (len types) (len params))
                        (begin
                            (define instance_types (mapf instance (sublist types (len params))))
                            (define instances (enr (cons enumerate (append method instance_types))))
                            (append (r_test_method instances) (r_generate_instances methods)))
                        (cons
                            (if (! (err? (eval-pre-conditions method)))
                                (list method (compute-score method))
                                nil)
                            (r_generate_instances methods))))))))";

pub const LAMBDA_R_TEST_METHOD: &str = "(define r_test_method 
    (lambda (instances)
        (if (null? instances)
            nil
            (if (eval-pre-conditions (car instances))
                (cons 
                    (list (car instances) (compute-score (car instances)))
                    (r_test_method (cdr instances)))
                (r_test_method (cdr instances))))))";

//pub const EVAL_PRE_CONDITIONS: &str = "eval-pre-conditions";
//pub const COMPUTE_SCORE: &str = "compute-score";
//pub const GENERATE_APPLICABLE_INSTANCES: &str = "generate-applicable-instances";
pub const STATE: &str = "state";

/*pub const LAMBDA_CHECK_PRECONDITIONS: &str = "(define check_preconditions
(lambda (method)
    (sim_block (eval-pre-conditions method))))";*/
pub const RAE_GENERATE_APPLICABLE_INSTANCES: &str = "generate_applicable_instances";

#[macro_rules_attribute(dyn_async!)]
pub async fn generate_applicable_instances<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            RAE_GENERATE_APPLICABLE_INSTANCES,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let mut applicable_methods: Vec<LValue> = vec![];
    let task: Vec<LValue> = (&args[0]).try_into()?;

    let task_label = &task[0];
    let params: Vec<LValue> = task[1..]
        .iter()
        .map(|lv| LValue::List(vec![lv.clone()]))
        .collect();
    let methods_template: Vec<LValue> = get(
        &[
            env.get_symbol(RAE_TASK_METHODS_MAP).unwrap(),
            task_label.clone(),
        ],
        env,
    )?
    .try_into()?;

    /*println!(
        "task: {}\ntemplates: {}",
        LValue::from(task.clone()),
        LValue::from(methods_template.clone())
    );*/

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

        let instances_template: Vec<LValue> = enumerate(&instances_template, env)?.try_into()?;

        /*println!(
            "instances for template {}: {}",
            template,
            LValue::from(instances_template.clone())
        );*/

        for i in &instances_template {
            let i_vec: Vec<LValue> = i.try_into()?;
            let arg = cons(&[pre_conditions_lambda.clone(), i_vec[1..].into()], env)?;
            let lv: LValue = eval(&arg, &mut env.clone()).await?;
            if !matches!(lv, LValue::Err(_)) {
                let arg = cons(&[score_lambda.clone(), i_vec[1..].into()], env)?;
                let score: LValue = eval(&arg, &mut env.clone()).await?;
                applicable_methods.push(vec![i.clone(), score].into())
            }
        }
    }

    let applicable_methods = applicable_methods.into();

    /*println!(
        "applicable instances for {}: {}",
        LValue::from(task),
        applicable_methods
    );*/

    Ok(applicable_methods)
}
