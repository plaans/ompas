use ompas_lisp::core::LEnv;
use ompas_lisp::language::scheme_primitives::*;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::convert::TryInto;
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_RAE_DESCRIPTION: &str = "rae-description";

pub const GENERATE_TASK: &str = "generate-task";
pub const GENERATE_TASK_SIMPLE: &str = "generate-task-simple";
pub const GENERATE_STATE_FUNCTION: &str = "generate-state-function";
pub const GENERATE_ACTION: &str = "generate-action";
pub const GENERATE_ACTION_MODEL: &str = "generate-action-model";
pub const GENERATE_ACTION_OPERATIONAL_MODEL: &str = "generate-action-operational-model";
pub const GENERATE_METHOD: &str = "generate-method";

/// Macro used to generate code to define a task in RAE environment.
/*pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task \
(lambda (l body) \
    (quasiquote (list (unquote l) (lambda (unquote (cdar body)) \
        (if (unquote (cadadr body)) \
            (unquote (cadaddr body)) \
            (quote (task is not applicable in the given state))))))))";*/

/// Macro used to generate code to define a task in the simplified representation in RAE environment.
pub const MACRO_GENERATE_TASK_SIMPLE: &str = "(defmacro generate-task-simple 
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
                (if (= rae-mode exec-mode)
                    ,(cons 'rae-get-state-variable (cons `(quote ,label) params))
                    ,(if (= params nil)
                        `(get-map state ',label)
                        `(get-map state ,(cons 'list (cons `(quote ,label) params))))))))))";

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
                    (if (= rae-mode exec-mode)
                        (begin
                            (if (rae-platform?)
                                ,(cons 'rae-exec-command
                                    (cons `(quote ,label) params))
                                ,(cons `(get-action-model (quote ,label)) params))
                            (print \"action \" (quote ,label) params \"executed !\"))
                        ,(cons `(get-action-model (quote ,label)) params)))))))";

pub const MACRO_GENERATE_ACTION_MODEL: &str = "
(defmacro generate-action-model
    (lambda (label def)
        (let* ((p_expr (cdar def))
               (p_unzip (unzip p_expr))
               (params (car p_unzip))
               (conds (cadr (get def 1)))
               (effs (cadr (get def 2))))
              `(list ,label (lambda ,params
                    (if ,(gtpc p_expr)
                        (if ,conds
                            ,effs))
                    )))))";
pub const MACRO_GENERATE_ACTION_OPERATIONAL_MODEL: &str =
    "(defmacro generate-action-operational-model
    (lambda (label def)
        (let* ((p_expr (cdar def))
               (body (cadr (get def 1)))
               (p_unzip (unzip p_expr))
               (params (car p_unzip)))

              `(list ,label (lambda ,params
                    (if ,(gtpc p_expr)
                        ,body)
                    )))))";

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
                    (if ,(gtpc p_expr)
                        (if ,conds true)))
                (lambda ,params ,score)
                (lambda ,params ,body))))))";

/// Macro used to generate code to define a method in RAE environment.
pub const MACRO_GENERATE_METHOD_PARAMETERS: &str =
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
                (eval_params (unquote (cons enumerate p_enum))))))))";

const GENERATE_TYPE_TEST_EXPR: &str = "generate-type-test-expr";

const LAMBDA_GENERATE_TYPE_PRE_CONDITIONS: &str =
    "(define gtpc (lambda (l) (parse (generate-type-test-expr l))))";

#[derive(Default)]
pub struct CtxRaeDescription {}

impl GetModule for CtxRaeDescription {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(()),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_GENERATE_TASK_SIMPLE,
                MACRO_GENERATE_STATE_FUNCTION,
                MACRO_GENERATE_ACTION,
                MACRO_GENERATE_ACTION_MODEL,
                MACRO_GENERATE_ACTION_OPERATIONAL_MODEL,
                MACRO_GENERATE_METHOD,
                MACRO_ENUMERATE_PARAMS,
                LAMBDA_GENERATE_TYPE_PRE_CONDITIONS,
            ]
            .into(),
            label: MOD_RAE_DESCRIPTION.to_string(),
        };

        module.add_fn_prelude(GENERATE_TYPE_TEST_EXPR, generate_type_test_expr);
        module
    }
}

/// Takes as input a p_expr of the form ((p1 p1_type) ... (p_n pn_type))
pub fn generate_type_test_expr(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
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
                                NameTypeLValue::Symbol,
                            ));
                        }
                    } else {
                        return Err(WrongType(
                            GENERATE_TYPE_TEST_EXPR,
                            param[0].clone(),
                            (&param[0]).into(),
                            NameTypeLValue::Symbol,
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
                    NameTypeLValue::List,
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
            NameTypeLValue::List,
        ))
    }
}
/// TODO: Test des macros
#[cfg(test)]
mod test {
    use crate::rae::module::mod_rae_description::*;
    use crate::rae::module::rae_exec::CtxRaeExec;
    use ompas_lisp::core::ImportType::WithoutPrefix;
    use ompas_lisp::core::{activate_debug, import, ContextCollection, LEnv};
    use ompas_lisp::modules::io::CtxIo;
    use ompas_lisp::modules::math::CtxMath;
    use ompas_lisp::modules::utils::CtxUtils;
    use ompas_lisp::structs::LError;
    use ompas_lisp::test_utils::{test_expression, test_expression_with_env, TestExpression};

    async fn init_env_and_ctxs() -> (LEnv, ContextCollection) {
        let (mut env, mut ctxs) = LEnv::root().await;

        import(&mut env, &mut ctxs, CtxUtils::default(), WithoutPrefix)
            .await
            .expect("error loading utils");

        import(&mut env, &mut ctxs, CtxMath::default(), WithoutPrefix)
            .await
            .expect("error loading math");

        import(&mut env, &mut ctxs, CtxRaeExec::default(), WithoutPrefix)
            .await
            .expect("error loading rae exec");

        import(
            &mut env,
            &mut ctxs,
            CtxRaeDescription::default(),
            WithoutPrefix,
        )
        .await
        .expect("error loading rae description");

        import(&mut env, &mut ctxs, CtxIo::default(), WithoutPrefix)
            .await
            .expect("error loading io");

        (env, ctxs)
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await?;

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
        test_expression_with_env(macro_to_test_2, &mut env, &mut ctxs, true).await
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await
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
        let (mut env, mut ctxs) = init_env_and_ctxs().await;

        test_expression_with_env(lambda_test, &mut env, &mut ctxs, false).await
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

        let (mut env, mut ctxs) = init_env_and_ctxs().await;
        test_expression_with_env(macro_to_test, &mut env, &mut ctxs, true).await
    }
}
