use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use std::convert::TryInto;
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_RAE_DESCRIPTION: &str = "rae-description";

pub const GENERATE_TASK_SIMPLE: &str = "generate-task-simple";
pub const GENERATE_STATE_FUNCTION: &str = "generate-state-function";
pub const GENERATE_TASK: &str = "generate-task";
pub const GENERATE_METHOD: &str = "generate-method";

/// Macro used to generate code to define a state function in RAE environment.
pub const MACRO_GENERATE_STATE_FUNCTION: &str = "(defmacro generate-state-function (lambda args
    (let ((label (car args))
          (params (cdr args)))
        `(list ,label
            (lambda ,params
                ,(cons 'rae-get-state-variable (cons `(quote ,label) params)))
            (lambda ,params
                (get-map state ,(cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a task in RAE environment.

pub const MACRO_GENERATE_TASK: &str = "(defmacro generate-task \
                                        (lambda (l body) \
                                            (quasiquote (list (unquote l) (lambda (unquote (cdar body)) \
                                                (if (unquote (cadadr body)) \
                                                    (unquote (cadaddr body)) \
                                                    (quote (task is not applicable in the given state))))))))";

/// Macro used to generate code to define a task in the simplified representation in RAE environment.
pub const MACRO_GENERATE_TASK_SIMPLE: &str = "(defmacro generate-task-simple 
    (lambda args
    (let ((label (car args))
          (params (cdr args)))
         `(list ,label (lambda ,params
                    ,(cons 'progress (cons `(quote ,label) params)))))))";

/// Macro used to generate code to define a method in REA environment.
pub const MACRO_GENERATE_METHOD_DEPRECATED: &str = "(defmacro generate-method \
                                          (lambda (l body) \
                                            (let ((task-label (cadar body)) \
                                                  (params (cdadr body)) \
                                                  (body (cadaddr body))) \
                                                 (quasiquote (list (unquote l) \
                                                                    (quote (unquote task-label)) \
                                                                    (lambda (unquote params) \
                                                                            (unquote body)))))))";

pub const MACRO_GENERATE_METHOD: &str=
    "(defmacro generate-method;-final-till-there-is-a-new-one
        (lambda (method-label m-def)
        (let* ((task-label (cadr (get-list m-def 0)))
                (params (cdr (get-list m-def 1)))
                (pre-conditions (cadr (get-list m-def 2)))
                (effects (cadr (get-list m-def 3)))
                (body (cadr (get-list m-def 6)))
                (parameter-generator (cdr (get-list m-def 4)))
                (list-element (car parameter-generator))
                (body-generator (cadr parameter-generator))
                (score-generator (cadr (get-list m-def 5))))
                `(list ,method-label
                        ;label of the task
                        (quote ,task-label)
                        ;body of the method
                        (lambda ,params ,pre-conditions)
                        (lambda ,params ,effects)
                        ;lambda to generate instances
                        (lambda args
                            (begin
                                (define eval_params
                                    (lambda args
                                        (let ((params (car args)))
                                            (if (null? params)
                                                nil
                                                (if (eval (cons (lambda ,params ,body-generator) params))
                                                    (cons (list (cons (quote ,method-label) params) (eval (cons (lambda ,params ,score-generator) params)))
                                                        (eval_params (cdr args)))
                                                    (eval_params (cdr args)))))))
                                (eval_params (eval (cons enumerate (append args (quote ,list-element)))))))
                        (lambda ,params ,body)))))";

/// Macro used to generate code to define an action in RAE environment.
pub const MACRO_GENERATE_ACTION: &str = "(defmacro generate-action
    (lambda args
        (let ((label (car args))
              (params (cdr args)))
             `(list ,label
                 (lambda ,params ,(cons 'rae-exec-command
                     (cons `(quote ,label) params)))))))";

pub const GENERATE_ACTION_MODEL: &str = "generate-action-model";
pub const MACRO_GENERATE_ACTION_MODEL: &str = "
(defmacro generate-action-model
    (lambda (label def)
        (let ((params (cdar def))
               (conds (cadr (get def 1)))
               (effs (cadr (get def 2))))
              `(list ,label (lambda ,params
                    (begin
                        (if ,conds
                            ,effs)
                        state))))))";
pub const GENERATE_ACTION_OPERATIONAL_MODEL: &str = "generate-action-operational-model";
pub const MACRO_GENERATE_OPERATIONAL_MODEL: &str = "(defmacro generate-action-operational-model
    (lambda (label def)
        (let ((params (cdar def))
              (body (cadr (get def 1))))
              `(list ,label (lambda ,params
                    (begin
                        ,body
                        state))))))";

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

pub const LABEL_GENERATE_METHOD_PARAMETERS: &str = "generate-method-parameters";

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

pub const LAMBDA_GENERATE_INSTANCES: &str = "
(define generate-instances (lambda args
    (let* ((label (car args))
            (i_params (cdr args))
            (methods (get-methods label)))

            (begin
                (define __generate__
                    (lambda (methods)
                        (if (null? methods)
                            nil
                            (append
                                (eval
                                    (append (list (get-method-generator (car methods)))
                                        i_params))
                                (__generate__ (cdr methods))))))
                (__generate__ methods)))))";

const GENERATE_TYPE_TEST_EXPR: &str = "generate-type-test-expr";

const LAMBDA_GENERATE_TYPE_PRE_CONDITIONS: &str =
    "(define gtpc (lambda (l) (eval (parse (generate-type-test-expr l)))))";

#[derive(Default)]
pub struct CtxRaeDescription {}

impl GetModule for CtxRaeDescription {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(()),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_GENERATE_ACTION,
                MACRO_GENERATE_ACTION_MODEL,
                MACRO_GENERATE_OPERATIONAL_MODEL,
                MACRO_GENERATE_METHOD,
                MACRO_GENERATE_TASK,
                MACRO_GENERATE_STATE_FUNCTION,
                MACRO_GENERATE_TASK_SIMPLE,
                MACRO_GENERATE_METHOD_PARAMETERS,
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
        if params.len() == 2 {
            let list_params: Vec<LValue> = params[0]
                .clone()
                .try_into()
                .expect("error converting what is supposed to be a list");
            let list_types: Vec<LValue> = params[1]
                .clone()
                .try_into()
                .expect("error converting what is supposed to be a list");
            let mut str = "(and ".to_string();

            for (p, t) in list_params.iter().zip(list_types) {
                str.push_str(format!("(!= ({}.instance {}) nil)", t, p).as_str())
            }
            str.push(')');

            Ok(str.into())
        } else {
            Err(WrongNumberOfArgument(
                GENERATE_TYPE_TEST_EXPR,
                params.into(),
                params.len(),
                2..2,
            ))
        }
    } else {
        Err(WrongType(
            GENERATE_TYPE_TEST_EXPR,
            args[0].clone(),
            (&args[0]).into(),
            NameTypeLValue::List,
        ))
    }
}
