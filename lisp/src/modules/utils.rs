//! Scheme module to load in the environment to add the following functions:
//! - rand-element: returns a random element of a list
//! # Example
//! ``` lisp
//! (rand-element (list 1 2 3 4))
//! => 1
//! (rand-element (list 1 2 3 4))
//! => 3
//! ```
//! - enumerate: returns a list of all combinations of elements of several lists
//!# Example:
//! ``` lisp
//! (enumerate (list 1 2) (list 3 4))
//! => ((1 3)(1 4)(2 3)(2 4))
//! ```

use crate::core::LEnv;
use crate::modules::doc::{Documentation, LHelp};
use crate::structs::LError::{WrongNumberOfArgument, WrongType};
use crate::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use aries_utils::StreamingIterator;
use rand::Rng;
use std::ops::Deref;
use std::sync::Arc;

//LANGUAGE

const MOD_UTILS: &str = "utils";
const RAND_ELEMENT: &str = "rand-element";
const ENUMERATE: &str = "enumerate";
const CONTAINS: &str = "contains";
const TRANSFORM_IN_SINGLETON_LIST: &str = "transform-in-singleton-list";

// Documentation
const DOC_RAND_ELEMENT: &str = "Return a random element of a list";
const DOC_RAND_ELEMENT_VERBOSE: &str = "Example: \n(rand-element (list 1 2 3 4))\n=> 1";
const DOC_ENUMERATE: &str =
    "Return a enumeration of all possible combinations of elements of 1+ lists";
const DOC_ENUMERATE_VERBOSE: &str =
    "Example: \n(enumerate (list 1 2) (list 3 4))\n=> ((1 3)(1 4)(2 3)(2 4))";
//MACROS

pub const MACRO_TEST_MACRO: &str = "(defmacro test-macro
   (lambda (x)
    `(expand (parse ,x))))";

pub const MACRO_AND: &str = "(defmacro and
                                (lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (length args) 1)
                                            (car args)
                                            `(if ,(car args)
                                                 ,(cons 'and (cdr args))
                                                 nil)))))";

pub const MACRO_OR: &str = "(defmacro or 
                                (lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (length args) 1)
                                            (car args)
                                            `(if ,(car args)
                                                  true
                                                  ,(cons 'or (cdr args)))))))";

pub const MACRO_CAAR: &str = "(defmacro caar (lambda (x) `(car (car ,x))))";
pub const MACRO_CADR: &str = "(defmacro cadr (lambda (x) `(car (cdr ,x))))";
pub const MACRO_CDAR: &str = "(defmacro cdar (lambda (x) `(cdr (car ,x))))";
pub const MACRO_CDDR: &str = "(defmacro cddr (lambda (x) `(cdr (cdr ,x))))";

pub const MACRO_CAADR: &str = "(defmacro caadr (lambda (x) `(car (car (cdr ,x)))))";
pub const MACRO_CADDR: &str = "(defmacro caddr (lambda (x) `(car (cdr (cdr ,x)))))";
pub const MACRO_CADAR: &str = "(defmacro cadar (lambda (x) `(car (cdr (car ,x)))))";
pub const MACRO_CADADR: &str = "(defmacro cadadr (lambda (x) `(car (cdr (car (cdr ,x))))))";
pub const MACRO_CDADR: &str = "(defmacro cdadr (lambda (x) `(cdr (car (cdr ,x)))))";
pub const MACRO_CADADDR: &str = "(defmacro cadaddr (lambda (x) `(car (cdr (car (cdr (cdr ,x)))))))";

pub const MACRO_NEQ: &str = "(defmacro neq
                                (lambda (a b)
                                    `(! (= ,a ,b))))";

pub const MACRO_NEQ_SHORT: &str = "(defmacro !=
                                        (lambda (a b)
                                            `(neq ,a ,b)))";

pub const MACRO_AWAIT_ASYNC: &str = "(defmacro await-async (lambda (x) `(await (async ,x))))";

pub const MACRO_APPLY: &str = "(defmacro apply
                                  (lambda (f args)
                                          (cons f args)))";

pub const MACRO_COND: &str = "(defmacro cond (lambda exprs
    (if (null? exprs)
        nil
        (if (= (caar exprs) 'else)
            (cadar exprs)
            `(if ,(caar exprs)
                ,(cadar exprs)
                ,(cons cond (cdr exprs)))))))";

pub const MACRO_FOR: &str = "(defmacro for (lambda args \
(let ((_i_ (get-list args 0)) \
        (_list_ (get args 2)) \
        (_body_ (get args 3))) \
    `(let ((_f_loop_ (lambda args \
        (if (null? args) \
            nil \
            (let ((,_i_ (car args))) \
                (begin \
                    ,_body_ \
                    (_f_loop_ (cdr args)))))))) \
        (_f_loop_ ,_list_)))))";

pub const MACRO_WHILE: &str = "(defmacro while
    (lambda (c b)
        `(begin
            (define __loop__
                (lambda nil
                    (if ,c
                        (begin
                            ,b
                            (__loop__))
                    nil)))
            (__loop__))))";

pub const MACRO_LOOP: &str = "(defmacro loop 
    (lambda (b)
        `(begin 
            (define __loop__
                (lambda nil 
                    (begin 
                        ,b
                        (__loop__))))
            (__loop__))))";

pub const MACRO_LET: &str = "(defmacro let
    (lambda (bindings body)
        (begin
            (define unzipped (unzip bindings))
            (define keys (car unzipped))
            (define values (cadr unzipped))
            (cons `(lambda ,keys
                        ,body)
                    values))))";

pub const MACRO_LET_STAR: &str = "(defmacro let*
    (lambda (bindings body)
        (if (= (length bindings) 1)
            (cons `(lambda ,(list (caar bindings))
                           ,body)
                    (cdar bindings))
            (cons `(lambda ,(list (caar bindings))
                            (let* ,(cdr bindings) ,body))
                        (cdar bindings)))))";

pub const LAMBDA_COMBINE:  &str = "(define combine (lambda (f)
                                                                (lambda (x y)
                                                                        (if (null? x) (quote ())
                                                                            (f (list (car x) (car y))
                                                                            ((combine f) (cdr x) (cdr y)))))))";
pub const LAMBDA_ZIP: &str = " (define zip (lambda (l1 l2)\
                                                        (if (or (null? l1)\
                                                                (null? l2))\
                                                             nil\
                                                             (cons (list (car l1)\
                                                                         (car l2))\
                                                                   (zip (cdr l1)\
                                                                        (cdr l2)))))))";

pub const LAMBDA_UNZIP: &str = "(define unzip
        (lambda (lists)
                (begin
                    (define firsts 
                        (lambda (lists)
                            (if (null? lists)
                                nil
                                (cons (caar lists)
                                        (firsts (cdr lists))))))
                    (define seconds 
                        (lambda (lists)
                            (if (null? lists)
                            nil
                            (cons (cadar lists)
                                    (seconds (cdr lists))))))
                    (list (firsts lists) (seconds lists)))))";

pub const LAMBDA_MAPF: &str = "(define mapf 
    (lambda (f seq)
         (if (null? seq)
         nil
         (cons (eval (cons f (car seq))) (mapf f (cdr seq))))))";

pub const LAMBDA_ARBITRARY: &str = "(define arbitrary
    (lambda args
        (cond ((= (length args) 1) ; default case
               (car (first args)))
              ((= (length args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases";

#[derive(Default, Copy, Clone, Debug)]
pub struct CtxUtils {}

impl GetModule for CtxUtils {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(()),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_TEST_MACRO,
                MACRO_AND,
                MACRO_OR,
                MACRO_CAAR,
                MACRO_CADR,
                MACRO_CDAR,
                MACRO_CDDR,
                MACRO_CADAR,
                MACRO_CADDR,
                MACRO_CDADR,
                MACRO_CAADR,
                MACRO_CADADR,
                MACRO_CADADDR,
                MACRO_NEQ,
                MACRO_NEQ_SHORT,
                MACRO_AWAIT_ASYNC,
                MACRO_APPLY,
                MACRO_COND,
                //MACRO_WHILE,
                MACRO_LOOP,
                LAMBDA_UNZIP,
                LAMBDA_ZIP,
                LAMBDA_MAPF,
                MACRO_LET,
                MACRO_LET_STAR,
                //MACRO_FOR,
                LAMBDA_ARBITRARY,
            ]
            .into(),
            label: MOD_UTILS.into(),
        };

        module.add_fn_prelude(RAND_ELEMENT, rand_element);
        module.add_fn_prelude(ENUMERATE, enumerate);
        module.add_fn_prelude(CONTAINS, contains);
        module.add_fn_prelude(TRANSFORM_IN_SINGLETON_LIST, transform_in_singleton_list);

        module
    }
}

impl Documentation for CtxUtils {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new_verbose(RAND_ELEMENT, DOC_RAND_ELEMENT, DOC_RAND_ELEMENT_VERBOSE),
            LHelp::new_verbose(ENUMERATE, DOC_ENUMERATE, DOC_ENUMERATE_VERBOSE),
        ]
    }
}

///Return enumeration from a list of list
///uses function from aries_utils
/// # Example:
///``` rust
/// use ompas_lisp::modules::utils::{CtxUtils, enumerate};
/// use ompas_lisp::structs::LValue;
/// use ompas_lisp::core::LEnv;
/// let lists: &[LValue] = &[vec![1,2,3].into(), vec![4,5,6].into()];
/// let enumeration = enumerate(lists, &LEnv::default(), &());
/// ```
pub fn enumerate(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    let mut vec_iter = vec![];
    let mut new_args = vec![];

    for arg in args {
        if let LValue::List(_) = arg {
            new_args.push(arg.clone())
        } else {
            new_args.push(LValue::List(vec![arg.clone()]))
        }
    }

    for arg in &new_args {
        if let LValue::List(iter) = arg {
            vec_iter.push(iter.iter())
        }
    }

    let mut iter_params = aries_utils::enumerate(vec_iter);

    let mut vec_result: Vec<LValue> = vec![];

    while let Some(val) = iter_params.next() {
        let mut enumeration = vec![];
        for val in val {
            enumeration.push(val.deref().clone())
        }
        vec_result.push(enumeration.into())
    }

    Ok(vec_result.into())
}

///Return an element randomly chosen from a list
/// Takes a LValue::List as arg.
pub fn rand_element(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    match args.len() {
        1 => {
            if let LValue::List(list) = &args[0] {
                let index = rand::thread_rng().gen_range(0..list.len());
                Ok(list[index].clone())
            } else {
                Err(WrongType(
                    RAND_ELEMENT,
                    args[0].clone(),
                    (&args[0]).into(),
                    NameTypeLValue::Symbol,
                ))
            }
        }
        _ => Err(WrongNumberOfArgument(
            RAND_ELEMENT,
            args.into(),
            args.len(),
            1..1,
        )),
    }
}

///Takes a list or map and search if it contains a LValue inside
pub fn contains(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.len() != 2 {
        return Err(WrongNumberOfArgument(
            CONTAINS,
            args.into(),
            args.len(),
            2..2,
        ));
    }

    if let LValue::List(vec) = &args[0] {
        for e in vec {
            if e == &args[1] {
                return Ok(LValue::True);
            }
        }
    } else if let LValue::Map(m) = &args[0] {
        for e in m.keys() {
            if e == &args[1] {
                return Ok(LValue::True);
            }
        }
    }
    Ok(LValue::Nil)
}

pub fn transform_in_singleton_list(args: &[LValue], _: &LEnv, _: &()) -> Result<LValue, LError> {
    if args.is_empty() {
        return Err(WrongNumberOfArgument(
            TRANSFORM_IN_SINGLETON_LIST,
            args.into(),
            0,
            1..std::usize::MAX,
        ));
    }

    Ok(args
        .iter()
        .map(|lv| LValue::List(vec![lv.clone()]))
        .collect::<Vec<LValue>>()
        .into())
}

#[cfg(test)]
mod test {
    use crate::core::{parse, LEnv};
    use crate::modules::utils::*;
    use crate::structs::LError;
    use crate::test_utils::{test_expression, TestExpression};

    #[tokio::test]
    async fn test_macro_test_macro() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_TEST_MACRO,
            dependencies: vec![],
            expression: "(test-macro \"(if (= 1 2) true nil)\")",
            expanded: "(expand (parse \"(if (= 1 2) true nil)\"))",
            result: "(if (= 1 2) true nil)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_and() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_AND,
            dependencies: vec![],
            expression: "(and (= (+ 1 1) 2) (< 3 4))",
            expanded: "(if (= (+ 1 1) 2) (< 3 4) nil)",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_or() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_OR,
            dependencies: vec![],
            expression: "(or (= (+ 1 1) 2) (< 3 4))",
            expanded: "(if (= (+ 1 1) 2) true (< 3 4))",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caar() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CAAR,
            dependencies: vec![],
            expression: "(caar '((1 2) (3 4)))",
            expanded: "(car (car (quote ((1 2) (3 4)))))",
            result: "1",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADR,
            dependencies: vec![],
            expression: "(cadr '((1 2) (3 4)))",
            expanded: "(car (cdr (quote ((1 2) (3 4)))))",
            result: "(3 4)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cdar() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDAR,
            dependencies: vec![],
            expression: "(cdar '((1 2) (3 4)))",
            expanded: "(cdr (car (quote ((1 2) (3 4)))))",
            result: "(2)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cddr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDDR,
            dependencies: vec![],
            expression: "(cddr '((1 2) (3 4) 5))",
            expanded: "(cdr (cdr (quote ((1 2) (3 4) 5))))",
            result: "(5)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadar() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADAR,
            dependencies: vec![],
            expression: "(cadar '((1 2) (3 4) 5))",
            expanded: "(car (cdr (car '((1 2) (3 4) 5))))",
            result: "2",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caddr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADDR,
            dependencies: vec![],
            expression: "(caddr '((1 2) (3 4) 5))",
            expanded: "(car (cdr (cdr '((1 2) (3 4) 5))))",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cdadr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDADR,
            dependencies: vec![],
            expression: "(cdadr '((1 2) (3 4) 5))",
            expanded: "(cdr (car (cdr '((1 2) (3 4) 5))))",
            result: "(4)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caadr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CAADR,
            dependencies: vec![],
            expression: "(caadr '((1 2) (3 4) 5))",
            expanded: "(car (car (cdr '((1 2) (3 4) 5))))",
            result: "3",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadadr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADADR,
            dependencies: vec![],
            expression: "(cadadr '((1 2) (3 4) 5))",
            expanded: "(car (cdr (car (cdr '((1 2) (3 4) 5)))))",
            result: "4",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadaddr() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADADDR,
            dependencies: vec![],
            expression: "(cadaddr '((1 2) (3 4) ((5 6) 7)))",
            expanded: "(car (cdr (car (cdr (cdr '((1 2) (3 4) ((5 6) 7)))))))",
            result: "7",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_neq() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_NEQ,
            dependencies: vec![],
            expression: "(neq 1 4)",
            expanded: "(! (= 1 4))",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_neq_short() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_NEQ_SHORT,
            dependencies: vec![MACRO_NEQ],
            expression: "(!= 1 4)",
            expanded: "(! (= 1 4))",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_await_async() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_AWAIT_ASYNC,
            dependencies: vec![],
            expression: "(await-async (+ 1 4))",
            expanded: "(await (async (+ 1 4)))",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_apply() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_APPLY,
            dependencies: vec![],
            expression: "(apply + (1 4))",
            expanded: "(+ 1 4)",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cond() -> Result<(), LError> {
        let macro_to_test = TestExpression {
            inner: MACRO_COND,
            dependencies: vec![MACRO_CAAR, MACRO_CADAR],
            expression: "(begin
                    (define weather (lambda (t) 
                        (cond ((< t 0) cold)
                            ((< t 10) cool)
                            ((< t 20) ok)
                            ((< t 30) warm)
                            (else hot))))

                    (weather 15))",
            expanded: "(begin
                    (define weather (lambda (t) (if (< t 0)
                        cold
                        (if (< t 10)
                            cool
                            (if (< t 20)
                                ok
                                (if (< t 30)
                                    warm
                                    hot))))))
                    (weather 15))",
            result: "ok",
        };

        test_expression(macro_to_test).await
    }
    #[tokio::test]
    async fn test_macro_loop() -> Result<(), LError> {
        let expression = "(loop (+ 1 1))";
        let expected = "(begin
            (define __loop__
                (lambda nil
                    (begin
                        (+ 1 1)
                        (__loop__))))
            (__loop__))))";

        let (mut env, mut ctxs) = LEnv::root().await;

        //Load macro
        parse(MACRO_LOOP, &mut env, &mut ctxs).await?;

        //Expand expression
        let expanded = parse(expression, &mut env, &mut ctxs).await?;

        let expected = parse(expected, &mut env, &mut ctxs).await?;

        println!(
            "test_macro:\n\
        \t-expression: {}\n\
        \t-extended: {}\n\
        \t-expected: {}",
            expression, expanded, expected
        );

        assert_eq!(expanded, expected);

        Ok(())
    }

    #[tokio::test]
    async fn test_lambda_zip() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: LAMBDA_ZIP,
            dependencies: vec![MACRO_OR],
            expression: "(zip '(1 2 3 4) '(5 6 7 8))",
            expanded: "(zip '(1 2 3 4) '(5 6 7 8))",
            result: "((1 5) (2 6) (3 7) (4 8))",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_lambda_unzip() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: LAMBDA_UNZIP,
            dependencies: vec![MACRO_CAAR, MACRO_CADAR, MACRO_APPLY],
            expression: "(unzip '((1 5) (2 6) (3 7) (4 8)))",
            expanded: "(unzip '((1 5) (2 6) (3 7) (4 8)))",
            result: "((1 2 3 4) (5 6 7 8))",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_lambda_mapf() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: LAMBDA_MAPF,
            dependencies: vec![],
            expression: "(begin
                            (define square (lambda (x)  (* x x)))
                            (mapf square '(1 2 3 4 5)))",
            expanded: "(begin
                            (define square (lambda (x)  (* x x)))
                            (mapf square '(1 2 3 4 5)))",
            result: "(1 4 9 16 25)",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_macro_let() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: MACRO_LET,
            dependencies: vec![MACRO_CAAR, MACRO_CADAR, MACRO_CADR, LAMBDA_UNZIP],
            expression: "(let ((x 1)
                               (y 2))
                              (+ x y))",
            expanded: "((lambda (x y) (+ x y)) 1 2)",
            result: "3",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_macro_let_star() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: MACRO_LET_STAR,
            dependencies: vec![MACRO_CAAR, MACRO_CDAR],
            expression: "(let* ((x 1)\
                               (y (+ x 1)))\
                              (+ x y))",
            expanded: "((lambda (x) ((lambda (y) (+ x y)) (+ x 1))) 1)",
            result: "3",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_lambda_arbitrary() -> Result<(), LError> {
        let test_lambda = TestExpression {
            inner: LAMBDA_ARBITRARY,
            dependencies: vec![
                MACRO_CAAR,
                MACRO_CADAR,
                MACRO_CADR,
                LAMBDA_UNZIP,
                MACRO_LET,
                MACRO_COND,
            ],
            expression: "(arbitrary '(1 2 3))",
            expanded: "(arbitrary '(1 2 3))",
            result: "1",
        };

        let test_lambda_2 = TestExpression {
            inner: LAMBDA_ARBITRARY,
            dependencies: vec![
                MACRO_CAAR,
                MACRO_CADAR,
                MACRO_CADR,
                LAMBDA_UNZIP,
                MACRO_LET,
                MACRO_COND,
            ],
            expression: "(arbitrary '(1 2 3) second)",
            expanded: "(arbitrary '(1 2 3) second)",
            result: "2",
        };

        test_expression(test_lambda).await?;
        test_expression(test_lambda_2).await
    }
}

//LAMBDAS
/*pub const LAMBDA_AND: &str = "(define and (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) (and (cdr args)) nil)))))";
pub const LAMBDA_OR: &str = "(define or (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) true (or (cdr args)))))))";*/

/*pub const LAMBDA_COND: &str = "(define cond (lambda x \
(if (null? x)\
    nil\
    (let ((a (car x))\
           (tests (car a))\
           (expr (cdr a)))\
          (if tests expr (cond (cdr x))))))";*/
