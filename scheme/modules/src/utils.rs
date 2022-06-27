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

use aries_utils::StreamingIterator;
use rand::Rng;
use sompas_core::eval;
use sompas_core::modules::list::car;
use sompas_language::*;
use sompas_macros::async_scheme_fn;
use sompas_macros::scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::{Documentation, LHelp};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lenv::LEnv;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use sompas_structs::{list, lruntimeerror};
use std::ops::Deref;

//LANGUAGE
pub const MOD_UTILS: &str = "utils";

// Documentation
pub const DOC_MOD_UTILS: &str = "collection of utility functions.";
pub const DOC_MOD_UTILS_VERBOSE: &str = "functions:\n\
-rand-element\n\
-enumerate\n\
-contains\n\
-sublist\n\
-transfrom-in-singleton-list\n";
pub const DOC_RAND_ELEMENT: &str = "Return a random element of a list";
pub const DOC_RAND_ELEMENT_VERBOSE: &str = "Example: \n(rand-element (list 1 2 3 4))\n=> 1";
pub const DOC_ENUMERATE: &str =
    "Return a enumeration of all possible combinations of elements of 1+ lists";
pub const DOC_ENUMERATE_VERBOSE: &str =
    "Example: \n(enumerate (list 1 2) (list 3 4))\n=> ((1 3)(1 4)(2 3)(2 4))";
pub const DOC_CONTAINS: &str =
    "Returns true if a LValue is contains into an other (for a map if the key is inside it)";
pub const DOC_SUB_LIST: &str = "Returns a sublist of a list";
pub const DOC_TRANSFORM_IN_SINGLETON_LIST: &str = "todo!";

pub const DOC_LET: &str = "Macro used to abstract variable binding in functional programming.";
pub const DOC_LET_STAR: &str = "Macro used to abstract variable binding in functional programming.\
    The difference with let is that you can bind variables in function of previously bound variables.";
pub const DOC_MACRO_TEST_MACRO: &str = "Test the macro expansion. Used mainly for debug";

//MACROS
pub const MACRO_TEST_MACRO: &str = "(defmacro test-macro
   (lambda (x)
    `(expand (parse ,x))))";

pub const MACRO_AND: &str = "(defmacro and
                                (lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (len args) 1)
                                            (car args)
                                            `(if ,(car args)
                                                 ,(cons 'and (cdr args))
                                                 nil)))))";

pub const MACRO_OR: &str = "(defmacro or 
                                (lambda args
                                    (if (null? args)
                                        nil
                                        (if (= (len args) 1)
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

/*pub const MACRO_NEQ: &str = "(defmacro neq
                                (lambda (a b)
                                    `(! (= ,a ,b))))";

pub const MACRO_NEQ_SHORT: &str = "(defmacro !=
                                        (lambda (a b)
                                            `(neq ,a ,b)))";*/

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
        (if (= (len bindings) 1)
            (cons `(lambda ,(list (caar bindings))
                           ,body)
                    (cdar bindings))
            (cons `(lambda ,(list (caar bindings))
                            (let* ,(cdr bindings) ,body))
                        (cdar bindings)))))";

/*pub const MACRO_DO: &str = "(defmacro do
(lambda args
    (if (= (len args) 1)
        (car args)
        `(begin
            (define __result__ ,(car args))
            (if (err? __result__)
                __result__
                ,(cons 'do (cdr args)))))))";*/

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
        (cond ((= (len args) 1) ; default case
               (car (first args)))
              ((= (len args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases";

pub const LAMBDA_EVAL_NON_RECURSIVE: &str = "(define enr
    (lambda (l)
        (eval (cons (car l) (quote-list (cdr l))))))";

pub const EVAL_NON_RECURSIVE: &str = "enr";

pub const DOC_ARBITRARY: &str = "todo!";
pub const DOC_EVAL_NON_RECURSIVE: &str = "todo!";

#[derive(Default, Copy, Clone, Debug)]
pub struct CtxUtils {}

impl IntoModule for CtxUtils {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(()),
            prelude: vec![],
            raw_lisp: vec![
                //MACRO_TEST_MACRO,
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
                //LAMBDA_ARBITRARY,
                //LAMBDA_EVAL_NON_RECURSIVE,
            ]
            .into(),
            label: MOD_UTILS.into(),
        };

        module.add_async_fn_prelude(ARBITRARY, arbitrary);
        module.add_async_fn_prelude(EVAL_NON_RECURSIVE, enr);
        module.add_fn_prelude(RAND_ELEMENT, rand_element);
        module.add_fn_prelude(ENUMERATE, enumerate);
        module.add_fn_prelude(CONTAINS, contains);
        module.add_fn_prelude(SUB_LIST, sublist);
        module.add_fn_prelude(TRANSFORM_IN_SINGLETON_LIST, transform_in_singleton_list);
        module.add_fn_prelude(QUOTE_LIST, quote_list);

        module
    }

    fn documentation(&self) -> Documentation {
        vec![
            LHelp::new_verbose(RAND_ELEMENT, DOC_RAND_ELEMENT, DOC_RAND_ELEMENT_VERBOSE),
            LHelp::new_verbose(ENUMERATE, DOC_ENUMERATE, DOC_ENUMERATE_VERBOSE),
            LHelp::new_verbose(MOD_UTILS, DOC_MOD_UTILS, DOC_MOD_UTILS_VERBOSE),
            LHelp::new(SUB_LIST, DOC_SUB_LIST),
            LHelp::new(CONTAINS, DOC_CONTAINS),
            LHelp::new(TRANSFORM_IN_SINGLETON_LIST, DOC_TRANSFORM_IN_SINGLETON_LIST),
            LHelp::new(LET, DOC_LET),
            LHelp::new(LET_STAR, DOC_LET_STAR),
            LHelp::new(TEST_MACRO, DOC_MACRO_TEST_MACRO),
        ]
        .into()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        vec![
            RAND_ELEMENT,
            ENUMERATE,
            CONTAINS,
            SUB_LIST,
            TRANSFORM_IN_SINGLETON_LIST,
            QUOTE_LIST,
        ]
        .into()
    }
}

#[async_scheme_fn]
pub async fn arbitrary(env: &LEnv, args: &[LValue]) -> LResult {
    /*pub const LAMBDA_ARBITRARY: &str = "(define arbitrary
    (lambda args
        (cond ((= (len args) 1) ; default case
               (car (first args)))
              ((= (len args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases";*/

    //activate_debug();

    match args.len() {
        1 => car(env, &[args[0].clone()]),
        2 => {
            eval(
                &vec![
                    args[1].clone(),
                    vec![LCoreOperator::Quote.into(), args[0].clone()].into(),
                ]
                .into(),
                &mut env.clone(),
            )
            .await
        }
        _ => Err(LRuntimeError::wrong_number_of_args(ARBITRARY, args, 1..2)),
    }
}

#[async_scheme_fn]
pub async fn enr<'a>(env: &'a LEnv, mut args: Vec<LValue>) -> LResult {
    for (i, arg) in args.iter_mut().enumerate() {
        if i != 0 {
            *arg = list![LCoreOperator::Quote.into(), arg.clone()]
        }
    }

    eval(&args.into(), &mut env.clone()).await
}

///Return enumeration from a list of list
///uses function from aries_utils
/// # Example:
///``` rust #[allow(unused_mut)]
/// use sompas_structs::lvalue::LValue;
/// use sompas_modules::utils::enumerate;
/// use sompas_structs::lenv::LEnv;
/// let lists: &[LValue] = &[vec![1,2,3].into(), vec![4,5,6].into()];
/// let enumeration = enumerate(lists, &LEnv::default());
/// ```
#[scheme_fn]
pub fn enumerate(element: &[LValue]) -> Vec<LValue> {
    let mut vec_iter = vec![];
    let mut new_args = vec![];
    for arg in element {
        if let LValue::List(_) = arg {
            new_args.push(arg.clone())
        } else {
            new_args.push(list![arg.clone()])
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

    vec_result
}

///Return an element randomly chosen from a list
/// Takes a LValue::List as arg.
#[scheme_fn]
pub fn rand_element(list: Vec<LValue>) -> LValue {
    let index = rand::thread_rng().gen_range(0..list.len());
    list[index].clone()
}

///Takes a list or map and search if it contains a LValue inside
#[scheme_fn]
pub fn contains(set: &LValue, val: &LValue) -> bool {
    match &set {
        LValue::List(vec) => {
            for e in vec.iter() {
                if e == val {
                    return true;
                }
            }
            false
        }
        LValue::Map(m) => {
            for e in m.keys() {
                if e == val {
                    return true;
                }
            }
            false
        }
        _ => false,
    }
}

//returns a sublist of the a list
#[scheme_fn]
pub fn sublist(args: &[LValue]) -> Result<Vec<LValue>, LRuntimeError> {
    match args.len() {
        2 => {
            let list: Vec<LValue> =
                <Vec<LValue>>::try_from(&args[0]).map_err(|e| e.chain("sublist"))?;
            let n: LNumber = <LNumber>::try_from(&args[1]).map_err(|e| e.chain("sublist"))?;

            if n.is_natural() {
                let i: usize = n.into();
                Ok(list[i..].to_vec())
            } else {
                Err(lruntimeerror!(
                    SUB_LIST,
                    "Indexes should be natural numbers".to_string()
                ))
            }
        }
        3 => {
            let list: Vec<LValue> =
                <Vec<LValue>>::try_from(&args[0]).map_err(|e| e.chain("sublist"))?;
            let n1: LNumber = <LNumber>::try_from(&args[1]).map_err(|e| e.chain("sublist"))?;
            let n2: LNumber = <LNumber>::try_from(&args[2]).map_err(|e| e.chain("sublist"))?;

            if n1.is_natural() && n2.is_natural() {
                let i1: usize = n1.into();
                let i2: usize = n2.into();
                Ok(list[i1..i2].to_vec())
            } else {
                Err(lruntimeerror!(
                    SUB_LIST,
                    "Indexes should be natural numbers".to_string()
                ))
            }
        }
        _ => Err(LRuntimeError::wrong_number_of_args(SUB_LIST, args, 2..3)),
    }
}
#[scheme_fn]
pub fn quote_list(mut list: Vec<LValue>) -> Vec<LValue> {
    //let mut vec: Vec<LValue> = vec![];
    let mut vec = vec![];
    for e in list.drain(..) {
        vec.push(list![LCoreOperator::Quote.into(), e])
        //vec.push(vec![LCoreOperator::Quote.into(), e.clone()].into());
    }
    vec
}

#[scheme_fn]
pub fn transform_in_singleton_list(args: &[LValue]) -> Vec<LValue> {
    args.iter()
        .map(|lv| list![lv.clone()])
        .collect::<Vec<LValue>>()
}

#[cfg(test)]
mod test {
    use super::*;
    use sompas_core::test_utils::{test_expression, TestExpression};
    use sompas_core::{get_root_env, parse};
    use sompas_structs::lruntimeerror;

    #[tokio::test]
    async fn test_arbitrary() -> lruntimeerror::Result<()> {
        let env = get_root_env().await;

        let lv = &[vec![1, 2, 3].into()];
        let result = arbitrary(lv, &env).await?;
        assert_eq!(result, LValue::from(1));

        let lv = &[vec![1, 2, 3].into(), SECOND.into()];
        let result = arbitrary(lv, &env).await?;
        assert_eq!(result, LValue::from(2));

        Ok(())
    }
    #[test]
    fn test_contains() -> lruntimeerror::Result<()> {
        let lv: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 6.into()];
        let result = contains(lv, &LEnv::default())?;
        assert_eq!(result, LValue::True);
        Ok(())
    }

    #[test]
    fn test_sublist() -> lruntimeerror::Result<()> {
        let lv_1: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into()];
        let lv_2: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into(), 3.into()];
        let result1 = sublist(lv_1, &LEnv::default())?;
        let result2 = sublist(lv_2, &LEnv::default())?;
        assert_eq!(result1, vec![2, 3, 4, 5, 6].into());
        assert_eq!(result2, vec![2, 3].into());
        Ok(())
    }

    #[tokio::test]
    async fn test_macro_and() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_AND,
            dependencies: vec![],
            expression: "(and (= (+ 1 1) 2) (< 3 4))",
            expected: "(if (= (+ 1 1) 2) (< 3 4) nil)",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_test_macro() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_TEST_MACRO,
            dependencies: vec![MACRO_AND],
            expression: "(test-macro \"(and (= 1 2) (> 3 4))\")",
            expected: "(expand (parse \"(and (= 1 2) (> 3 4))\"))",
            result: "(if (= 1 2) (> 3 4) nil)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_or() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_OR,
            dependencies: vec![],
            expression: "(or (= (+ 1 1) 2) (< 3 4))",
            expected: "(if (= (+ 1 1) 2) true (< 3 4))",
            result: "true",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caar() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CAAR,
            dependencies: vec![],
            expression: "(caar '((1 2) (3 4)))",
            expected: "(car (car (quote ((1 2) (3 4)))))",
            result: "1",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADR,
            dependencies: vec![],
            expression: "(cadr '((1 2) (3 4)))",
            expected: "(car (cdr (quote ((1 2) (3 4)))))",
            result: "(3 4)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cdar() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDAR,
            dependencies: vec![],
            expression: "(cdar '((1 2) (3 4)))",
            expected: "(cdr (car (quote ((1 2) (3 4)))))",
            result: "(2)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cddr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDDR,
            dependencies: vec![],
            expression: "(cddr '((1 2) (3 4) 5))",
            expected: "(cdr (cdr (quote ((1 2) (3 4) 5))))",
            result: "(5)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadar() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADAR,
            dependencies: vec![],
            expression: "(cadar '((1 2) (3 4) 5))",
            expected: "(car (cdr (car '((1 2) (3 4) 5))))",
            result: "2",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caddr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADDR,
            dependencies: vec![],
            expression: "(caddr '((1 2) (3 4) 5))",
            expected: "(car (cdr (cdr '((1 2) (3 4) 5))))",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cdadr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CDADR,
            dependencies: vec![],
            expression: "(cdadr '((1 2) (3 4) 5))",
            expected: "(cdr (car (cdr '((1 2) (3 4) 5))))",
            result: "(4)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_caadr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CAADR,
            dependencies: vec![],
            expression: "(caadr '((1 2) (3 4) 5))",
            expected: "(car (car (cdr '((1 2) (3 4) 5))))",
            result: "3",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadadr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADADR,
            dependencies: vec![],
            expression: "(cadadr '((1 2) (3 4) 5))",
            expected: "(car (cdr (car (cdr '((1 2) (3 4) 5)))))",
            result: "4",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cadaddr() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_CADADDR,
            dependencies: vec![],
            expression: "(cadaddr '((1 2) (3 4) ((5 6) 7)))",
            expected: "(car (cdr (car (cdr (cdr '((1 2) (3 4) ((5 6) 7)))))))",
            result: "7",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_await_async() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_AWAIT_ASYNC,
            dependencies: vec![],
            expression: "(await-async (+ 1 4))",
            expected: "(await (async (+ 1 4)))",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_apply() -> lruntimeerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_APPLY,
            dependencies: vec![],
            expression: "(apply + (1 4))",
            expected: "(+ 1 4)",
            result: "5",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_cond() -> lruntimeerror::Result<()> {
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
            expected: "(begin
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
    async fn test_macro_loop() -> lruntimeerror::Result<()> {
        let expression = "(loop (+ 1 1))";
        let expected = "(begin
            (define __loop__
                (lambda nil
                    (begin
                        (+ 1 1)
                        (__loop__))))
            (__loop__))))";

        let mut env = get_root_env().await;

        //Load macro
        parse(MACRO_LOOP, &mut env).await?;

        //Expand expression
        let expanded = parse(expression, &mut env).await?;

        let expected = parse(expected, &mut env).await?;

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
    async fn test_lambda_zip() -> lruntimeerror::Result<()> {
        let test_lambda = TestExpression {
            inner: LAMBDA_ZIP,
            dependencies: vec![MACRO_OR],
            expression: "(zip '(1 2 3 4) '(5 6 7 8))",
            expected: "(zip '(1 2 3 4) '(5 6 7 8))",
            result: "((1 5) (2 6) (3 7) (4 8))",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_lambda_unzip() -> lruntimeerror::Result<()> {
        let test_lambda = TestExpression {
            inner: LAMBDA_UNZIP,
            dependencies: vec![MACRO_CAAR, MACRO_CADAR, MACRO_APPLY],
            expression: "(unzip '((1 5) (2 6) (3 7) (4 8)))",
            expected: "(unzip '((1 5) (2 6) (3 7) (4 8)))",
            result: "((1 2 3 4) (5 6 7 8))",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_lambda_mapf() -> lruntimeerror::Result<()> {
        let test_lambda = TestExpression {
            inner: LAMBDA_MAPF,
            dependencies: vec![],
            expression: "(begin
                            (define square (lambda (x)  (* x x)))
                            (mapf square '(1 2 3 4 5)))",
            expected: "(begin
                            (define square (lambda (x)  (* x x)))
                            (mapf square '(1 2 3 4 5)))",
            result: "(1 4 9 16 25)",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_macro_let() -> lruntimeerror::Result<()> {
        let test_lambda = TestExpression {
            inner: MACRO_LET,
            dependencies: vec![MACRO_CAAR, MACRO_CADAR, MACRO_CADR, LAMBDA_UNZIP],
            expression: "(let ((x 1)
                               (y 2))
                              (+ x y))",
            expected: "((lambda (x y) (+ x y)) 1 2)",
            result: "3",
        };

        test_expression(test_lambda).await
    }

    #[tokio::test]
    async fn test_macro_let_star() -> lruntimeerror::Result<()> {
        let test_lambda = TestExpression {
            inner: MACRO_LET_STAR,
            dependencies: vec![MACRO_CAAR, MACRO_CDAR],
            expression: "(let* ((x 1)\
                               (y (+ x 1)))\
                              (+ x y))",
            expected: "((lambda (x) ((lambda (y) (+ x y)) (+ x 1))) 1)",
            result: "3",
        };

        test_expression(test_lambda).await
    }
}
