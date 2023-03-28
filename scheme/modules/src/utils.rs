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
use sompas_language::utils::*;
use sompas_macros::scheme_fn;
use sompas_structs::lmodule::LModule;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use sompas_structs::{list, lruntimeerror};
use std::ops::Deref;

#[derive(Default, Copy, Clone, Debug)]
pub struct ModUtils {}

impl From<ModUtils> for LModule {
    fn from(m: ModUtils) -> Self {
        let mut module = LModule::new(m, MOD_UTILS, DOC_MOD_UTILS);
        module.add_fn(
            ENUMERATE,
            enumerate,
            (DOC_ENUMERATE, DOC_ENUMERATE_VERBOSE),
            true,
        );
        module.add_fn(
            RAND_ELEMENT,
            rand_element,
            (DOC_RAND_ELEMENT, DOC_RAND_ELEMENT_VERBOSE),
            false,
        );
        module.add_fn(CONTAINS, contains, DOC_CONTAINS, true);
        module.add_fn(SUBLIST, sublist, (DOC_SUBLIST, DOC_SUBLIST_VERBOSE), true);
        module.add_fn(
            QUOTE_LIST,
            quote_list,
            (DOC_QUOTE_LIST, DOC_QUOTE_LIST_VERBOSE),
            true,
        );
        module.add_fn(
            TRANSFORM_IN_SINGLETON_LIST,
            transform_in_singleton_list,
            (
                DOC_TRANSFORM_IN_SINGLETON_LIST,
                DOC_TRANSFORM_IN_SINGLETON_LIST_VERBOSE,
            ),
            true,
        );

        module.add_fn(RANGE, range, DOC_RANGE, true);

        module.add_macro(AND, MACRO_AND, DOC_AND);
        module.add_macro(OR, MACRO_OR, DOC_OR);
        module.add_macro(CAAR, MACRO_CAAR, DOC_CAAR);
        module.add_macro(CADR, MACRO_CADR, DOC_CADR);
        module.add_macro(CDAR, MACRO_CDAR, DOC_CDAR);
        module.add_macro(CDDR, MACRO_CDDR, DOC_CDDR);
        module.add_macro(CAADR, MACRO_CAADR, DOC_CAADR);
        module.add_macro(CADDR, MACRO_CADDR, DOC_CADDR);
        module.add_macro(CADAR, MACRO_CADAR, DOC_CADAR);
        module.add_macro(CDADR, MACRO_CDADR, DOC_CDADR);
        module.add_macro(CADADR, MACRO_CADADR, DOC_CADADR);
        module.add_macro(CADADDR, MACRO_CADADDR, DOC_CADADDR);
        module.add_macro(AWAIT_ASYNC, MACRO_AWAIT_ASYNC, DOC_AWAIT_ASYNC);
        module.add_macro(COND, MACRO_COND, (DOC_COND, DOC_COND_VERBOSE));
        module.add_lambda(UNZIP, LAMBDA_UNZIP, (DOC_UNZIP, DOC_UNZIP_VERBOSE));

        //todo:
        module.add_doc(FOR, DOC_FOR, "Lambda");
        //todo:
        module.add_doc(WHILE, DOC_WHILE, "Lambda");
        module.add_macro(LOOP, MACRO_LOOP, DOC_LOOP);
        module.add_macro(LET, MACRO_LET, (DOC_LET, DOC_LET_VERBOSE));
        module.add_macro(
            LET_STAR,
            MACRO_LET_STAR,
            (DOC_LET_STAR, DOC_LET_STAR_VERBOSE),
        );
        module.add_lambda(_LOOP_, LAMBDA__LOOP_, DOC__LOOP_);

        //Lambdas
        module.add_lambda(APPLY, LAMBDA_APPLY, (DOC_APPLY, DOC_APPLY_VERBOSE));
        module.add_lambda(ZIP, LAMBDA_ZIP, (DOC_ZIP, DOC_ZIP_VERBOSE));
        module.add_lambda(MAPF, LAMBDA_MAPF, (DOC_MAPF, DOC_MAPF_VERBOSE));
        module.add_lambda(PAR, LAMBDA_PAR, DOC_PAR);
        module.add_lambda(SEQ, LAMBDA_SEQ, DOC_SEQ);
        module.add_lambda(REPEAT, LAMBDA_REPEAT, DOC_REPEAT);
        module.add_lambda(RETRY_ONCE, LAMBDA_RETRY_ONCE, DOC_RETRY_ONCE);
        module.add_lambda(AWAIT_INTERRUPT, LAMBDA_AWAIT_INTERRUPT, DOC_AWAIT_INTERRUPT);

        module
    }
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
                    SUBLIST,
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
                    SUBLIST,
                    "Indexes should be natural numbers".to_string()
                ))
            }
        }
        _ => Err(LRuntimeError::wrong_number_of_args(SUBLIST, args, 2..3)),
    }
}
#[scheme_fn]
pub fn quote_list(mut list: Vec<LValue>) -> Vec<LValue> {
    //let mut vec: Vec<LValue> = vec![];
    let mut vec = vec![];
    for e in list.drain(..) {
        vec.push(list![LPrimitive::Quote.into(), e])
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

#[scheme_fn]
pub fn range(start: i64, end: i64) -> Vec<i64> {
    (start..end + 1).collect()
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
        let result = arbitrary(&env, lv).await?;
        assert_eq!(result, LValue::from(1));

        let lv = &[vec![1, 2, 3].into(), SECOND.into()];
        let result = arbitrary(&env, lv).await?;
        assert_eq!(result, LValue::from(2));

        Ok(())
    }
    #[test]
    fn test_contains() -> lruntimeerror::Result<()> {
        let lv: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 6.into()];
        let result = contains(&LEnv::default(), lv)?;
        assert_eq!(result, LValue::True);
        Ok(())
    }

    #[test]
    fn test_sublist() -> lruntimeerror::Result<()> {
        let lv_1: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into()];
        let lv_2: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into(), 3.into()];
        let result1 = sublist(&LEnv::default(), lv_1)?;
        let result2 = sublist(&LEnv::default(), lv_2)?;
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
