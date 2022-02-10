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

use crate::core::root_module::list::car;
use crate::core::structs::contextcollection::Context;
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lcoreoperator::LCoreOperator;
use crate::core::structs::lenv::LEnv;
use crate::core::structs::lerror::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use crate::core::structs::lerror::LResult;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{IntoModule, Module};
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;
use crate::core::{activate_debug, eval};
use crate::modules::utils::language::*;
use ::macro_rules_attribute::macro_rules_attribute;
use aries_utils::StreamingIterator;
use ompas_utils::dyn_async;
use rand::Rng;
use std::ops::Deref;

//LANGUAGE
pub mod language {
    pub const MOD_UTILS: &str = "utils";
    pub const ARBITRARY: &str = "arbitrary";
    pub const RAND_ELEMENT: &str = "rand-element";
    pub const ENUMERATE: &str = "enumerate";
    pub const CONTAINS: &str = "contains";
    pub const SUB_LIST: &str = "sublist";
    pub const QUOTE_LIST: &str = "quote-list";
    pub const TRANSFORM_IN_SINGLETON_LIST: &str = "transform-in-singleton-list";

    pub const LET: &str = "let";
    pub const LET_STAR: &str = "let*";
    pub const COND: &str = "cond";
    pub const TEST_MACRO: &str = "test-macro";
    //Not yet implemented
    pub const FN_MAP: &str = "map";
    pub const APPLY: &str = "APPLY";
    pub const ZIP: &str = "zip";
    pub const UNZIP: &str = "unzip";
    pub const COMBINE: &str = "combine";

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
}

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
                //LAMBDA_ARBITRARY,
                LAMBDA_EVAL_NON_RECURSIVE,
            ]
            .into(),
            label: MOD_UTILS.into(),
        };

        module.add_async_fn_prelude(ARBITRARY, arbitrary);
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

#[macro_rules_attribute(dyn_async!)]
pub async fn arbitrary<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    /*pub const LAMBDA_ARBITRARY: &str = "(define arbitrary
    (lambda args
        (cond ((= (len args) 1) ; default case
               (car (first args)))
              ((= (len args) 2) ; specific function
               (let ((l (first args))
                     (f (second args)))
                    (f l)))
              (else nil)))) ; error cases";*/

    activate_debug();

    match args.len() {
        1 => car(&[args[0].clone()], env),
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
        _ => Err(WrongNumberOfArgument(
            ARBITRARY,
            args.into(),
            args.len(),
            1..2,
        )),
    }
}

///Return enumeration from a list of list
///uses function from aries_utils
/// # Example:
///``` rust    #[allow(unused_mut)]
/// use ompas_lisp::modules::utils::{CtxUtils, enumerate};
/// use ompas_lisp::core::structs::lvalue::LValue;
/// use ompas_lisp::core::structs::lenv::LEnv;
/// let lists: &[LValue] = &[vec![1,2,3].into(), vec![4,5,6].into()];
/// let enumeration = enumerate(lists, &LEnv::default());
/// ```
pub fn enumerate(args: &[LValue], _: &LEnv) -> LResult {
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
pub fn rand_element(args: &[LValue], _: &LEnv) -> LResult {
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
                    TypeLValue::Symbol,
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
pub fn contains(args: &[LValue], _: &LEnv) -> LResult {
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

//returns a sublist of the a list
pub fn sublist(args: &[LValue], _: &LEnv) -> LResult {
    match args.len() {
        2 => {
            if let LValue::List(l) = &args[0] {
                if let LValue::Number(n) = &args[1] {
                    if n.is_natural() {
                        let i: usize = n.into();
                        Ok(l[i..].into())
                    } else {
                        Err(SpecialError(
                            SUB_LIST,
                            "Indexes should be natural numbers".to_string(),
                        ))
                    }
                } else {
                    Err(WrongType(
                        SUB_LIST,
                        args[1].clone(),
                        (&args[1]).into(),
                        TypeLValue::Number,
                    ))
                }
            } else {
                Err(WrongType(
                    SUB_LIST,
                    args[0].clone(),
                    (&args[0]).into(),
                    TypeLValue::List,
                ))
            }
        }
        3 => {
            if let LValue::List(l) = &args[0] {
                if let LValue::Number(n1) = &args[1] {
                    if let LValue::Number(n2) = &args[2] {
                        if n1.is_natural() && n2.is_natural() {
                            let i1: usize = n1.into();
                            let i2: usize = n2.into();
                            Ok(l[i1..i2].into())
                        } else {
                            Err(SpecialError(
                                SUB_LIST,
                                "Indexes should be natural numbers".to_string(),
                            ))
                        }
                    } else {
                        Err(WrongType(
                            SUB_LIST,
                            args[1].clone(),
                            (&args[2]).into(),
                            TypeLValue::Number,
                        ))
                    }
                } else {
                    Err(WrongType(
                        SUB_LIST,
                        args[1].clone(),
                        (&args[1]).into(),
                        TypeLValue::Number,
                    ))
                }
            } else {
                Err(WrongType(
                    SUB_LIST,
                    args[0].clone(),
                    (&args[0]).into(),
                    TypeLValue::List,
                ))
            }
        }
        _ => Err(WrongNumberOfArgument(
            SUB_LIST,
            args.into(),
            args.len(),
            2..3,
        )),
    }
}

pub fn quote_list(args: &[LValue], _: &LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            QUOTE_LIST,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    if let LValue::List(l) = &args[0] {
        let mut vec: Vec<LValue> = vec![];
        for e in l {
            vec.push(vec![LCoreOperator::Quote.into(), e.clone()].into());
        }
        Ok(vec.into())
    } else if let LValue::Nil = &args[0] {
        Ok(LValue::Nil)
    } else {
        Err(WrongType(
            QUOTE_LIST,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::List,
        ))
    }
}

pub fn transform_in_singleton_list(args: &[LValue], _: &LEnv) -> LResult {
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
    use crate::core::parse;
    use crate::core::root_module::list::language::SECOND;
    use crate::core::structs::lerror;
    use crate::modules::utils::*;
    use crate::test_utils::{test_expression, TestExpression};

    #[tokio::test]
    async fn test_arbitrary() -> lerror::Result<()> {
        let env = LEnv::root().await;

        let lv = &[vec![1, 2, 3].into()];
        let result = arbitrary(lv, &env).await?;
        assert_eq!(result, LValue::from(1));

        let lv = &[vec![1, 2, 3].into(), SECOND.into()];
        let result = arbitrary(lv, &env).await?;
        assert_eq!(result, LValue::from(2));

        Ok(())
    }
    #[test]
    fn test_contains() -> lerror::Result<()> {
        let lv: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 6.into()];
        let result = contains(lv, &LEnv::default())?;
        assert_eq!(result, LValue::True);
        Ok(())
    }

    #[test]
    fn test_sublist() -> lerror::Result<()> {
        let lv_1: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into()];
        let lv_2: &[LValue] = &[vec![1, 2, 3, 4, 5, 6].into(), 1.into(), 3.into()];
        let result1 = sublist(lv_1, &LEnv::default())?;
        let result2 = sublist(lv_2, &LEnv::default())?;
        assert_eq!(result1, vec![2, 3, 4, 5, 6].into());
        assert_eq!(result2, vec![2, 3].into());
        Ok(())
    }

    #[tokio::test]
    async fn test_macro_and() -> lerror::Result<()> {
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
    async fn test_macro_test_macro() -> lerror::Result<()> {
        let macro_to_test = TestExpression {
            inner: MACRO_TEST_MACRO,
            dependencies: vec![MACRO_AND],
            expression: "(test-macro \"(and (= 1 2) (> 3 4))\")",
            expanded: "(expand (parse \"(and (= 1 2) (> 3 4))\"))",
            result: "(if (= 1 2) (> 3 4) nil)",
        };

        test_expression(macro_to_test).await
    }

    #[tokio::test]
    async fn test_macro_or() -> lerror::Result<()> {
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
    async fn test_macro_caar() -> lerror::Result<()> {
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
    async fn test_macro_cadr() -> lerror::Result<()> {
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
    async fn test_macro_cdar() -> lerror::Result<()> {
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
    async fn test_macro_cddr() -> lerror::Result<()> {
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
    async fn test_macro_cadar() -> lerror::Result<()> {
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
    async fn test_macro_caddr() -> lerror::Result<()> {
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
    async fn test_macro_cdadr() -> lerror::Result<()> {
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
    async fn test_macro_caadr() -> lerror::Result<()> {
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
    async fn test_macro_cadadr() -> lerror::Result<()> {
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
    async fn test_macro_cadaddr() -> lerror::Result<()> {
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
    async fn test_macro_neq() -> lerror::Result<()> {
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
    async fn test_macro_neq_short() -> lerror::Result<()> {
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
    async fn test_macro_await_async() -> lerror::Result<()> {
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
    async fn test_macro_apply() -> lerror::Result<()> {
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
    async fn test_macro_cond() -> lerror::Result<()> {
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
    async fn test_macro_loop() -> lerror::Result<()> {
        let expression = "(loop (+ 1 1))";
        let expected = "(begin
            (define __loop__
                (lambda nil
                    (begin
                        (+ 1 1)
                        (__loop__))))
            (__loop__))))";

        let mut env = LEnv::root().await;

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
    async fn test_lambda_zip() -> lerror::Result<()> {
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
    async fn test_lambda_unzip() -> lerror::Result<()> {
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
    async fn test_lambda_mapf() -> lerror::Result<()> {
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
    async fn test_macro_let() -> lerror::Result<()> {
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
    async fn test_macro_let_star() -> lerror::Result<()> {
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
}
