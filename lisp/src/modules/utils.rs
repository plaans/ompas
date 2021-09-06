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

const MOD_UTILS: &str = "mod-utils";
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
pub const MACRO_LET: &str = "(defmacro let \
                                           (lambda (bindings body) \
                                                   (begin (define unzipped (unzip bindings)) \
                                                          (define keys (car unzipped)) \
                                                          (define values (cadr unzipped)) \
                                                          (cons (quasiquote (lambda (unquote keys) \
                                                                                    (unquote body))) values))))";

pub const MACRO_LET_STAR: &str = "(defmacro let* \
                                                (lambda (bindings body) \
                                                        (if (= (length bindings) 1) \
                                                            (cons (quasiquote (lambda (unquote (list (caar bindings))) \
                                                                                      (unquote body))) \
                                                                  (cdar bindings)) \
                                                            (cons (quasiquote (lambda (unquote (list (caar bindings))) \
                                                                                      (let* (unquote (cdr bindings)) \
                                                                                            (unquote body)))) \
                                                                  (cdar bindings)))))";

//TODO: find a way to make it a lambda
pub const MACRO_APPLY: &str = "(defmacro apply\
                                              (lambda (f args)\
                                                      (cons f args)))";

pub const MACRO_AND: &str = "(defmacro and (lambda args\
                                                (if (null? args)\
                                                    nil\
                                                    (if (= (length args) 1)\
                                                        (car args)\
                                                        (quasiquote (if (unquote (car args))\
                                                                        (and (unquote (cdr args)))\
                                                                        nil))))))";

pub const MACRO_OR: &str= "(defmacro or (lambda args\
                                                (if (null? args)\
                                                    nil\
                                                    (if (= (length args) 1)\
                                                        (car args)\
                                                        (quasiquote (if (unquote (car args))\
                                                                        true \
                                                                        (or (unquote (cdr args)))))))))";

pub const MACRO_NEQ: &str = "(defmacro neq (lambda (a b)\
                                            (! (= a b))))";

pub const MACRO_NEQ_SHORT: &str = "(defmacro != (lambda (a b) \
                                                (neq a b )))";

pub const MACRO_COND: &str = "(defmacro cond (lambda exprs \
                                                (if (null? exprs) \
                                                    nil \
                                                    (if (= (caar exprs) (quote else)) \
                                                        (cadar exprs) \
                                                        (quasiquote \
                                                            (if (unquote (caar exprs)) \
                                                                (unquote (cadar exprs)) \
                                                                (cond (unquote (cdr exprs)))))))))";

pub const MACRO_AWAIT_ASYNC: &str = "(defmacro await-async (lambda (x) `(await (async ,x))))";

pub const MACRO_FOR: &str = "(defmacro for (lambda args \
    (let ((_i_ (get-list args 0)) \
            (_list_ (get-list args 2)) \
            (_body_ (get-list args 3))) \
        `(let ((_f_loop_ (lambda args \
            (if (null? args) \
                nil \
                (let ((,_i_ (car args))) \
                    (begin \
                        ,_body_ \
                        (_f_loop_ (cdr args)))))))) \
            (_f_loop_ ,_list_)))))";

pub const MACRO_CAAR: &str = "(defmacro caar (lambda (x) (quasiquote (car (car (unquote x))))))";
pub const MACRO_CADR: &str = "(defmacro cadr (lambda (x) (quasiquote (car (cdr (unquote x))))))";
pub const MACRO_CDAR: &str = "(defmacro cdar (lambda (x) (quasiquote (cdr (car (unquote x))))))";
pub const MACRO_CDDR: &str = "(defmacro cddr (lambda (x) (quasiquote (cdr (cdr (unquote x))))))";
pub const MACRO_CAADR: &str =
    "(defmacro caadr (lambda (x) (quasiquote (car (car (cdr(unquote x)))))))";
pub const MACRO_CADDR: &str =
    "(defmacro caddr (lambda (x) (quasiquote (car (cdr (cdr (unquote x)))))))";
pub const MACRO_CADAR: &str =
    "(defmacro cadar (lambda (x) (quasiquote (car (cdr (car (unquote x)))))))";
pub const MACRO_CADADR: &str =
    "(defmacro cadadr (lambda (x) (quasiquote (car (cdr (car (cdr (unquote x))))))))";
pub const MACRO_CDADR: &str =
    "(defmacro cdadr (lambda (x) (quasiquote (cdr (car (cdr (unquote x)))))))";
pub const MACRO_CADADDR: &str =
    "(defmacro cadaddr (lambda (x) (quasiquote (car (cdr (car (cdr (cdr (unquote x)))))))))";

//LAMBDAS
pub const LAMBDA_AND: &str = "(define and (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) (and (cdr args)) nil)))))";
pub const LAMBDA_OR: &str = "(define or (lambda args \
                                                   (if (null? args) true \
                                                       (if (= (length args) 1) (car args)
                                                           (if (car args) true (or (cdr args)))))))";

pub const LAMBDA_COND: &str = "(define cond (lambda x \
                                            (if (null? x)\
                                                nil\
                                                (let ((a (car x))\
                                                       (tests (car a))\
                                                       (expr (cdr a)))\
                                                      (if tests expr (cond (cdr x))))))";

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
        (lambda lists
                (begin
                    (define firsts (lambda lists
                                            (if (null? lists)
                                                nil
                                                (cons (caar lists)
                                                      (firsts (cdr lists))))))
                    (define seconds (lambda lists
                                            (if (null? lists)
                                            nil
                                            (cons (cadar lists)
                                                  (seconds (cdr lists))))))
                    (list (firsts lists) (seconds lists)))))";

pub const LAMBDA_MAPF : &str = "(define mapf (lambda (f seq)\
                                                         (if (null? seq)\
                                                         nil\
                                                         (cons (eval (cons f (car seq))) (mapf f (cdr seq))))))";

#[derive(Default, Copy, Clone, Debug)]
pub struct CtxUtils {}

impl GetModule for CtxUtils {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: vec![
                MACRO_CAAR,
                MACRO_CADR,
                MACRO_CDAR,
                MACRO_CDDR,
                MACRO_CADAR,
                MACRO_CADDR,
                MACRO_CADADR,
                MACRO_CDADR,
                MACRO_CADADDR,
                MACRO_CAADR,
                MACRO_AND,
                MACRO_OR,
                MACRO_NEQ,
                MACRO_NEQ_SHORT,
                MACRO_LET,
                MACRO_LET_STAR,
                MACRO_APPLY,
                MACRO_COND,
                MACRO_AWAIT_ASYNC,
                LAMBDA_UNZIP,
                LAMBDA_ZIP,
                LAMBDA_MAPF,
                MACRO_FOR,
            ]
            .into(),
            label: MOD_UTILS,
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
/// use ompas_modules::utils::{enumerate, CtxUtils};
/// use ompas_lisp::modules::utils::{CtxUtils, enumerate};
/// use ompas_lisp::structs::LValue;
/// use ompas_lisp::core::LEnv;
/// let lists: &[LValue] = &[vec![1,2,3].into(), vec![4,5,6].into()];
/// let enumeration = enumerate(lists, &LEnv::default(), &CtxUtils::default());
/// ```
pub fn enumerate(args: &[LValue], _: &LEnv, _: &CtxUtils) -> Result<LValue, LError> {
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
pub fn rand_element(args: &[LValue], _: &LEnv, _: &CtxUtils) -> Result<LValue, LError> {
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
pub fn contains(args: &[LValue], _: &LEnv, _: &CtxUtils) -> Result<LValue, LError> {
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

pub fn transform_in_singleton_list(
    args: &[LValue],
    _: &LEnv,
    _: &CtxUtils,
) -> Result<LValue, LError> {
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
