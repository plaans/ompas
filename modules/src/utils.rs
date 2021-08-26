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

use crate::doc::{Documentation, LHelp};
use aries_utils::StreamingIterator;
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use rand::Rng;
use std::ops::Deref;
use std::sync::Arc;

//LANGUAGE

const MOD_UTILS: &str = "mod-utils";
const RAND_ELEMENT: &str = "rand-element";
const ENUMERATE: &str = "enumerate";
const CONTAINS: &str = "contains";

// Documentation
const DOC_RAND_ELEMENT: &str = "Return a random element of a list";
const DOC_RAND_ELEMENT_VERBOSE: &str = "Example: \n(rand-element (list 1 2 3 4))\n=> 1";
const DOC_ENUMERATE: &str =
    "Return a enumeration of all possible combinations of elements of 1+ lists";
const DOC_ENUMERATE_VERBOSE: &str =
    "Example: \n(enumerate (list 1 2) (list 3 4))\n=> ((1 3)(1 4)(2 3)(2 4))";

#[derive(Default, Copy, Clone, Debug)]
pub struct CtxUtils {}

impl GetModule for CtxUtils {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_UTILS,
        };

        module.add_fn_prelude(RAND_ELEMENT, rand_element);
        module.add_fn_prelude(ENUMERATE, enumerate);
        module.add_fn_prelude(CONTAINS, contains);

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
/// use ompas_lisp::structs::LValue;
/// use ompas_lisp::core::LEnv;
/// let lists: &[LValue] = &[vec![1,2,3].into(), vec![4,5,6].into()];
/// let enumeration = enumerate(lists, &LEnv::default(), &CtxUtils::default());
/// ```
pub fn enumerate(args: &[LValue], _: &LEnv, _: &CtxUtils) -> Result<LValue, LError> {
    let mut vec_iter = vec![];
    for arg in args {
        if let LValue::List(iter) = arg {
            vec_iter.push(iter.iter())
        } else {
            return Err(WrongType(
                ENUMERATE,
                arg.clone(),
                arg.into(),
                NameTypeLValue::List,
            ));
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
