use crate::doc::{Documentation, LHelp};
use aries_utils::StreamingIterator;
use ompas_lisp::core::LEnv;
use ompas_lisp::structs::LError::{WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use rand::Rng;
use std::ops::Deref;
use std::sync::Arc;

/*
LANGUAGE
 */

const MOD_UTILS: &str = "mod-utils";
const RAND_ELEMENT: &str = "rand-element";
const ENUMERATE: &str = "enumerate";

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

        module
    }
}

impl Documentation for CtxUtils {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

///Return enumeration from a list of list
/// uses function from aries_utils
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
        let mut iter = val.iter();
        let mut enumeration = vec![];
        while let Some(val) = iter.next() {
            enumeration.push(val.deref().clone())
        }
        vec_result.push(enumeration.into())
    }

    Ok(vec_result.into())
}

///Return an element randomly chosen from a list
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
