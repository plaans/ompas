/*
LANGUAGE
*/
use crate::lisp_root::lisp_struct::*;
use std::any::{Any, TypeId};
use std::rc::Rc;

pub const COUNTER: &str = "counter";
pub const SET_COUNTER: &str = "set-counter";
pub const GET_COUNTER: &str = "get-counter";

#[derive(Default)]
pub struct Counter {
    v: u32,
}

impl NativeContext for Counter {
    fn get_component(&self, type_id: TypeId) -> Option<&dyn Any> {
        if type_id == TypeId::of::<u32>() {
            Some(&self.v)
        } else {
            None
        }
    }
    fn get_component_mut(&mut self, type_id: TypeId) -> Option<&mut dyn Any> {
        if type_id == TypeId::of::<u32>() {
            Some(&mut self.v)
        } else {
            None
        }
    }
}

pub fn get_counter(_: &[LValue], ctx: &dyn NativeContext) -> Result<LValue, LError> {
    if let Some(cnt) = ctx
        .get_component(TypeId::of::<u32>())
        .and_then(|x| x.downcast_ref::<u32>())
    {
        Ok(LValue::Number(LNumber::Int(*cnt as i64)))
    } else {
        Err(LError::SpecialError("No such component".to_string()))
    }
}

pub fn set_counter(args: &[LValue], ctx: &mut dyn NativeContext) -> Result<LValue, LError> {
    if let Some(cnt) = ctx
        .get_component_mut(TypeId::of::<u32>())
        .and_then(|x| x.downcast_mut::<u32>())
    {
        *cnt = match args[0] {
            LValue::Number(LNumber::Int(x)) => x as u32,
            _ => panic!("type error"),
        };
        Ok(LValue::None)
    } else {
        Err(LError::SpecialError("No such component".to_string()))
    }
}

impl AsModule for Counter {
    fn get_module() -> Module {
        let mut prelude_unmut = vec![];
        let mut prelude_mut = vec![];
        prelude_unmut.push((
            GET_COUNTER.into(),
            LNativeLambda {
                fun: Rc::new(get_counter),
            },
        ));
        prelude_mut.push((
            SET_COUNTER.into(),
            LNativeMutLambda {
                fun: Rc::new(set_counter),
            },
        ));

        Module {
            ctx: Box::new(Self::default()),
            prelude_unmut,
            prelude_mut,
        }
    }
}
