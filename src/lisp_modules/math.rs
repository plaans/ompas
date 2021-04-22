use crate::lisp_root::lisp_struct::LError::{WrongNumberOfArgument, WrongType};
use crate::lisp_root::lisp_struct::{
    AsModule, LError, LFn, LNumber, LValue, Module, NameTypeLValue,
};
use crate::lisp_root::RefLEnv;

/*
LANGUAGE LITERALS
 */

pub const PI: &str = "pi";
pub const SIN: &str = "sin";
pub const COS: &str = "cos";

#[derive(Default, Debug)]
pub struct CtxMath {}

pub fn sin(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.sin().into())
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Number)),
    }
}

pub fn cos(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(args.into(), args.len(), 1..1));
    }

    match &args[0] {
        LValue::Number(n) => {
            let f: f64 = n.into();
            Ok(f.cos().into())
        }
        lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Number)),
    }
}

impl AsModule for CtxMath {
    fn get_module() -> Module {
        let mut prelude = vec![];

        prelude.push((
            PI.into(),
            LValue::Number(LNumber::Float(std::f64::consts::PI)),
        ));
        prelude.push((COS.into(), LValue::Fn(LFn::new(Box::new(cos), COS.into()))));
        prelude.push((SIN.into(), LValue::Fn(LFn::new(Box::new(sin), SIN.into()))));

        Module {
            ctx: Box::new(CtxMath::default()),
            prelude,
        }
    }
}
