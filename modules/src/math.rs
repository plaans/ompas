use crate::doc::{Documentation, LHelp};
use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;

/*
LANGUAGE LITERALS
 */
const MOD_MATH: &str = "mod-math";
const DOC_MOD_MATH: &str =
    "Module handling mathematical functions for basic arithmetic operations and comparisons.";
const DOC_MOD_MATH_VERBOSE: &str = "functions:\n\
                                    -arithmetic: '+', '-', '*','/'\n\
                                    -comparison: '>', '>=', '<', '<='\n\
                                    -trigonometry: cos, sin\n\
                                    -constants: pi";

//Mathematical functions
const ADD: &str = "+";
const SUB: &str = "-";
const MUL: &str = "*";
const DIV: &str = "/";

//Comparison
const GT: &str = ">";
const LT: &str = "<";
const GE: &str = ">=";
const LE: &str = "<=";
const EQ: &str = "=";

//Trigonometry
const SIN: &str = "sin";
const COS: &str = "cos";

//Constants
const PI: &str = "pi";

#[derive(Default, Debug)]
pub struct CtxMath {}

impl GetModule for CtxMath {
    /// Provides all the functions to make some basic computation.
    /// New functions and constants will be added in time, when needed.
    /// Here is a list of all the elements provided by this module
    /// -Basic functions: "+", "-", "*", "/".
    /// -Comparisons: "<", ">", "<=", ">=".
    /// -Trigonometry: "sin", "cos".
    /// -Constants: "pi".
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Box::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_MATH,
        };

        module.add_fn_prelude(ADD, Box::new(add));
        module.add_fn_prelude(SUB, Box::new(sub));
        module.add_fn_prelude(MUL, Box::new(mul));
        module.add_fn_prelude(DIV, Box::new(div));
        module.add_fn_prelude(GT, Box::new(gt));
        module.add_fn_prelude(GE, Box::new(ge));
        module.add_fn_prelude(LT, Box::new(lt));
        module.add_fn_prelude(LE, Box::new(le));
        module.add_fn_prelude(COS, Box::new(cos));
        module.add_fn_prelude(SIN, Box::new(sin));
        module.add_fn_prelude(EQ, Box::new(eq));
        module.add_prelude(PI, std::f64::consts::PI.into());

        module
    }
}

/*
HELP
 */

const DOC_ADD: &str = "Takes 2+ arguments. Return the addition.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
const DOC_SUB: &str = "Takes 2 arguments. Return the substraction of the first by the second.\
return an error if inputs are not numbers or there is wrong numbers of arguments";
const DOC_MUL: &str = "Takes 2+ arguments. Return the multiplication.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
const DOC_DIV: &str = "Takes 2 arguments. Return the division of the first by the second.\
Return an error if inputs are not numbers or there is wrong numbers of arguments";
const DOC_GT: &str = "Takes 2 arguments. Return *true* if the first is greater than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_GE: &str = "Takes 2 arguments. Return *true* if the first is greater or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_LT: &str = "Takes 2 arguments. Return *true* if the first is less than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_LE: &str = "Takes 2 arguments. Return *true* if the first is less or equal than the second.\
Return *false* otherwise. Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_COS: &str = "Takes 1 argument. Return the cosinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_SIN: &str = "Takes 1 argument. Return the sinus of it.\
Return an error if args are not numbers of there is the wrong number of arguments";
const DOC_EQ: &str = "Takes 2 arguments. Return true if two arguments are equal. False otherwise.";

impl Documentation for CtxMath {
    fn documentation() -> Vec<LHelp> {
        vec![
            LHelp::new(MOD_MATH, DOC_MOD_MATH, Some(DOC_MOD_MATH_VERBOSE)),
            LHelp::new(ADD, DOC_ADD, None),
            LHelp::new(SUB, DOC_SUB, None),
            LHelp::new(MUL, DOC_MUL, None),
            LHelp::new(DIV, DOC_DIV, None),
            LHelp::new(GT, DOC_GT, None),
            LHelp::new(GE, DOC_GE, None),
            LHelp::new(LT, DOC_LT, None),
            LHelp::new(LE, DOC_LE, None),
            LHelp::new(SIN, DOC_SIN, None),
            LHelp::new(COS, DOC_COS, None),
            LHelp::new(EQ, DOC_EQ, None),
        ]
    }
}

///Compute the sin of a LNumber
/// Only takes one element in args
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

/// Compute the cos of a LNumber
/// Only takes one element in args
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

pub fn add(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(0.0));
    for value in args {
        result = (&result + value)?;
    }
    Ok(result)
}

pub fn sub(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] - &args[1],
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn mul(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    let mut result = LValue::Number(LNumber::Float(1.0));
    for value in args {
        result = (&result * value)?;
    }
    Ok(result)
}

pub fn div(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => &args[0] / &args[1],
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

//Comparison functions
pub fn gt(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] > args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn lt(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] < args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn ge(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] >= args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn le(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] <= args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn eq(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok((args[0] == args[1]).into()),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::convert::TryInto;

    /*fn generate_env() -> (RefLEnv, ContextCollection) {
        let env = RefLEnv::empty();
        let ctxs = ContextCollection::default();
        load_module(&mut env, &mut ctxs, CtxMath::get_module());
        (env, ctxs)
    }*/

    #[test]
    fn test_add() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result = add(&[3.into(), 2.into()], &env, &ctx).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(5.0)), result);
    }

    #[test]
    fn test_sub() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result = sub(&[3.into(), 2.into()], &env, &ctx).unwrap();
        assert_eq!(LValue::Number(LNumber::Int(1)), result);
    }

    #[test]
    fn test_mul() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result = mul(&[3.into(), 2.into()], &env, &ctx).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(6.0)), result);
    }

    #[test]
    fn test_div() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result = div(&[3.0.into(), 2.0.into()], &env, &ctx).unwrap();
        assert_eq!(LValue::Number(LNumber::Float(1.5)), result);
    }

    #[test]
    fn test_gt() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_true: bool = gt(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = gt(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = gt(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_ge() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_true: bool = ge(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false: bool = ge(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = ge(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }

    #[test]
    fn test_lt() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_false: bool = lt(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true: bool = lt(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_false_2: bool = lt(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_le() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_false: bool = le(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true: bool = le(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        let result_true_2: bool = le(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .try_into()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }

    #[test]
    fn test_cos() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        assert_eq!(cos(&[0.0.into()], &env, &ctx).unwrap(), 1.0.into())
    }

    #[test]
    fn test_sin() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        assert_eq!(sin(&[0.0.into()], &env, &ctx).unwrap(), 0.0.into())
    }
}
