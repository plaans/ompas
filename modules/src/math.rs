use ompas_lisp::core::*;
use ompas_lisp::structs::LError::*;
use ompas_lisp::structs::*;

/*
LANGUAGE LITERALS
 */

//Mathematical functions
pub const ADD: &str = "+";
pub const SUB: &str = "-";
pub const MUL: &str = "*";
pub const DIV: &str = "/";

//Comparison
pub const GT: &str = ">";
pub const LT: &str = "<";
pub const GE: &str = ">=";
pub const LE: &str = "<=";
pub const EQ: &str = "=";

//Trigonometry
pub const SIN: &str = "sin";
pub const COS: &str = "cos";

//Constants
pub const PI: &str = "pi";

#[derive(Default, Debug)]
pub struct CtxMath {}

impl AsModule for CtxMath {
    /// Provides all the functions to make some basic computation.
    /// New functions and constants will be added in time, when needed.
    /// Here is a list of all the elements provided by this module
    /// -Basic functions: "+", "-", "*", "/".
    /// -Comparisons: "<", ">", "<=", ">=".
    /// -Trigonometry: "sin", "cos".
    /// -Constants: "pi".
    fn get_module() -> Module {
        let mut module = Module {
            ctx: Box::new(CtxMath::default()),
            prelude: vec![]
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
        module.add_prelude(PI, std::f64::consts::PI.into());

        module
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

///Compute the cos of a LNumber
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
        2 => Ok(LValue::Bool(args[0] > args[1])),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn lt(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok(LValue::Bool(args[0] < args[1])),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn ge(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok(LValue::Bool(args[0] >= args[1])),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn le(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok(LValue::Bool(args[0] <= args[1])),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

pub fn eq(args: &[LValue], _: &RefLEnv, _: &CtxMath) -> Result<LValue, LError> {
    match args.len() {
        2 => Ok(LValue::Bool(args[0] == args[1])),
        i => Err(WrongNumberOfArgument(args.into(), i, 2..2)),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

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
        let result_true = gt(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_false = gt(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_false_2 = gt(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_ge() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_true = ge(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_false = ge(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_true_2 = ge(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(result_true_2);
    }

    #[test]
    fn test_lt() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_false = lt(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_true = lt(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_false_2 = lt(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        assert!(result_true);
        assert!(!result_false);
        assert!(!result_false_2);
    }

    #[test]
    fn test_le() {
        let env = RefLEnv::default();
        let ctx = CtxMath::default();
        let result_false = le(&[3.into(), 2.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_true = le(&[2.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
            .unwrap();
        let result_true_2 = le(&[3.into(), 3.into()], &env, &ctx)
            .unwrap()
            .as_bool()
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
