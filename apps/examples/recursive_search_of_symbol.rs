use ompas_lisp::core::structs::lenv::{LEnv, LEnvSymbols};
use ompas_lisp::core::structs::lvalue::LValue;

fn main() {
    println!("test lenv");

    let mut env1 = LEnvSymbols::default();
    env1.insert("val1".to_string(), LValue::Character('a'));
    let mut env2 = LEnvSymbols::default();
    env2.insert("val2".to_string(), LValue::Character('b'));
    env2.set_outer(env1);
    let mut env3 = LEnvSymbols::default();
    env3.insert("val3".to_string(), LValue::Character('c'));
    env3.set_outer(env2);
    println!("env:\n{}", env3);

    let val1 = env3.get("val1");
    println!("{:?}", val1);
}
