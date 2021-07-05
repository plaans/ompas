use im::HashMap;
use ompas_lisp::core::{load_module, ContextCollection, LEnv};
use ompas_lisp::structs::InitLisp;
use ompas_modules::math::CtxMath;

#[tokio::main]
async fn main() {
    println!("hello world!");

    let ctx_math = CtxMath::default();
    let mut env = LEnv::default();
    //let mut second_env = env.clone();

    let mut ctxs = ContextCollection::default();
    //let mut new_ctxs = ctxs.clone();

    tokio::spawn(async move {
        let mut init_lisp = InitLisp::default();
        load_module(&mut env, &mut ctxs, ctx_math, &mut init_lisp)
    });

    test_hashmap();
}

pub fn test_hashmap() {
    let mut map = HashMap::new();
    let result = map.insert(10, "ten");
    println!("{:?}", result);
    let ancient_value = map.insert(10, "dix");
    println!("{:?}", ancient_value);
}
