use im::HashMap;
use ompas_lisp::core::{load_module, ContextCollection, LEnv};
use ompas_lisp::structs::{InitLisp, LValue, LError};
use ompas_modules::math::CtxMath;
use std::sync::Arc;
use std::sync::atomic::{AtomicUsize, Ordering};

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

/*
#[derive(Default, Debug)]
pub struct TaskHandler {
    inner: HashMap<usize, JoinHandle<Result<LValue,LError>>>,
    next_id:  Arc<AtomicUsize>
}

//TODO: use tokio::RwLock
impl TaskHandler {
    pub fn add_handler(&mut self, handler: JoinHandle<Result<LValue, LError>>) -> usize {
        let id = self.next_id.load(Ordering::Relaxed);
        self.next_id.store(id+1, Ordering::Relaxed); //increment the id
        self.inner.insert(id, handler);

        id
    }

    pub fn get_ref_handler(&self, id: &usize) -> &JoinHandle<Result<LValue, LError>> {
        match self.inner.get(id) {
            None => panic!("id corresponds to no handler"),
            Some(j) => j
        }
    }

    pub fn get_handler(&mut self, id: &usize) -> JoinHandle<Result<LValue, LError>> {
        match self.inner.remove(id) {
            None => panic!("id corresponds to no handler"),
            Some(h) => h
        }
    }
}*/