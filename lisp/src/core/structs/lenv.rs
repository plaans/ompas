use crate::core::root_module::language::SET;
use crate::core::root_module::CtxRoot;
use crate::core::structs::contextcollection::ContextCollection;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::UndefinedSymbol;
use crate::core::structs::llambda::LLambda;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::GetModule;
use crate::core::{eval, parse};
use im::HashSet;
use std::fmt::{Display, Formatter};

/// Structs used to store the Scheme Environment
/// - It contains a mapping of <symbol(String), LValue>
/// - It also contains macros, special LLambdas used to format LValue expressions.
/// - A LEnv can inherits from an outer environment. It can use symbols from it, but not modify them.
#[derive(Clone, Debug)]
pub struct LEnv {
    symbols: im::HashMap<String, LValue>,
    macro_table: im::HashMap<String, LLambda>,
    //pub(crate) new_entries: Vec<String>, Used to export new entries, but not really important in the end
    outer: Option<Box<LEnv>>,
    //task_handler: TaskHandler
}

impl LEnv {
    pub fn merge_by_symbols(&mut self, other: &Self) {
        self.symbols = self.symbols.clone().union(other.symbols.clone());
    }
    /// Returns the env with all the basic functions, the ContextCollection with CtxRoot
    /// and InitialLisp containing the definition of macros and lambdas,
    pub async fn root() -> (Self, ContextCollection) {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut env = LEnv::default();
        let mut ctxs = ContextCollection::default();
        import(
            &mut env,
            &mut ctxs,
            CtxRoot::default(),
            ImportType::WithoutPrefix,
        )
        .await
        .expect("error while loading module root");
        (env, ctxs)
    }

    pub fn empty() -> Self {
        LEnv {
            symbols: Default::default(),
            macro_table: Default::default(),
            //new_entries: vec![],
            outer: None,
        }
    }

    pub fn set_outer(&mut self, outer: Self) {
        self.outer = Some(Box::new(outer));
    }

    pub fn get_symbol(&self, s: &str) -> Option<LValue> {
        match self.symbols.get(s) {
            None => match &self.outer {
                None => None,
                Some(outer) => outer.get_symbol(s),
            },
            Some(s) => Some(s.clone()),
        }
    }

    pub fn get_ref_symbol(&self, s: &str) -> Option<&LValue> {
        match self.symbols.get(s) {
            None => match &self.outer {
                None => None,
                Some(outer) => outer.get_ref_symbol(s),
            },
            Some(s) => Some(s),
        }
    }

    pub fn insert(&mut self, key: String, exp: LValue) {
        self.symbols = self.symbols.update(key, exp);
    }

    pub fn update(&self, key: String, exp: LValue) -> Self {
        let mut update = self.clone();
        update.symbols.insert(key, exp);
        update
    }

    pub fn set(&mut self, key: String, exp: LValue) -> Result<(), LError> {
        match self.get_symbol(key.as_str()) {
            None => Err(UndefinedSymbol(SET, key)),
            Some(_) => {
                self.symbols.insert(key, exp);
                Ok(())
            }
        }
    }

    pub fn add_macro(&mut self, key: String, _macro: LLambda) {
        self.macro_table = self.macro_table.update(key, _macro)
    }

    pub fn get_macro(&self, key: &str) -> Option<&LLambda> {
        self.macro_table.get(key)
    }

    pub fn keys(&self) -> im::HashSet<String> {
        let mut keys: HashSet<String> = self.symbols.keys().cloned().collect();
        keys = keys.union(self.macro_table.keys().cloned().collect());
        if let Some(outer) = &self.outer {
            keys = keys.union(outer.keys());
        }
        keys
    }

    pub fn macros(&self) -> HashSet<String> {
        let mut macros: HashSet<String> = self.macro_table.keys().cloned().collect();
        if let Some(outer) = &self.outer {
            macros = macros.union(outer.macros());
        }
        macros
    }
}

impl Display for LEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "".to_string();
        for s in &self.symbols {
            str.push_str(format!("{}: {}\n", s.0, s.1).as_str())
        }
        if let Some(outer) = &self.outer {
            str.push_str(outer.to_string().as_str());
        }
        writeln!(f, "{}", str)
    }
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Default for LEnv {
    fn default() -> Self {
        Self::empty()
    }
}

#[derive(Debug)]
pub enum ImportType {
    WithPrefix,
    WithoutPrefix,
}

pub async fn import(
    env: &mut LEnv,
    ctxs: &mut ContextCollection,
    ctx: impl GetModule,
    import_type: ImportType,
) -> Result<(), LError> {
    let mut module = ctx.get_module();
    let id = ctxs.insert(module.ctx, module.label.clone());
    //println!("id: {}", id);
    for (sym, lv) in &mut module.prelude {
        match lv {
            LValue::Fn(fun) => fun.set_index_mod(id),
            LValue::MutFn(fun) => fun.set_index_mod(id),
            LValue::AsyncFn(fun) => fun.set_index_mod(id),
            LValue::AsyncMutFn(fun) => fun.set_index_mod(id),
            _ => {}
        }
        match import_type {
            ImportType::WithPrefix => {
                env.insert(format!("{}::{}", module.label, sym.to_string()), lv.clone());
            }
            ImportType::WithoutPrefix => {
                env.insert(sym.to_string(), lv.clone());
            }
        }
    }

    for element in module.raw_lisp.inner() {
        let lvalue = parse(element, env, ctxs).await?;

        if lvalue != LValue::Nil {
            eval(&lvalue, env, ctxs).await?;
        }
    }
    Ok(())
}

/*
#[derive(Clone)]
pub struct RefLEnv(Rc<LEnv>);

impl Default for RefLEnv {
    fn default() -> Self {
        RefLEnv(Rc::new(LEnv::default()))
    }
}

impl From<LEnv> for RefLEnv {
    fn from(e: LEnv) -> Self {
        RefLEnv(Rc::new(e))
    }
}

impl RefLEnv {
    pub fn clone_from_root(&self) -> LEnv {
        let mut env = self.0.deref().clone();
        let outer = env.outer.clone();
        match outer {
            None => {}
            Some(s) => env.merge_by_symbols(&s.clone().clone_from_root()),
        };
        env.outer = None;
        env.into()
    }
}

impl RefLEnv {
    pub fn keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.symbols.keys().cloned().collect();
        keys.append(&mut self.macro_table.keys().cloned().collect());
        if let Some(outer) = self.outer.clone() {
            keys.append(&mut outer.keys())
        }
        keys
    }

    pub fn root() -> Self {
        RefLEnv(Rc::new(LEnv::root()))
    }

    pub fn new(env: LEnv) -> Self {
        RefLEnv(Rc::new(env))
    }

    pub fn new_from_outer(outer: RefLEnv) -> Self {
        RefLEnv(Rc::new(LEnv {
            symbols: Default::default(),
            macro_table: Default::default(),
            //new_entries: vec![],
            //outer: Some(outer),
        }))
    }

    pub fn empty() -> Self {
        RefLEnv(Rc::new(LEnv::empty()))
    }
}

impl Deref for RefLEnv {
    type Target = LEnv;

    fn deref(&self) -> &Self::Target {
        &(self.0)
    }
}

impl DerefMut for RefLEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Rc::get_mut(&mut self.0).unwrap()
    }
}*/
