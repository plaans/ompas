use crate::contextcollection::{Context, ContextCollection};
use crate::documentation::Documentation;
use crate::llambda::LLambda;
use crate::lruntimeerror;
use crate::lvalue::LValue;
use crate::module::{InitLisp, IntoModule};
use crate::purefonction::PureFonctionCollection;
use im::HashSet;
use std::any::Any;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

#[derive(Default, Clone, Debug)]
pub struct LEnvSymbols {
    inner: im::HashMap<String, LValue>,
    outer: Arc<Option<LEnvSymbols>>,
}

impl LEnvSymbols {
    pub fn set_outer(&mut self, outer: LEnvSymbols) {
        self.outer = Arc::new(Some(outer))
    }

    pub fn insert(&mut self, label: impl Into<String>, lv: LValue) {
        self.inner = self.inner.update(label.into(), lv);
    }
    pub fn get(&self, label: &str) -> Option<LValue> {
        match self.inner.get(label) {
            Some(lv) => Some(lv.clone()),
            None => match self.outer.deref() {
                None => None,
                Some(outer) => outer.get(label),
            },
        }
    }
    pub fn get_ref(&self, label: &str) -> Option<&LValue> {
        match self.inner.get(label) {
            Some(lv) => Some(lv),
            None => match &self.outer.deref() {
                None => None,
                Some(outer) => outer.get_ref(label),
            },
        }
    }

    pub fn keys(&self) -> HashSet<String> {
        let mut keys: HashSet<String> = self.inner.keys().cloned().collect();
        if let Some(outer) = &*self.outer {
            keys = keys.union(outer.keys());
        }
        keys
    }
}

impl Display for LEnvSymbols {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "".to_string();
        for s in &self.inner {
            str.push_str(format!("{}: {}\n", s.0, s.1).as_str())
        }
        writeln!(f, "{}", str)
    }
}

/// Structs used to store the Scheme Environment
/// - It contains a mapping of <symbol(String), LValue>
/// - It also contains macros, special LLambdas used to format LValue expressions.
/// - A LEnv can inherits from an outer environment. It can use symbols from it, but not modify them.
#[derive(Clone, Debug, Default)]
pub struct LEnv {
    symbols: LEnvSymbols,
    macro_table: im::HashMap<String, LLambda>,
    ctxs: ContextCollection,
    pfc: PureFonctionCollection,
    documentation: Documentation,
    init: InitLisp,
}

impl LEnv {
    /*pub fn set_outer(&mut self, env: LEnv) {
        self.outer = Some(Arc::new(env))
    }*/
    pub fn get_documentation(&self) -> Documentation {
        self.documentation.clone()
    }

    pub fn add_documentation(&mut self, doc: Documentation) {
        self.documentation.append(doc)
    }

    pub fn add_pure_functions(&mut self, pfc: PureFonctionCollection) {
        self.pfc.append(pfc);
    }

    pub fn get_pfc(&self) -> &PureFonctionCollection {
        &self.pfc
    }

    pub fn get_init(&self) -> &InitLisp {
        &self.init
    }
}

/*
Context collection methods
 */
impl LEnv {
    /*
    Return the reference of a context
     */
    pub fn get_context<T: Any + Send + Sync>(&self, label: &str) -> lruntimeerror::Result<&T> {
        self.ctxs.get::<T>(label)
    }

    pub fn add_context(&mut self, ctx: Context, label: String) {
        self.ctxs.insert(ctx, label);
    }
}

#[derive(Debug, Copy, Clone)]
pub enum ImportType {
    WithPrefix,
    WithoutPrefix,
}

impl LEnv {
    /// Returns the env with all the basic functions, the ContextCollection with CtxRoot
    /// and InitialLisp containing the definition of macros and lambdas,
    pub fn import(&mut self, ctx: impl IntoModule, import_type: ImportType) {
        self.add_documentation(ctx.documentation());
        self.add_pure_functions(ctx.pure_fonctions());

        let mut module = ctx.into_module();
        self.add_context(module.ctx, module.label.clone());
        //println!("id: {}", id);
        for (sym, lv) in &mut module.prelude {
            match import_type {
                ImportType::WithPrefix => {
                    self.insert(format!("{}::{}", module.label, sym), lv.clone());
                }
                ImportType::WithoutPrefix => {
                    self.insert(sym.to_string(), lv.clone());
                }
            }
        }

        self.init.append(&mut module.raw_lisp);
    }

    pub fn get_symbols(&self) -> LEnvSymbols {
        self.symbols.clone()
    }

    pub fn set_new_top_symbols(&mut self, mut symbols: LEnvSymbols) {
        symbols.outer = Arc::new(Some(self.symbols.clone()));
        self.symbols = symbols;
    }

    pub fn get_symbol(&self, s: &str) -> Option<LValue> {
        self.symbols.get(s)
    }

    pub fn get_ref_symbol(&self, s: &str) -> Option<&LValue> {
        self.symbols.get_ref(s)
    }

    pub fn insert(&mut self, key: impl Into<String>, exp: LValue) {
        self.symbols.insert(key, exp);
    }

    pub fn update(&self, key: String, exp: LValue) -> Self {
        let mut update = self.clone();
        update.symbols.insert(key, exp);
        update
    }

    pub fn add_macro(&mut self, key: String, _macro: LLambda) {
        self.macro_table.insert(key, _macro);
    }

    pub fn get_list_modules(&self) -> Vec<String> {
        self.ctxs.get_list_modules()
    }

    pub fn get_macro(&self, key: &str) -> Option<&LLambda> {
        self.macro_table.get(key)
    }

    pub fn keys(&self) -> im::HashSet<String> {
        self.symbols.keys()
    }

    pub fn macros(&self) -> HashSet<String> {
        self.macro_table.keys().cloned().collect()
    }
}

impl Display for LEnv {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "{}", self.symbols)
    }
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

/*lfn! { env_get_keys(_,env) {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}
}*/
