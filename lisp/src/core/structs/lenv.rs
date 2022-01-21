use crate::core::root_module::language::SET;
use crate::core::root_module::CtxRoot;
use crate::core::structs::contextcollection::ContextCollection;
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lcoreoperator::language::*;
use crate::core::structs::lenv::language::*;
use crate::core::structs::lerror::LError::{UndefinedSymbol, WrongNumberOfArgument, WrongType};
use crate::core::structs::lerror::{LError, LResult};
use crate::core::structs::llambda::LLambda;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::IntoModule;
use crate::core::structs::new_function::*;
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;
use crate::core::{eval, parse};
use anyhow::{anyhow, bail};
use im::HashSet;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::sync::Arc;
use tokio::sync::RwLock;

pub mod language {

    pub const ENV_GET_KEYS: &str = "env.get_keys"; //return a list of keys of the environment
    pub const ENV_GET_MACROS: &str = "env.get_macros";
    pub const ENV_GET_MACRO: &str = "env.get_macro";

    pub const HELP: &str = "help";
    pub const DOC_HELP: &str =
        "Give a list of all the available functions added by the modules and available in the core.";
    pub const DOC_HELP_VERBOSE: &str = "takes 0..1 arguments:\
                                -no argument: give the list of all the functions\n\
                                -1 argument: give the documentation of the function.";
}

/// Structs used to store the Scheme Environment
/// - It contains a mapping of <symbol(String), LValue>
/// - It also contains macros, special LLambdas used to format LValue expressions.
/// - A LEnv can inherits from an outer environment. It can use symbols from it, but not modify them.
#[derive(Clone, Debug)]
pub struct InnerLEnv {
    symbols: im::HashMap<String, LValue>,
    macro_table: im::HashMap<String, LLambda>,
    ctxs: Arc<RwLock<ContextCollection>>,
    pfc: Arc<RwLock<PureFonctionCollection>>,
    documentation: Arc<RwLock<Documentation>>,
    outer: Option<Arc<LEnv>>,
}

impl InnerLEnv {
    pub async fn get_documentation(&self) -> Documentation {
        self.documentation.read().await.clone()
    }

    pub async fn add_documentation(&mut self, doc: Documentation) {
        self.documentation.write().await.append(doc)
    }

    pub async fn add_pure_functions(&mut self, pfc: PureFonctionCollection) {
        self.pfc.write().await.append(pfc);
    }
}

#[derive(Clone, Debug)]
pub struct LEnv {
    inner: Arc<InnerLEnv>,
}

impl LEnv {
    pub fn get_context<T>(&self, label: &str) -> Result<&T, anyhow::Error> {
        self.inner.ctxs.read().get::<T>(label)
    }

    pub async fn get_mut_context<T>(&self, label: &str) -> Result<&mut T, anyhow::Error> {
        self.inner.ctxs.read().get_mut::<T>(label)
    }

    pub async fn get_documentation(&self) -> Documentation {
        self.inner.get_documentation().await
    }

    pub async fn add_documentation(&self, doc: Documentation) {
        self.inneradd_documentation(doc).await
    }

    pub async fn add_pure_functions(&self, pfc: PureFonctionCollection) {
        self.inner.add_pure_fonctions(pfc).await
    }
}

impl From<InnerLEnv> for LEnv {
    fn from(inner_lenv: InnerLEnv) -> Self {
        Self {
            inner: Arc::new(inner_lenv),
        }
    }
}

impl Default for LEnv {
    fn default() -> Self {
        let mut symbols = HashMap::default();
        let documentation = vec![
            LHelp::new_verbose(HELP, DOC_HELP, DOC_HELP_VERBOSE),
            LHelp::new(DEFINE, DOC_DEFINE),
            LHelp::new_verbose(LAMBDA, DOC_LAMBDA, DOC_LAMBDA_VEBROSE),
            LHelp::new(DEF_MACRO, DOC_DEF_MACRO),
            LHelp::new(IF, DOC_IF),
            LHelp::new(QUOTE, DOC_QUOTE),
            LHelp::new(QUASI_QUOTE, QUASI_QUOTE),
            LHelp::new(UNQUOTE, DOC_UNQUOTE),
            LHelp::new_verbose(BEGIN, DOC_BEGIN, DOC_BEGIN_VERBOSE),
            LHelp::new(AWAIT, DOC_AWAIT),
            LHelp::new(ASYNC, DOC_ASYNC),
            LHelp::new(EVAL, DOC_EVAL),
        ]
        .into();
        let pfc = PureFonctionCollection::default();

        symbols.insert(HELP, LFn::new(help, HELP.to_string()));

        symbols.insert(ENV_GET_KEYS, LFn::new(env_get_keys, ENV_GET_KEYS));
        symbols.insert(ENV_GET_MACROS, LFn::new(env_get_macros, ENV_GET_MACROS));
        symbols.insert(ENV_GET_MACRO, LFn::new(env_get_macro, ENV_GET_MACRO));

        Self {
            inner: Arc::new(InnerLEnv {
                symbols: Default::default(),
                macro_table: Default::default(),
                ctxs: Arc::new(Default::default()),
                pfc: Arc::new(Default::default()),
                documentation: Arc::new(documentation),
                outer: None,
            }),
        }
    }
}

impl LEnv {
    pub fn merge_by_symbols(&mut self, other: &Self) {
        self.symbols = self.symbols.clone().union(other.symbols.clone());
    }
    /// Returns the env with all the basic functions, the ContextCollection with CtxRoot
    /// and InitialLisp containing the definition of macros and lambdas,
    pub async fn root() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut env = LEnv::default();
        env.import(CtxRoot::default(), ImportType::WithoutPrefix)
            .await
            .expect("error while loading module root");
        env
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

    pub async fn import(
        &mut self,
        ctx: impl IntoModule,
        import_type: ImportType,
    ) -> Result<(), LError> {
        self.add_documentation(ctx.documentation());
        self.add_pure_functions(ctx.pure_fonctions());

        let mut module = ctx.into_module();
        self.inner.ctxs.insert(module.ctx, module.label.clone());
        //println!("id: {}", id);
        for (sym, lv) in &mut module.prelude {
            match import_type {
                ImportType::WithPrefix => {
                    self.insert(format!("{}::{}", module.label, sym.to_string()), lv.clone());
                }
                ImportType::WithoutPrefix => {
                    self.insert(sym.to_string(), lv.clone());
                }
            }
        }

        for element in module.raw_lisp.inner() {
            let lvalue = parse(element, self).await?;

            if lvalue != LValue::Nil {
                eval(&lvalue, self).await?;
            }
        }
        Ok(())
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

/// Returns a list of all the keys present in the environment
pub fn env_get_keys(_: &[LValue], env: &LEnv) -> LResult {
    Ok(env
        .keys()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macros(_: &[LValue], env: &LEnv) -> LResult {
    Ok(env
        .macros()
        .iter()
        .map(|x| LValue::from(x.clone()))
        .collect::<Vec<LValue>>()
        .into())
}

pub fn env_get_macro(args: &[LValue], env: &LEnv) -> LResult {
    if args.len() != 1 {
        return bail!(WrongNumberOfArgument(
            ENV_GET_MACRO,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    if let LValue::Symbol(s) = &args[0] {
        Ok(match env.get_macro(s).cloned() {
            Some(l) => l.into(),
            None => LValue::Nil,
        })
    } else {
        bail!(WrongType(
            ENV_GET_MACRO,
            args[0].clone(),
            (&args[0]).into(),
            TypeLValue::Symbol,
        ))
    }
}

///print the help
/// Takes 0 or 1 parameter.
/// 0 parameter: gives the list of all the functions
/// 1 parameter: write the help of
pub fn help(args: &[LValue], env: &LEnv) -> LResult {
    let documentation: Arc<Documentation> = env.get_documentation()?;

    match args.len() {
        0 => Ok(documentation.get_all().into()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(LValue::String(documentation.get(fun.get_label()))),
            LValue::MutFn(fun) => Ok(LValue::String(documentation.get(fun.get_label()))),
            LValue::Symbol(s) => Ok(LValue::String(documentation.get(s))),
            LValue::CoreOperator(co) => Ok(LValue::String(documentation.get(&co.to_string()))),
            lv => bail!(WrongType(HELP, lv.clone(), lv.into(), TypeLValue::Symbol)),
        },
        _ => bail!(WrongNumberOfArgument(HELP, args.into(), args.len(), 0..1)),
    }
}
