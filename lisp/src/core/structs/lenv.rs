use crate::core::root_module::CtxRoot;
use crate::core::structs::contextcollection::ContextCollection;
use crate::core::structs::documentation::{Documentation, LHelp};
use crate::core::structs::lcoreoperator::language::*;
use crate::core::structs::lenv::language::*;
use crate::core::structs::lerror;
use crate::core::structs::lerror::LError::{WrongNumberOfArgument, WrongType};
use crate::core::structs::lerror::LResult;
use crate::core::structs::llambda::LLambda;
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::IntoModule;
use crate::core::structs::new_function::*;
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::core::structs::typelvalue::TypeLValue;
use crate::core::{eval, parse};
use im::HashSet;
use std::any::Any;
use std::fmt::{Display, Formatter};
use std::ops::Deref;
use std::sync::Arc;

pub mod language {

    pub const ENV_GET_KEYS: &str = "get_keys"; //return a list of keys of the environment
    pub const ENV_GET_MACROS: &str = "get_macros";
    pub const ENV_GET_MACRO: &str = "get_macro";
    pub const ENV_GET_LIST_MODULES: &str = "get_list_modules";

    pub const HELP: &str = "help";
    pub const DOC_HELP: &str =
        "Give a list of all the available functions added by the modules and available in the core.";
    pub const DOC_HELP_VERBOSE: &str = "takes 0..1 arguments:\
                                -no argument: give the list of all the functions\n\
                                -1 argument: give the documentation of the function.";
}

#[derive(Default, Clone, Debug)]
pub struct LEnvSymbols {
    inner: im::HashMap<String, LValue>,
    outer: Arc<Option<LEnvSymbols>>,
}

impl LEnvSymbols {
    pub fn set_outer(&mut self, outer: LEnvSymbols) {
        self.outer = Arc::new(Some(outer))
    }

    pub fn insert(&mut self, label: String, lv: LValue) {
        self.inner = self.inner.update(label, lv);
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
#[derive(Clone, Debug)]
pub struct LEnv {
    symbols: LEnvSymbols,
    macro_table: im::HashMap<String, LLambda>,
    ctxs: ContextCollection,
    pfc: PureFonctionCollection,
    documentation: Documentation,
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
}

impl LEnv {
    pub fn get_context<T: Any + Send + Sync>(&self, label: &str) -> lerror::Result<&T> {
        self.ctxs.get::<T>(label)
    }
}

impl Default for LEnv {
    fn default() -> Self {
        let mut symbols: LEnvSymbols = Default::default();
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

        symbols.insert(HELP.to_string(), LFn::new(help, HELP.to_string()).into());

        symbols.insert(
            ENV_GET_LIST_MODULES.to_string(),
            LFn::new(get_list_modules, ENV_GET_LIST_MODULES.to_string()).into(),
        );

        symbols.insert(
            ENV_GET_KEYS.to_string(),
            LFn::new(env_get_keys, ENV_GET_KEYS.to_string()).into(),
        );
        symbols.insert(
            ENV_GET_MACROS.to_string(),
            LFn::new(env_get_macros, ENV_GET_MACROS.to_string()).into(),
        );
        symbols.insert(
            ENV_GET_MACRO.to_string(),
            LFn::new(env_get_macro, ENV_GET_MACRO.to_string()).into(),
        );

        Self {
            symbols,
            macro_table: Default::default(),
            ctxs: Default::default(),
            pfc: Default::default(),
            documentation,
        }
    }
}

impl LEnv {
    /*pub fn merge_by_symbols(&mut self, other: &Self) {
        self.symbols = self.symbols.clone().union(other.symbols.clone());
    }*/
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

    pub fn insert(&mut self, key: String, exp: LValue) {
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

    pub fn get_macro(&self, key: &str) -> Option<&LLambda> {
        self.macro_table.get(key)
    }

    pub fn keys(&self) -> im::HashSet<String> {
        self.symbols.keys()
    }

    pub fn macros(&self) -> HashSet<String> {
        self.macro_table.keys().cloned().collect()
    }

    pub async fn import(
        &mut self,
        ctx: impl IntoModule,
        import_type: ImportType,
    ) -> lerror::Result<()> {
        self.add_documentation(ctx.documentation());
        self.add_pure_functions(ctx.pure_fonctions());

        let mut module = ctx.into_module();
        self.ctxs.insert(module.ctx, module.label.clone());
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
        writeln!(f, "{}", self.symbols)
    }
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
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
        return Err(WrongNumberOfArgument(
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
        Err(WrongType(
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
    let documentation: Documentation = env.get_documentation();

    match args.len() {
        0 => Ok(documentation.get_all().into()),
        1 => match &args[0] {
            LValue::Fn(fun) => Ok(LValue::String(documentation.get(fun.get_label()))),
            LValue::Symbol(s) => Ok(LValue::String(documentation.get(s))),
            LValue::CoreOperator(co) => Ok(LValue::String(documentation.get(&co.to_string()))),
            lv => Err(WrongType(HELP, lv.clone(), lv.into(), TypeLValue::Symbol)),
        },
        _ => Err(WrongNumberOfArgument(HELP, args.into(), args.len(), 0..1)),
    }
}

pub fn get_list_modules(_: &[LValue], env: &LEnv) -> LResult {
    let list = env.ctxs.get_list_modules();
    let mut str = '{'.to_string();
    for (i, s) in list.iter().enumerate() {
        if i != 0 {
            str.push(',')
        }
        str.push_str(s)
    }

    str.push(')');

    Ok(LValue::String(str))
}
