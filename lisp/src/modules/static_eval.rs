use crate::core::root_module::CtxRoot;
use crate::core::structs::contextcollection::Context;
use crate::core::structs::documentation::Documentation;
use crate::core::structs::lenv::ImportType::{WithPrefix, WithoutPrefix};
use crate::core::structs::lenv::{ImportType, LEnv};
use crate::core::structs::lerror;
use crate::core::structs::lerror::LError::WrongNumberOfArgument;
use crate::core::structs::lerror::{LError, LResult};
use crate::core::structs::lvalue::LValue;
use crate::core::structs::module::{IntoModule, Module};
use crate::core::structs::purefonction::PureFonctionCollection;
use crate::modules::advanced_math::CtxMath;
use crate::modules::error::CtxError;
use crate::modules::static_eval::language::*;
use crate::modules::utils::CtxUtils;
use crate::static_eval::{eval_static, expand_static};
use ::macro_rules_attribute::macro_rules_attribute;
use ompas_utils::dyn_async;
use std::sync::Arc;
use tokio::sync::RwLock;

/*
LANGUAGE
 */

pub mod language {

    pub const MOD_STATIC_EVAL: &str = "mod-static-eval";
    pub const EVAL_STATIC: &str = "eval-static";
}

#[derive(Default, Clone)]
pub struct CtxStaticEval {
    env: Arc<RwLock<LEnv>>,
}

impl CtxStaticEval {
    pub async fn get_env(&self) -> LEnv {
        self.env.read().await.clone()
    }

    pub async fn set_env(&self, env: LEnv) {
        *self.env.write().await = env;
    }
}

///Import basic scheme modules.
/// Other modules can be imported via 'import' and 'import_namespace' functions
impl CtxStaticEval {
    pub async fn new() -> Result<Self, LError> {
        let mut env = LEnv::default();

        env.import(CtxRoot::default(), ImportType::WithoutPrefix)
            .await?;
        env.import(CtxMath::default(), ImportType::WithoutPrefix)
            .await?;
        env.import(CtxUtils::default(), ImportType::WithoutPrefix)
            .await?;
        env.import(CtxError::default(), ImportType::WithoutPrefix)
            .await?;
        let env = Arc::new(RwLock::new(env));
        Ok(Self { env })
    }
}

impl CtxStaticEval {
    pub async fn import_namespace(&mut self, ctx: impl IntoModule) -> lerror::Result<()> {
        self.env.write().await.import(ctx, WithoutPrefix).await
    }

    pub async fn import(&mut self, ctx: impl IntoModule) -> lerror::Result<()> {
        self.env.write().await.import(ctx, WithPrefix).await
    }
}

impl IntoModule for CtxStaticEval {
    fn into_module(self) -> Module {
        let mut module = Module {
            ctx: Context::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: MOD_STATIC_EVAL.to_string(),
        };

        module.add_async_fn_prelude(EVAL_STATIC, scheme_eval_static);

        module
    }

    fn documentation(&self) -> Documentation {
        Default::default()
    }

    fn pure_fonctions(&self) -> PureFonctionCollection {
        Default::default()
    }
}

#[macro_rules_attribute(dyn_async!)]
pub async fn scheme_eval_static<'a>(args: &'a [LValue], env: &'a LEnv) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            EVAL_STATIC,
            args.into(),
            args.len(),
            1..1,
        ));
    }
    let ctx = env.get_context::<CtxStaticEval>(MOD_STATIC_EVAL)?;

    let mut env = ctx.get_env().await;

    let result = expand_static(&args[0], true, &mut env)?;

    let result = eval_static(result.get_lvalue(), &mut env)?;

    ctx.set_env(env).await;

    println!("static evaluation returned: {}", result.get_lvalue());

    Ok(result.get_lvalue().clone())
}
