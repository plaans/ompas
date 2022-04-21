use crate::advanced_math::CtxMath;
use crate::utils::CtxUtils;
use sompas_core::static_eval::{eval_static, expand_static};
use sompas_core::{eval_init, get_root_env};
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::documentation::Documentation;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_structs::module::{IntoModule, Module};
use sompas_structs::purefonction::PureFonctionCollection;
use std::sync::Arc;
use tokio::sync::RwLock;
/*
LANGUAGE
 */

pub const MOD_STATIC_EVAL: &str = "mod-static-eval";
pub const EVAL_STATIC: &str = "eval-static";

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
    pub async fn new() -> Self {
        let mut env = get_root_env().await;

        env.import(CtxMath::default(), ImportType::WithoutPrefix);
        env.import(CtxUtils::default(), ImportType::WithoutPrefix);
        eval_init(&mut env).await;
        let env = Arc::new(RwLock::new(env));
        Self { env }
    }
}

impl CtxStaticEval {
    pub async fn import_namespace(&mut self, ctx: impl IntoModule) {
        self.env
            .write()
            .await
            .import(ctx, ImportType::WithoutPrefix)
    }

    pub async fn import(&mut self, ctx: impl IntoModule) {
        self.env.write().await.import(ctx, ImportType::WithPrefix)
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

#[async_scheme_fn]
pub async fn scheme_eval_static<'a>(env: &'a LEnv, lv: &LValue) -> LResult {
    let ctx = env.get_context::<CtxStaticEval>(MOD_STATIC_EVAL)?;

    let mut env = ctx.get_env().await;

    let result = expand_static(&lv, true, &mut env)?;

    let result = eval_static(result.get_lvalue(), &mut env)?;

    println!("static evaluation returned: {}", result.get_lvalue());

    Ok(result.get_lvalue().clone())
}
