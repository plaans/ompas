use crate::advanced_math::ModMath;
use crate::utils::ModUtils;
use sompas_core::static_eval::{eval_static, expand_static};
use sompas_core::{eval_init, get_root_env};
use sompas_language::eval_static::*;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;
use tokio::sync::RwLock;
/*
LANGUAGE
 */

#[derive(Default, Clone)]
pub struct ModStaticEval {
    env: Arc<RwLock<LEnv>>,
}

impl ModStaticEval {
    pub async fn get_env(&self) -> LEnv {
        self.env.read().await.clone()
    }

    pub async fn set_env(&self, env: LEnv) {
        *self.env.write().await = env;
    }
}

///Import basic scheme modules.
/// Other modules can be imported via 'import' and 'import_namespace' functions
impl ModStaticEval {
    pub async fn new() -> Self {
        let mut env = get_root_env().await;

        env.import_module(ModMath::default(), ImportType::WithoutPrefix);
        env.import_module(ModUtils::default(), ImportType::WithoutPrefix);
        eval_init(&mut env).await;
        let env = Arc::new(RwLock::new(env));
        Self { env }
    }

    pub async fn import_namespace(&mut self, ctx: impl Into<LModule>) {
        self.env
            .write()
            .await
            .import_module(ctx, ImportType::WithoutPrefix)
    }

    pub async fn import(&mut self, ctx: impl Into<LModule>) {
        self.env
            .write()
            .await
            .import_module(ctx, ImportType::WithPrefix)
    }
}

impl From<ModStaticEval> for LModule {
    fn from(m: ModStaticEval) -> Self {
        let mut module = LModule::new(m, MOD_EVAL_STATIC, DOC_MOD_EVAL_STATIC);
        module.add_async_fn(EVAL_STATIC, scheme_eval_static, DOC_EVAL_STATIC, true);
        module
    }
}

#[async_scheme_fn]
pub async fn scheme_eval_static<'a>(env: &'a LEnv, lv: &LValue) -> LResult {
    let ctx = env.get_context::<ModStaticEval>(MOD_EVAL_STATIC)?;

    let mut env = ctx.get_env().await;

    let result = expand_static(lv, true, &mut env)?;

    let result = eval_static(result.get_lvalue(), &mut env)?;

    println!("static evaluation returned: {}", result.get_lvalue());

    Ok(result.get_lvalue().clone())
}
