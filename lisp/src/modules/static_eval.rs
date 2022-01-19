use crate::core::ImportType::{WithPrefix, WithoutPrefix};
use crate::core::{import, ContextCollection, CtxRoot, ImportType, LEnv};
use crate::modules::doc::{Documentation, LHelp};
use crate::modules::error::CtxError;
use crate::modules::math::CtxMath;
use crate::modules::utils::CtxUtils;
use crate::static_eval::{eval_static, PLValue, PureFonction, PureFonctionCollection};
use crate::structs::LError::WrongNumberOfArgument;
use crate::structs::{GetModule, LError, LResult, LValue, Module};
use ompas_utils::blocking_async;
use std::sync::Arc;

/*
LANGUAGE
 */

pub const EVAL_STATIC: &str = "eval-static";

#[derive(Default, Clone)]
pub struct CtxStaticEval {
    env: LEnv,
    ctxs: ContextCollection,
    pfc: PureFonctionCollection,
}

///Import basic scheme modules.
/// Other modules can be imported via 'import' and 'import_namespace' functions
impl CtxStaticEval {
    pub async fn new() -> Self {
        let mut env = LEnv::default();
        let mut ctxs = ContextCollection::default();
        let mut pfc = PureFonctionCollection::default();

        //root
        let ctx_root = CtxRoot::default();
        pfc.append(ctx_root.get_pure_fonctions_symbols());
        //math
        let ctx_math = CtxMath::default();
        pfc.append(ctx_math.get_pure_fonctions_symbols());
        //utils
        let ctx_utils = CtxUtils::default();
        pfc.append(ctx_utils.get_pure_fonctions_symbols());
        //error
        let ctx_error = CtxError::default();
        pfc.append(ctx_error.get_pure_fonctions_symbols());

        import(&mut env, &mut ctxs, ctx_root, ImportType::WithoutPrefix).await;
        import(&mut env, &mut ctxs, ctx_math, ImportType::WithoutPrefix).await;
        import(&mut env, &mut ctxs, ctx_utils, ImportType::WithoutPrefix).await;
        import(&mut env, &mut ctxs, ctx_error, ImportType::WithoutPrefix).await;
        Self { env, ctxs, pfc }
    }
}

impl CtxStaticEval {
    pub async fn import_namespace(
        &mut self,
        ctx: impl GetModule + PureFonction,
    ) -> Result<(), LError> {
        self.pfc.append(ctx.get_pure_fonctions_symbols());
        import(&mut self.env, &mut self.ctxs, ctx, WithoutPrefix).await
    }

    pub async fn import(&mut self, ctx: impl GetModule + PureFonction) -> Result<(), LError> {
        self.pfc.append(ctx.get_pure_fonctions_symbols());
        import(&mut self.env, &mut self.ctxs, ctx, WithPrefix).await
    }
}

impl GetModule for CtxStaticEval {
    fn get_module(self) -> Module {
        let mut module = Module {
            ctx: Arc::new(self),
            prelude: vec![],
            raw_lisp: Default::default(),
            label: "".to_string(),
        };

        module.add_fn_prelude(EVAL_STATIC, scheme_eval_static);

        module
    }
}

impl Documentation for CtxStaticEval {
    fn documentation() -> Vec<LHelp> {
        todo!()
    }
}

pub fn scheme_eval_static(args: &[LValue], _: &LEnv, ctx: &CtxStaticEval) -> LResult {
    if args.len() != 1 {
        return Err(WrongNumberOfArgument(
            EVAL_STATIC,
            args.into(),
            args.len(),
            1..1,
        ));
    }

    let result = eval_static(
        &args.into(),
        &mut ctx.env.clone(),
        &mut ctx.ctxs.clone(),
        &ctx.pfc,
    )?;

    Ok(result.get_lvalue().clone())
}
