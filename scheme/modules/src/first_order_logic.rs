use sompas_core::eval;
use sompas_language::first_order_logic::{
    DOC_EXISTS, DOC_FORALL, DOC_MOD_FIRST_ORDER_LOGIC, EXISTS, FORALL, MOD_FIRST_ORDER_LOGIC,
};
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::LEnv;
use sompas_structs::list;
use sompas_structs::llambda::LLambda;
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;

#[derive(Default, Debug)]
pub struct ModFirstOrderLogic {}

impl From<ModFirstOrderLogic> for LModule {
    fn from(m: ModFirstOrderLogic) -> Self {
        let mut module = LModule::new(m, MOD_FIRST_ORDER_LOGIC, DOC_MOD_FIRST_ORDER_LOGIC);

        module.add_async_fn(EXISTS, exists, DOC_EXISTS, true);
        module.add_async_fn(FORALL, forall, DOC_FORALL, true);
        module
    }
}

#[async_scheme_fn]
pub async fn exists(env: &LEnv, set: Vec<LValue>, lambda: LLambda) -> Result<bool, LRuntimeError> {
    for e in set {
        let value: bool = eval(&list!(lambda.clone().into(), e), &mut env.clone(), None)
            .await?
            .try_into()?;
        if value {
            return Ok(true);
        }
    }
    Ok(false)
}

#[async_scheme_fn]
pub async fn forall(env: &LEnv, set: Vec<LValue>, lambda: LLambda) -> Result<bool, LRuntimeError> {
    for e in set {
        let value: bool = eval(&list!(lambda.clone().into(), e), &mut env.clone(), None)
            .await?
            .try_into()?;
        if !value {
            return Ok(false);
        }
    }
    Ok(true)
}

#[cfg(test)]
mod tests {
    use crate::first_order_logic::ModFirstOrderLogic;
    use sompas_core::modules::ModStd;
    use sompas_core::{eval, parse};
    use sompas_structs::lenv::ImportType::WithoutPrefix;
    use sompas_structs::lenv::LEnv;

    #[tokio::test]
    async fn test_exists() {
        let mut env = LEnv::default();
        env.import_module(ModStd::default(), WithoutPrefix);
        env.import_module(ModFirstOrderLogic::default(), WithoutPrefix);

        let new_str =
            |t: u32| -> String { format!("(exists '(1 2 3) (lambda (?n) (>= ?n {})))", t) };

        let value_1: bool = eval(&parse(&new_str(2), &mut env).await.unwrap(), &mut env, None)
            .await
            .unwrap()
            .try_into()
            .unwrap();

        assert!(value_1);

        let value_2: bool = eval(&parse(&new_str(4), &mut env).await.unwrap(), &mut env, None)
            .await
            .unwrap()
            .try_into()
            .unwrap();

        assert!(!value_2);
    }

    #[tokio::test]
    async fn test_forall() {
        let mut env = LEnv::default();
        env.import_module(ModStd::default(), WithoutPrefix);
        env.import_module(ModFirstOrderLogic::default(), WithoutPrefix);

        let new_str =
            |t: u32| -> String { format!("(exists '(1 2 3) (lambda (?n) (>= ?n {})))", t) };

        let value_1: bool = eval(&parse(&new_str(2), &mut env).await.unwrap(), &mut env, None)
            .await
            .unwrap()
            .try_into()
            .unwrap();

        assert!(value_1);

        let value_2: bool = eval(&parse(&new_str(4), &mut env).await.unwrap(), &mut env, None)
            .await
            .unwrap()
            .try_into()
            .unwrap();

        assert!(!value_2);
    }
}
