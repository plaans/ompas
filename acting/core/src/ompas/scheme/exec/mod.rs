use crate::model::process_ref::{Label, ProcessRef};
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::acting::{ActingManager, ActingProcessId};
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::ompas::OMPASManager;
use crate::ompas::manager::platform::PlatformManager;
use crate::ompas::scheme::exec::acting_context::ModActingContext;
use crate::ompas::scheme::exec::mode::{CtxMode, RAEMode};
use crate::ompas::scheme::exec::platform::ModPlatform;
use crate::ompas::scheme::exec::refinement::ModRefinement;
use crate::ompas::scheme::exec::resource::ModResource;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::monitor::control::ModControl;
use ompas_language::exec::acting_context::MOD_ACTING_CONTEXT;
use ompas_language::exec::mode::DOC_CTX_MODE;
use ompas_language::exec::{ARBITRARY, DOC_ARBITRARY, DOC_MOD_EXEC, MOD_EXEC};
use ompas_language::process::LOG_TOPIC_OMPAS;
use ompas_middleware::logger::LogClient;
use sompas_core::eval;
use sompas_core::modules::list::car;
use sompas_macros::async_scheme_fn;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lmodule::LModule;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::{LResult, LRuntimeError};
use sompas_structs::lvalue::LValue;

pub mod acting_context;
pub mod mode;
pub mod platform;
pub mod refinement;
pub mod resource;
pub mod state;
/*
LANGUAGE
 */

pub const LABEL_ENUMERATE_PARAMS: &str = "enumerate-params";

///Context that will contains primitives for the RAE executive
pub struct ModExec {
    options: OMPASManager,
    pub acting_manager: ActingManager,
    domain: DomainManager,
    platform: PlatformManager,
    log: LogClient,
}

impl ModExec {
    pub async fn new(monitor: &ModControl) -> Self {
        Self {
            options: monitor.options.clone(),
            acting_manager: monitor.acting_manager.clone(),
            domain: monitor.acting_manager.domain_manager.clone(),
            platform: monitor.platform.clone(),
            log: LogClient::new("exec-ompas", LOG_TOPIC_OMPAS).await,
        }
    }
}

impl From<ModExec> for LModule {
    fn from(m: ModExec) -> Self {
        let mod_platform = ModPlatform::new(&m);
        let mod_resource = ModResource::new(&m);
        let mod_state = ModState::new(&m);
        let mod_refinement = ModRefinement::new(&m);

        let mut module = LModule::new(m, MOD_EXEC, DOC_MOD_EXEC);
        module.add_subcontext(CtxMode::new(RAEMode::Exec), DOC_CTX_MODE);
        module.add_submodule(mod_platform, ImportType::WithoutPrefix);
        module.add_submodule(mod_resource, ImportType::WithoutPrefix);
        module.add_submodule(mod_state, ImportType::WithoutPrefix);
        module.add_submodule(ModActingContext::default(), ImportType::WithoutPrefix);
        module.add_submodule(mod_refinement, ImportType::WithoutPrefix);
        module.add_async_fn(ARBITRARY, arbitrary, DOC_ARBITRARY, false);
        module
    }
}

#[async_scheme_fn]
pub async fn arbitrary(env: &LEnv, args: &[LValue]) -> LResult {
    let pr = &env
        .get_context::<ModActingContext>(MOD_ACTING_CONTEXT)?
        .process_ref;
    let supervisor = &env.get_context::<ModExec>(MOD_EXEC)?.acting_manager;

    let greedy = match args.len() {
        1 => car(env, &[args[0].clone()]),
        2 => {
            eval(
                &vec![
                    args[1].clone(),
                    vec![LPrimitive::Quote.into(), args[0].clone()].into(),
                ]
                .into(),
                &mut env.clone(),
                None,
            )
            .await
        }
        _ => Err(LRuntimeError::wrong_number_of_args(ARBITRARY, args, 1..2)),
    }?;

    let set: Vec<LValue> = args[0].clone().try_into()?;

    let id: ActingProcessId = match pr {
        ProcessRef::Id(id) => {
            if supervisor.get_kind(id).await == ActingProcessKind::Method {
                supervisor
                    .new_arbitrary(
                        Label::Arbitrary(supervisor.get_number_arbitrary(*id).await),
                        id,
                        ProcessOrigin::Execution,
                    )
                    .await
            } else {
                panic!()
            }
        }
        ProcessRef::Relative(id, labels) => match supervisor.get_id(pr.clone()).await {
            Some(id) => id,
            None => match labels[0] {
                Label::Arbitrary(s) => {
                    supervisor
                        .new_arbitrary(Label::Arbitrary(s), id, ProcessOrigin::Execution)
                        .await
                }
                _ => panic!(),
            },
        },
    };

    let value = supervisor.set_arbitrary_value(&id, set, greedy).await;

    Ok(value)
}

/*
#[cfg(test)]
mod test {
    use super::*;
    use crate::ompas::scheme::monitor::ModMonitor;
    use sompas_core::get_root_env;
    use sompas_language::list::SECOND;
    use sompas_structs::lenv::ImportType;
    use sompas_structs::lruntimeerror;

    #[skip]
    #[tokio::test]
    async fn test_arbitrary() -> lruntimeerror::Result<()> {
        let mut env: LEnv = get_root_env().await;
        env.import_module(
            ModExec::new(&ModControl::new(&ModMonitor::default())).await,
            ImportType::WithoutPrefix,
        );

        let lv = &[vec![1, 2, 3].into()];
        let result = arbitrary(&env, lv).await?;
        assert_eq!(result, LValue::from(1));

        let lv = &[vec![1, 2, 3].into(), SECOND.into()];
        let result = arbitrary(&env, lv).await?;
        assert_eq!(result, LValue::from(2));

        Ok(())
    }
}
*/
