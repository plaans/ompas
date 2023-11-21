use crate::model::acting_domain::command::Command;
use crate::model::acting_domain::OMPASDomain;
use crate::ompas::interface::select_mode::{UPOMConfig, UPOMMode};
use crate::ompas::manager::event::run_fluent_checker;
use crate::ompas::manager::state::state_update_manager::StateRule;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::ompas::scheme::exec::mode::RAEMode;
use crate::ompas::scheme::exec::refinement::applicable;
use crate::ompas::scheme::exec::refinement::sampling::efficiency::Efficiency;
use crate::ompas::scheme::exec::refinement::sampling::robustness::Robustness;
use crate::ompas::scheme::exec::refinement::sampling::{SampledResult, Utility};
use crate::ompas::scheme::exec::resource::ModResource;
use crate::ompas::scheme::exec::state::ModState;
use crate::ompas::scheme::exec::ModExec;
use lazy_static::lazy_static;
use ompas_language::exec::state::MOD_STATE;
use ompas_language::exec::upom::*;
use ompas_language::exec::MOD_EXEC;
use rand::prelude::IteratorRandom;
use rand::thread_rng;
use sompas_core::{eval, parse};
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::marker::PhantomData;
use std::sync::Arc;
use tokio::sync::Mutex;
use tokio::sync::RwLock;

lazy_static! {
    static ref GLOBAL_UPOM: UPOMGlobal = UPOMGlobal::default();
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct StateKey {
    pub inner: im::HashMap<LValueS, LValueS>,
}

impl From<&WorldStateSnapshot> for StateKey {
    fn from(value: &WorldStateSnapshot) -> Self {
        Self {
            inner: value
                .dynamic
                .inner
                .clone()
                .union(value.inner_dynamic.inner.clone())
                .iter()
                .map(|(k, v)| (k.clone(), v.value.clone()))
                .collect(),
        }
    }
}

impl StateKey {
    pub fn new(inner: im::HashMap<LValueS, LValueS>) -> Self {
        Self { inner }
    }
}

#[derive(Default, Clone)]
pub struct MethodValue {
    n: u64,
    q: f64,
    q0: f64,
}

#[derive(Default, Clone)]
pub struct StateValue {
    n_task: HashMap<LValueS, u64>,
    method_values: HashMap<LValueS, MethodValue>,
}

impl StateValue {
    pub fn compute_q_value(&self, method: &LValueS, task: &LValueS, c: f64) -> f64 {
        let n_task = *self.n_task.get(&task).unwrap();
        let method = self.method_values.get(&method).unwrap().clone();
        (n_task as f64 / method.n as f64).ln().sqrt() * c + method.q
    }

    pub fn update_q_value_method<T: Utility>(&mut self, method: &LValueS, lambda: T) {
        let method_value = self.method_values.get_mut(method).unwrap();
        method_value.q =
            (method_value.n as f64 * method_value.q + lambda.f64()) / (1.0 + method_value.n as f64)
    }

    pub fn q(&self, method: &LValueS) -> Option<f64> {
        self.method_values.get(method).map(|m| m.q)
    }

    pub fn q0(&self, method: &LValueS) -> Option<f64> {
        self.method_values.get(method).map(|m| m.q0)
    }
}

#[derive(Default, Clone)]
pub struct UPOMGlobal {
    inner: Arc<RwLock<HashMap<StateKey, StateValue>>>,
}

impl UPOMGlobal {
    pub async fn q(&self, state_key: &StateKey, method: &LValueS) -> Option<f64> {
        self.inner
            .read()
            .await
            .get(state_key)
            .and_then(|sv| sv.q(method))
    }

    pub async fn q0(&self, state_key: &StateKey, method: &LValueS) -> Option<f64> {
        self.inner
            .read()
            .await
            .get(state_key)
            .and_then(|sv| sv.q0(method))
    }
}

#[derive(Default)]
pub struct LocalValue<T: Utility> {
    utility: T,
    remaining_depth: u64,
}

impl<T: Utility> LocalValue<T> {
    pub fn new_from(other: &Self) -> Self {
        Self {
            utility: T::default(),
            remaining_depth: other.remaining_depth - 1,
        }
    }
}

pub struct ModUPOM<T: Utility> {
    pub(crate) global: UPOMGlobal,
    utility: Arc<Mutex<LocalValue<T>>>,
    type_utility: PhantomData<T>,
    pub(crate) actions: Arc<HashMap<String, Command>>,
    c: f64,
}

impl<T: Utility> From<ModUPOM<T>> for Context {
    fn from(value: ModUPOM<T>) -> Self {
        Context::new(value, MOD_UPOM)
    }
}

impl<T: Utility> ModUPOM<T> {
    pub async fn new(d: u64, c: f64, actions: Arc<HashMap<String, Command>>) -> Self {
        Self {
            global: GLOBAL_UPOM.clone(),
            utility: Arc::new(Mutex::new(LocalValue {
                utility: T::default(),
                remaining_depth: d,
            })),
            type_utility: Default::default(),
            actions,
            c,
        }
    }

    pub async fn new_from(other: &Self) -> Self {
        Self {
            global: other.global.clone(),
            utility: Arc::new(Mutex::new(LocalValue::new_from(
                &*other.utility.lock().await,
            ))),
            type_utility: other.type_utility.clone(),
            c: other.c,
            actions: other.actions.clone(),
        }
    }
}

impl<T: Utility> From<ModUPOM<T>> for LModule {
    fn from(value: ModUPOM<T>) -> Self {
        let mut m = LModule::new(value, MOD_UPOM, DOC_MOD_UPOM);
        m.add_async_mut_fn(UPOM_TASK, upom_task::<T>, DOC_UPOM_TASK);
        m.add_async_mut_fn(UPOM_COMMAND, upom_command::<T>, DOC_UPOM_COMMAND);
        m
    }
}

#[async_scheme_fn]
pub async fn upom_task<T: Utility>(env: &mut LEnv, task: &[LValue]) -> LResult {
    let ctx_state: &ModState = env.get_context::<ModState>(MOD_STATE)?;
    let snapshot = ctx_state.state_manager.get_snapshot().await;

    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    let task_key: LValueS = LValue::from(task).try_into()?;

    let state_key: im::HashMap<LValueS, LValueS> = snapshot
        .dynamic
        .inner
        .clone()
        .union(snapshot.inner_dynamic.inner.clone())
        .iter()
        .map(|(key, value)| (key.clone(), value.value.clone()))
        .collect();
    let state_key = StateKey::new(state_key);
    let mut global = ctx.global.inner.write().await;
    let methods: Vec<LValueS> = applicable(&snapshot, task, env)
        .await?
        .drain(..)
        .map(|m| LValueS::try_from(m).unwrap())
        .collect();

    if let Entry::Vacant(v) = global.entry(state_key.clone()) {
        let mut state_value = StateValue::default();
        state_value.n_task.insert(task_key.clone(), 0);

        for method in &methods {
            state_value
                .method_values
                .insert(method.clone(), MethodValue::default());
        }
        v.insert(state_value);
    }
    let c = ctx.c;
    let state_value: &mut StateValue = global.get_mut(&state_key).unwrap();

    let untried: Vec<_> = methods
        .iter()
        .filter(|method| state_value.method_values.get(method).unwrap().n == 0)
        .collect();
    let mc: LValueS = match untried.is_empty() {
        false => {
            let mut rng = thread_rng();
            methods.iter().choose(&mut rng).cloned().unwrap()
        }
        true => methods
            .iter()
            .max_by(|m1, m2| {
                state_value
                    .compute_q_value(m1, &task_key, c)
                    .total_cmp(&state_value.compute_q_value(m2, &task_key, c))
            })
            .unwrap()
            .clone(),
    };
    let mut new_env = env.clone();
    new_env.update_context(ModUPOM::<T>::new_from(ctx).await);
    new_env.update_context(ModState::new_from_snapshot(snapshot));
    drop(global);
    let _ = eval(&(&mc).into(), &mut new_env, None).await;
    let utility = new_env
        .get_context::<ModUPOM<T>>(MOD_UPOM)
        .unwrap()
        .utility
        .lock()
        .await
        .utility
        .clone();
    let mut global = ctx.global.inner.write().await;
    let state_value: &mut StateValue = global.get_mut(&state_key).unwrap();

    state_value.update_q_value_method(&mc, utility.clone());

    *state_value.n_task.get_mut(&task_key).unwrap() += 1;
    state_value.method_values.get_mut(&mc).unwrap().n += 1;

    let mut local = ctx.utility.lock().await;
    local.utility = T::compose(&local.utility, &utility);
    Ok(LValue::Nil)
}

#[async_scheme_fn]
pub async fn upom_command<T: Utility>(env: &mut LEnv, command: &[LValue]) -> LResult {
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    let label = (&command[0]).to_string();
    let args = &command[1..];
    let command_model = ctx.actions.get(&label).unwrap();
    let mut lv = vec![command_model.get_body().clone()];
    for arg in args {
        lv.push(arg.clone());
    }

    let command_result = eval(&lv.into(), env, None).await;
    let sampled_result = match command_result {
        Ok(LValue::Err(_)) => SampledResult::Failure,
        Ok(_) => SampledResult::Success(command),
        Err(_) => {
            return command_result;
        }
    };
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    let mut local = ctx.utility.lock().await;
    local.utility = T::compose(&local.utility, &T::compute(env, sampled_result).await);
    command_result
}

pub async fn upom_select(
    task: &[LValue],
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    config: UPOMConfig,
) -> LResult {
    let global = GLOBAL_UPOM.clone();
    let mut new_env = env.clone();

    let ctx_exec = env.get_context::<ModExec>(MOD_EXEC).unwrap();
    let domain_manager = ctx_exec.domain.clone();
    let mod_state = ModState::new_from_snapshot(state.clone());
    let event_manager = mod_state.event_manager.clone();
    let receiver_event_update_state = mod_state.state_manager.new_subscriber(StateRule::All).await;
    let resource_manager = ctx_exec
        .acting_manager
        .resource_manager
        .new_from_current()
        .await;

    new_env.update_context(ModResource::new_for_sim(
        resource_manager,
        mod_state.state_manager.clone(),
    ));
    new_env.update_context(mod_state);
    new_env.update_context(RAEMode::Simu);

    let methods: Vec<LValueS> = candidates
        .iter()
        .map(|m| LValueS::try_from(m).unwrap())
        .collect();

    let mut m = candidates[0].clone();
    let mut q_max = 0.0;
    let state_key = StateKey::from(state);
    for (i, method) in methods.iter().enumerate() {
        if let Some(q) = global.q0(&state_key, method).await {
            if q > q_max {
                m = candidates[i].clone();
                q_max = q;
            }
        }
    }

    let domain = domain_manager.get_inner().await;
    upom_env(&mut new_env, &domain).await;
    tokio::spawn(async move {
        run_fluent_checker(receiver_event_update_state, event_manager).await;
    });
    let actions = Arc::new(domain.commands);

    let c = config.get_c();
    for d in 1..config.get_d() {
        for _ in 0..config.get_nro() {
            let mut new_env = new_env.clone();
            match config.get_mode() {
                UPOMMode::Efficiency => {
                    let mod_upom = ModUPOM::<Efficiency>::new(d, c, actions.clone()).await;
                    new_env.import_module(mod_upom, ImportType::WithoutPrefix);
                }
                UPOMMode::Robustness => {
                    let mod_upom = ModUPOM::<Robustness>::new(d, c, actions.clone()).await;
                    new_env.import_module(mod_upom, ImportType::WithoutPrefix);
                }
            }
            let task: LValue = task.into();
            let _ = eval(&task, &mut new_env, None).await;
        }
        let mut q_max = 0.0;
        for (i, method) in methods.iter().enumerate() {
            if let Some(q) = global.q(&state_key, method).await {
                if q > q_max {
                    m = candidates[i].clone();
                    q_max = q;
                }
            }
        }
    }

    Ok(m)
}

pub async fn upom_env(env: &mut LEnv, domain: &OMPASDomain) {
    let mut env_eval = env.clone();
    for (label, command) in &domain.commands {
        let lv_params = command.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in command.get_parameters().get_labels() {
            params.push_str(param.to_string().as_str());
            params.push(' ');
        }
        let model_expr = format!(
            "(lambda {} (upom-command '{} {}))",
            lv_params, label, params,
        );
        let model = eval(
            &parse(model_expr.as_str(), &mut env_eval).await.unwrap(),
            &mut env_eval,
            None,
        )
        .await
        .unwrap();
        env.insert(label, model);
    }
    for (label, task) in &domain.tasks {
        let lv_params = task.get_parameters().get_params_as_lvalue();
        let mut params = "".to_string();
        for param in task.get_parameters().get_labels() {
            params.push_str(param.to_string().as_str());
            params.push(' ');
        }
        let model_expr = format!("(lambda {} (upom-task '{} {}))", lv_params, label, params);
        let model = eval(
            &parse(model_expr.as_str(), &mut env_eval).await.unwrap(),
            &mut env_eval,
            None,
        )
        .await
        .unwrap();
        env.insert(label, model);
    }
}
