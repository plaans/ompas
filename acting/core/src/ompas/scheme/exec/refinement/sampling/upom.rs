use crate::model::acting_domain::model::{ModelCollection, ModelKind};
use crate::model::acting_domain::OMPASDomain;
use crate::ompas::interface::select_mode::{Planner, SelectMode, UPOMConfig, UPOMMode};
use crate::ompas::manager::acting::process::task::Selected;
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
use im::OrdSet;
use lazy_static::lazy_static;
use ompas_language::exec::state::MOD_STATE;
use ompas_language::exec::upom::*;
use ompas_language::exec::MOD_EXEC;
use ompas_middleware::logger::LogClient;
use ompas_utils::other::get_and_update_id_counter;
use rand::prelude::IteratorRandom;
use rand::thread_rng;
use sompas_core::{eval, parse};
use sompas_macros::async_scheme_fn;
use sompas_structs::contextcollection::Context;
use sompas_structs::lenv::{ImportType, LEnv};
use sompas_structs::lmodule::LModule;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LResult;
use sompas_structs::lvalue::LValue;
use sompas_structs::lvalues::LValueS;
use std::collections::hash_map::Entry;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::marker::PhantomData;
use std::sync::atomic::AtomicUsize;
use std::sync::Arc;
use std::time::{Duration, SystemTime};
use tokio::sync::oneshot::error::TryRecvError;
use tokio::sync::RwLock;
use tokio::task::JoinHandle;

lazy_static! {
    static ref GLOBAL_UPOM: UPOMGlobal = UPOMGlobal::default();
}

#[derive(Hash, PartialEq, Eq, Clone)]
pub struct StateKey(OrdSet<String>);

impl Display for StateKey {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f,)?;
        for k in &self.0 {
            writeln!(f, "{}", k)?;
        }
        Ok(())
    }
}

impl From<&WorldStateSnapshot> for StateKey {
    fn from(value: &WorldStateSnapshot) -> Self {
        let o1: OrdSet<String> = value
            .dynamic
            .inner
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v.value))
            .collect();
        let o2: OrdSet<String> = value
            .inner_dynamic
            .inner
            .iter()
            .map(|(k, v)| format!("{}:{}", k, v.value))
            .collect();
        let o: OrdSet<String> = o1.union(o2);
        Self(o)
    }
}

#[derive(Default, Clone, Debug)]
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

impl Display for StateValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        writeln!(f, "\nN(s,t)")?;
        for (t, n) in &self.n_task {
            writeln!(f, "{} : {}", t, n)?;
        }

        writeln!(f, "N(s,m), Q(s,m), Q0(s,m)")?;
        for (k, v) in &self.method_values {
            writeln!(f, "{}: {:?}", k, v)?;
        }

        Ok(())
    }
}

impl StateValue {
    pub fn rank(&self, method: &LValueS, task: &LValueS, c: f64) -> f64 {
        let n_task = *self.n_task.get(task).unwrap();
        let method = self.method_values.get(method).unwrap().clone();
        (n_task as f64 / method.n as f64).ln().sqrt() * c + method.q
    }

    pub fn update_q_value_method<T: Utility>(&mut self, method: &LValueS, lambda: T) {
        let method_value = self.method_values.get_mut(method).unwrap();
        let q = method_value.q;
        let n = method_value.n as f64;
        let lambda = lambda.f64();
        //println!("q = {}, n={}, lambda = {}", q, n, lambda);
        let new_value = (n * q + lambda) / (1.0 + n);
        //println!("{}.q = {}", method, new_value);
        method_value.q = new_value;
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
    next: Arc<AtomicUsize>,
}

impl UPOMGlobal {
    pub async fn _q(&self, state_key: &StateKey, method: &LValueS) -> Option<f64> {
        self.inner
            .read()
            .await
            .get(state_key)
            .and_then(|sv| sv.q(method))
    }

    pub async fn _q0(&self, state_key: &StateKey, method: &LValueS) -> Option<f64> {
        self.inner
            .read()
            .await
            .get(state_key)
            .and_then(|sv| sv.q0(method))
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next.clone())
    }
}

pub struct LocalValue<T: Utility> {
    utility: T,
    remaining_depth: u64,
    bottom: bool,
}

impl<T: Utility> Default for LocalValue<T> {
    fn default() -> Self {
        Self {
            utility: Default::default(),
            remaining_depth: Default::default(),
            bottom: true,
        }
    }
}

impl<T: Utility> LocalValue<T> {
    pub fn new_from(other: &Self) -> Self {
        Self {
            utility: T::identity(),
            remaining_depth: other.remaining_depth - 1,
            bottom: true,
        }
    }
}

pub struct ModUPOM<T: Utility> {
    pub(crate) global: UPOMGlobal,
    local: Arc<RwLock<LocalValue<T>>>,
    type_utility: PhantomData<T>,
    pub(crate) models: Arc<im::HashMap<String, ModelCollection>>,
    c: f64,
    log: LogClient,
    id: usize,
}

impl<T: Utility> From<ModUPOM<T>> for Context {
    fn from(value: ModUPOM<T>) -> Self {
        Context::new(value, MOD_UPOM)
    }
}

impl<T: Utility> ModUPOM<T> {
    pub async fn new(d: u64, c: f64, models: Arc<im::HashMap<String, ModelCollection>>) -> Self {
        Self {
            global: GLOBAL_UPOM.clone(),
            local: Arc::new(RwLock::new(LocalValue {
                utility: T::default(),
                remaining_depth: d,
                bottom: true,
            })),
            type_utility: Default::default(),
            models,
            c,
            log: LogClient::new(PROCESS_UPOM, LOG_TOPIC_UPOM).await,
            id: GLOBAL_UPOM.get_next_id(),
        }
    }

    pub async fn new_from(other: &Self) -> Self {
        Self {
            global: other.global.clone(),
            local: Arc::new(RwLock::new(LocalValue::new_from(
                &*other.local.read().await,
            ))),
            type_utility: other.type_utility,
            c: other.c,
            models: other.models.clone(),
            log: other.log.clone(),
            id: GLOBAL_UPOM.get_next_id(),
        }
    }

    pub fn log(&self, message: impl Display) {
        self.log.debug(format!("({}) {}", self.id, message))
    }
}

impl<T: Utility> From<ModUPOM<T>> for LModule {
    fn from(value: ModUPOM<T>) -> Self {
        let mut m = LModule::new(value, MOD_UPOM, DOC_MOD_UPOM);
        m.add_async_fn(UPOM_TASK, upom_task::<T>, DOC_UPOM_TASK, false);
        m.add_async_fn(UPOM_COMMAND, upom_command::<T>, DOC_UPOM_COMMAND, false);
        m
    }
}

#[async_scheme_fn]
pub async fn upom_task<T: Utility>(env: &LEnv, task: &[LValue]) -> LResult {
    let time = SystemTime::now();
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    let id = ctx.id;

    ctx.log(format!("UPOM(task:{})", LValue::from(task)));
    let task_key: LValueS = LValue::from(task).try_into()?;

    let ctx_state: &ModState = env.get_context::<ModState>(MOD_STATE)?;
    let snapshot = ctx_state.state_manager.get_snapshot().await;

    let state_key = StateKey::from(&snapshot);

    let methods: Vec<LValueS> = applicable(&snapshot, task, env)
        .await?
        .drain(..)
        .map(|m| LValueS::try_from(m).unwrap())
        .collect();

    ctx.log(format!(
        "Applicable time: {} µs",
        time.elapsed().unwrap().as_micros()
    ));

    let mut global = ctx.global.inner.write().await;
    ctx.log(format!(
        "Wait on global = {} µs",
        time.elapsed().unwrap().as_micros()
    ));
    if let Entry::Vacant(v) = global.entry(state_key.clone()) {
        //ctx.log("State key does not exists yet");
        v.insert(StateValue::default());
    } else {
        //ctx.log("state key already exists");
    }
    let c = ctx.c;
    let state_value: &mut StateValue = global.get_mut(&state_key).unwrap();
    match state_value.n_task.entry(task_key.clone()) {
        Entry::Occupied(_) => {
            //ctx.log("Task already encountered");
        }
        Entry::Vacant(o) => {
            //ctx.log("Newly addressed task");
            o.insert(0);
            for method in &methods {
                // ctx.log(format!("Adding method {} to state key", method));
                state_value
                    .method_values
                    .insert(method.clone(), MethodValue::default());
            }
        }
    }
    drop(global);
    let global = ctx.global.inner.read().await;
    let state_value = global.get(&state_key).unwrap();

    let untried: Vec<_> = methods
        .iter()
        .filter(|method| state_value.method_values.get(method).unwrap().n == 0)
        .collect();
    //ctx.log(format!("untried methods: {}", LValueS::from(&untried)));
    let mc: Option<LValueS> = match untried.is_empty() {
        false => {
            let mut rng = thread_rng();
            methods.iter().choose(&mut rng).cloned()
        }
        true => methods
            .iter()
            .max_by(|m1, m2| {
                state_value
                    .rank(m1, &task_key, c)
                    .total_cmp(&state_value.rank(m2, &task_key, c))
            })
            .cloned(),
    };
    drop(global);

    let mc: LValueS = if let Some(mc) = mc {
        mc
    } else {
        //ctx.log("No applicable method");
        let mut local = ctx.local.write().await;
        local.utility = T::compose(&local.utility, &T::failure());
        return Ok(LValue::Nil);
    };

    //drop(state_value);
    let bottom;
    let utility = if ctx.local.read().await.remaining_depth == 0 {
        //ctx.log("Reached bottom");
        bottom = false;
        let LValue::List(list) = &mc.clone().into() else {
            unreachable!()
        };
        T::compute(env, SampledResult::Method(list)).await
    } else {
        //ctx.log(format!("Sampling method {}", mc));
        let mut new_env = env.clone();
        let ctx: ModUPOM<T> = ModUPOM::<T>::new_from(ctx).await;
        new_env.update_context(ctx);
        //new_env.update_context(ModState::new_from_snapshot(snapshot));
        let _ = eval(&(&mc).into(), &mut new_env, None).await;
        let local = new_env
            .get_context::<ModUPOM<T>>(MOD_UPOM)
            .unwrap()
            .local
            .write()
            .await;
        let utility = local.utility.clone();
        bottom = local.bottom;
        utility
    };
    ctx.log(format!("utility({}) = {:?}", mc, utility));
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM).unwrap();
    assert_eq!(id, ctx.id);
    //ctx.log("test");
    let mut global = ctx.global.inner.write().await;
    let state_value: &mut StateValue = global.get_mut(&state_key).unwrap();
    //ctx.log("bip");
    //ctx.log(format!("StateValue(before): {}", state_value));

    state_value.update_q_value_method(&mc, utility.clone());

    *state_value.n_task.get_mut(&task_key).unwrap() += 1;
    state_value.method_values.get_mut(&mc).unwrap().n += 1;

    //ctx.log(format!("StateValue(after): {}", state_value));
    drop(global);

    let mut local = ctx.local.write().await;
    local.utility = T::compose(&local.utility, &utility);
    local.bottom = bottom;
    ctx.log(format!(
        "{} µs",
        time.elapsed().unwrap().as_secs_f64() * 1_000_000.0
    ));
    return Ok(LValue::Nil);
}

#[async_scheme_fn]
pub async fn upom_command<T: Utility>(env: &LEnv, command: &[LValue]) -> LResult {
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    ctx.log(format!("UPOM(command:{})", LValue::from(command)));
    let label = command[0].to_string();
    let args = &command[1..];
    let command_model = ctx
        .models
        .get(&label)
        .unwrap()
        .get(&ModelKind::SimModel)
        .unwrap();
    let mut lv = vec![command_model];
    for arg in args {
        lv.push(arg.clone());
    }
    let mut env = env.clone();

    let command_result = eval(&lv.into(), &mut env, None).await;
    ctx.log(format!("result: {:?}", command_result));
    let sampled_result = match command_result {
        Ok(LValue::Err(_)) => SampledResult::Failure,
        Ok(_) => SampledResult::Command(command),
        Err(_) => {
            return command_result;
        }
    };
    let ctx: &ModUPOM<T> = env.get_context::<ModUPOM<T>>(MOD_UPOM)?;
    let utility = T::compute(&env, sampled_result).await;
    ctx.log(format!("{:?}", utility));
    let mut local = ctx.local.write().await;
    local.utility = T::compose(&local.utility, &utility);
    //ctx.log("end");
    command_result
}

pub async fn upom_select(
    task: &[LValue],
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    config: UPOMConfig,
) -> lruntimeerror::Result<Selected> {
    match config.get_mode() {
        UPOMMode::Efficiency => {
            _upom_select::<Efficiency>(task, candidates, state, env, config).await
        }
        UPOMMode::Robustness => {
            _upom_select::<Robustness>(task, candidates, state, env, config).await
        }
    }
}

async fn _upom_select<T: Utility>(
    task: &[LValue],
    candidates: &[LValue],
    state: &WorldStateSnapshot,
    env: &LEnv,
    config: UPOMConfig,
) -> lruntimeerror::Result<Selected> {
    let global = GLOBAL_UPOM.clone();
    let mut new_env = env.clone();
    let ctx_exec = new_env.get_context::<ModExec>(MOD_EXEC).unwrap();
    let domain_manager = ctx_exec.domain.clone();
    let mut domain = domain_manager.get_inner().await;
    upom_env(&mut new_env, &domain).await;
    let models_commands: im::HashMap<String, ModelCollection> = domain
        .commands
        .drain()
        .map(|(k, c)| (k, c.model_collection))
        .collect();
    let models_methods: im::HashMap<String, ModelCollection> = domain
        .methods
        .drain()
        .map(|(k, m)| (k, m.model_collection))
        .collect();
    let models = Arc::new(models_commands.union(models_methods));
    let mod_upom = ModUPOM::<T>::new(0, config.get_c(), models.clone()).await;
    new_env.import_module(mod_upom, ImportType::WithoutPrefix);

    let state = state.clone();
    let state_key = StateKey::from(&state);
    let mod_state = ModState::new_from_snapshot(state.clone());
    new_env.update_context(mod_state);
    let methods: Vec<LValueS> = candidates
        .iter()
        .map(|m| LValueS::try_from(m).unwrap())
        .collect();

    let mut m = candidates[0].clone();
    let mut q_max = 0.0;
    let global_lock = global.inner.read().await;
    for (i, method) in methods.iter().enumerate() {
        let q = if let Some(q) = global_lock.get(&state_key).and_then(|sv| sv.q0(method)) {
            q
        } else {
            let LValue::List(method) = &candidates[i] else {
                unreachable!()
            };
            T::compute(&new_env, SampledResult::Method(method))
                .await
                .f64()
        };

        if q > q_max {
            m = candidates[i].clone();
            q_max = q;
        }
    }
    drop(global_lock);

    let timeout = tokio::time::sleep(Duration::from_secs_f64(config.get_timeout()));
    let task: LValue = task.into();
    let candidates = candidates.to_vec();
    let (tx, mut rx) = tokio::sync::oneshot::channel();

    let d_min = match config.get_iterative_deepening() {
        true => 1,
        false => config.get_d_max(),
    };

    let handle = tokio::spawn(async move {
        'loop_depth: for d in d_min..config.get_d_max() + 1 {
            if rx.try_recv() != Err(TryRecvError::Empty) {
                break 'loop_depth;
            }
            let mut bottom = true;
            let mut handles: Vec<JoinHandle<bool>> = vec![];

            for _ in 0..config.get_nro() {
                let mut new_env = new_env.clone();
                let state = state.clone();
                let task = task.clone();
                let handle = tokio::spawn(async move {
                    let mod_state = ModState::new_from_snapshot(state);
                    let event_manager = mod_state.event_manager.clone();
                    let receiver_event_update_state =
                        mod_state.state_manager.new_subscriber(StateRule::All).await;
                    let ctx_exec = new_env.get_context::<ModExec>(MOD_EXEC).unwrap();
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

                    let handle = tokio::spawn(async move {
                        run_fluent_checker(receiver_event_update_state, event_manager).await;
                    });

                    let o_mod_upom = new_env.get_context::<ModUPOM<T>>(MOD_UPOM).unwrap();

                    let mod_upom =
                        ModUPOM::<T>::new(d, o_mod_upom.c, o_mod_upom.models.clone()).await;
                    new_env.import_module(mod_upom, ImportType::WithoutPrefix);
                    //mod_upom.log(format!("d = {}", d));

                    let _ = eval(&task, &mut new_env, None).await;
                    handle.abort();
                    new_env
                        .get_context::<ModUPOM<T>>(MOD_UPOM)
                        .unwrap()
                        .local
                        .read()
                        .await
                        .bottom
                });
                //handle.await;
                handles.push(handle);
            }
            for handle in handles {
                bottom &= handle.await.unwrap()
            }

            if bottom {
                //println!("reached bottom at depth: {}", d);
                break 'loop_depth;
            }
        }
    });

    tokio::select! {
        _ = timeout =>  {
            let _ = tx.send(true);
        }
        _ = handle => {}
    };

    let global = global.inner.read().await;

    let mut q_max = 0.0;
    for (i, method) in methods.iter().enumerate() {
        if let Some(q) = global.get(&state_key).and_then(|sv| sv.q(method)) {
            if q > q_max {
                m = candidates[i].clone();
                q_max = q;
            }
        }
    }

    Ok(Selected::Generated(
        m,
        SelectMode::Planning(Planner::UPOM(config)),
    ))
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
