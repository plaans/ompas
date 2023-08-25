use crate::model::acting_domain::model::ActingModel;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::process_ref::{Label, MethodId, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::acting::filter::ProcessFilter;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::monitor::MonitorManager;
use crate::ompas::manager::planning::plan_update::ActingTreeUpdate;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::planning::PlannerManager;
use crate::ompas::manager::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::state_manager::StateManager;
use crate::planning::conversion::_convert;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::p_eval::p_eval;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use crate::planning::conversion::flow_graph::algo::pre_processing::pre_processing;
use crate::planning::planner::solver::PMetric;
use inner::InnerActingManager;
use ompas_language::exec::acting_context::DEF_PROCESS_ID;
use sompas_structs::lenv::LEnv;
use sompas_structs::list;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::{broadcast, watch, RwLock};

pub mod acting_var;
pub mod filter;
pub mod inner;
pub mod interval;
pub mod process;
pub mod task_network;

pub type ActionId = usize;

//Ids of element of the acting manager
pub type ActingProcessId = usize;
pub type AMId = usize;

pub type RefInnerActingManager = Arc<RwLock<InnerActingManager>>;
#[derive(Clone)]
pub struct ActingManager {
    pub st: RefSymTable,
    pub resource_manager: ResourceManager,
    pub monitor_manager: MonitorManager,
    pub domain: DomainManager,
    pub state: StateManager,
    pub inner: RefInnerActingManager,
    pub clock_manager: ClockManager,
}

impl ActingManager {
    pub fn new(st: RefSymTable) -> Self {
        let clock_manager = ClockManager::default();
        let resource_manager = ResourceManager::default();
        Self {
            st: st.clone(),
            resource_manager: resource_manager.clone(),
            monitor_manager: MonitorManager::from(clock_manager.clone()),
            domain: Default::default(),
            state: StateManager::new(st.clone()),
            inner: Arc::new(RwLock::new(InnerActingManager::new(
                resource_manager,
                clock_manager.clone(),
                st,
            ))),
            clock_manager,
        }
    }
}

impl Default for ActingManager {
    fn default() -> Self {
        Self::new(RefSymTable::default())
    }
}

impl ActingManager {
    pub async fn clear(&self) {
        self.monitor_manager.clear().await;
        self.resource_manager.clear().await;
        self.inner.write().await.clear().await;
    }

    pub async fn dump_trace(&self, path: Option<PathBuf>) {
        self.inner.read().await.dump_trace(path)
    }

    pub async fn format_task_network(&self) -> String {
        todo!()
    }

    pub async fn print_processes(&self, _pf: ProcessFilter) -> String {
        todo!()
    }

    pub async fn get_stats(&self) -> LValue {
        todo!()
    }

    pub async fn export_to_csv(
        &self,
        working_dir: Option<PathBuf>,
        file: Option<String>,
    ) -> LValue {
        self.inner
            .read()
            .await
            .export_to_csv(working_dir, file)
            .await;
        LValue::Nil
    }

    pub async fn st(&self) -> RefSymTable {
        self.inner.read().await.st()
    }

    pub async fn get_id(&self, pr: impl Into<ProcessRef>) -> Option<ActingProcessId> {
        self.inner.read().await.get_id(pr)
    }

    pub async fn get_status(&self, id: &ActingProcessId) -> ProcessStatus {
        self.inner.read().await.get_status(id)
    }

    pub async fn get_origin(&self, id: &ActingProcessId) -> ProcessOrigin {
        self.inner.read().await.get_origin(id)
    }

    pub async fn get_kind(&self, id: &ActingProcessId) -> ProcessKind {
        self.inner.read().await.get_kind(id)
    }

    pub async fn get_tried(&self, id: &ActingProcessId) -> Vec<LValue> {
        self.inner.read().await.get_tried(id)
    }

    //ActingProcess declaration

    pub async fn new_high_level_task(&self, debug: String, args: Vec<Cst>) -> ProcessRef {
        self.inner
            .write()
            .await
            .new_high_level_task(debug, args)
            .await
    }

    //Task methods
    pub async fn new_action(
        &self,
        label: Label,
        parent: &ActingProcessId,
        args: Vec<Option<Cst>>,
        debug: String,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_action(label, parent, args, debug, origin)
            .await
    }

    pub async fn new_refinement(
        &self,
        parent: &ActingProcessId,
        debug: String,
        args: Vec<Option<Cst>>,
        model: ActingModel,
        origin: ProcessOrigin,
        method_id: Option<MethodId>,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_refinement(parent, debug, args, model, origin, method_id)
            .await
    }

    pub async fn new_arbitrary(
        &self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_arbitrary(label, parent, origin)
            .await
    }

    // Acquire methods
    pub async fn new_acquire(
        &self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_acquire(label, parent, origin)
            .await
    }

    pub async fn subscribe(&self, id: &ActingProcessId) -> watch::Receiver<ProcessStatus> {
        self.inner.write().await.subscribe(id)
    }

    // Setters
    //ActingProcess methods
    pub async fn set_start(&self, id: &ActingProcessId, instant: Option<Timepoint>) {
        self.inner.write().await.set_start(id, instant).await
    }

    pub async fn set_end(
        &self,
        id: &ActingProcessId,
        instant: Option<Timepoint>,
        status: ProcessStatus,
    ) {
        self.inner.write().await.set_end(id, instant, status).await
    }

    pub async fn set_status(&self, id: &ActingProcessId, status: ProcessStatus) {
        self.inner.write().await.set_status(id, status)
    }

    pub async fn set_moment(&self, id: &ActingProcessId, instant: Option<Timepoint>) {
        self.inner.write().await.set_moment(id, instant).await
    }

    pub async fn get_last_planned_refinement(
        &self,
        task_id: &ActingProcessId,
    ) -> Option<ActingProcessId> {
        self.inner.read().await.get_last_planned_refinement(task_id)
    }

    pub async fn get_last_executed_refinement(
        &self,
        task_id: &ActingProcessId,
    ) -> Option<ActingProcessId> {
        self.inner
            .read()
            .await
            .get_last_executed_refinement(task_id)
    }

    pub async fn get_number_arbitrary(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .childs
            .keys()
            .filter(|p| matches!(p, Label::Arbitrary(_)))
            .count()
    }

    pub async fn get_number_subtask(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .childs
            .keys()
            .filter(|p| matches!(p, Label::Action(_)))
            .count()
    }

    pub async fn get_number_acquire(&self, method_id: ActingProcessId) -> usize {
        self.inner
            .read()
            .await
            .get(method_id)
            .unwrap()
            .inner
            .as_method()
            .unwrap()
            .childs
            .keys()
            .filter(|p| matches!(p, Label::Acquire(_)))
            .count()
    }

    pub async fn executed(&self, id: &ActingProcessId) {
        self.inner.write().await.executed(id)
    }

    pub async fn set_arbitrary_value(
        &self,
        id_arbitrary: &ActingProcessId,
        set: Vec<LValue>,
        greedy: LValue,
    ) -> LValue {
        self.inner
            .write()
            .await
            .set_arbitrary_value(id_arbitrary, set, greedy)
            .await
    }

    pub async fn acquire(
        &self,
        id: &ActingProcessId,
        resource: String,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        self.inner
            .write()
            .await
            .acquire(id, resource, quantity, priority)
            .await
    }

    pub async fn set_s_acq(&self, acquire_id: &ActingProcessId, instant: Option<Timepoint>) {
        self.inner
            .write()
            .await
            .set_s_acq(acquire_id, instant)
            .await
    }

    pub async fn get_task_args(&self, id: &ActingProcessId) -> Vec<Cst> {
        self.inner.read().await.get_task_args(id)
    }

    pub async fn set_action_args(&self, id: &ActingProcessId, args: Vec<Cst>) {
        self.inner.write().await.set_action_args(id, args).await;
    }

    pub async fn get_lv(&self, id: &ActingProcessId) -> LValue {
        self.inner.read().await.get_lv(id)
    }

    pub async fn get_refinement_lv(&self, id: &ActingProcessId) -> LValue {
        self.inner.read().await.get_refinement_lv(id)
    }

    pub async fn get_om(&self, id: &ActingProcessId) -> LValue {
        self.inner.read().await.get_om(id)
    }

    pub async fn get_debug(&self, id: &ActingProcessId) -> Option<String> {
        self.inner.read().await.get_debug(id).clone()
    }

    pub async fn start_planner_manager(&self, env: LEnv, opt: Option<PMetric>) {
        let pmi = PlannerManager::run(
            self.inner.clone(),
            self.state.clone(),
            self.resource_manager.clone(),
            self.clock_manager.clone(),
            self.domain.get_inner().await,
            self.st.clone(),
            env,
            opt,
        )
        .await;

        self.inner
            .write()
            .await
            .set_planner_manager_interface(pmi)
            .await
    }

    pub async fn get_execution_problem(&self) -> ExecutionProblem {
        let mut state = self.state.get_snapshot().await;
        let resource_state = self.resource_manager.get_snapshot().await;
        state.absorb(resource_state);
        ExecutionProblem {
            state,
            chronicles: self.inner.read().await.get_current_chronicles(),
        }
    }

    pub async fn subscribe_on_plan_update(&self) -> Option<broadcast::Receiver<bool>> {
        self.inner
            .read()
            .await
            .subscribe_on_planner_tree_update()
            .await
    }

    pub async fn plan(&self) {
        self.inner.write().await.plan().await;
    }

    pub async fn get_om_lvalue(&self, id: &ActingProcessId) -> LValue {
        let locked = self.inner.read().await;
        let am_id = locked.processes[*id].am_id();
        let action_id = locked.processes[*id].parent();

        let model: &ActingModel = &locked.models[am_id];
        let lv: LValue = locked.get_refinement_lv(id);
        let lv_om: LValue = model.lv_om.clone();
        let (label, params_values) = if let LValue::List(list) = lv {
            (list[0].to_string(), list[1..].to_vec())
        } else {
            panic!("action_id: {action_id}, lv: {}, lv_om: {}", lv, lv_om)
        };

        let mut body = vec![
            LPrimitive::Begin.into(),
            list!(DEF_PROCESS_ID.into(), (*id).into()),
        ];

        let labels: Vec<Arc<Sym>> = self
            .domain
            .get_method(&label)
            .await
            .unwrap()
            .parameters
            .get_labels();

        for (param, value) in labels.iter().zip(params_values) {
            body.push(list![LPrimitive::Define.into(), param.into(), value]);
        }

        body.push(lv_om);
        list!(body.into(), (action_id).into())
    }

    pub async fn generate_acting_model_for_method(
        &self,
        lv: &LValue,
        mut p_env: PLEnv,
    ) -> Result<ActingModel, LRuntimeError> {
        let debug = lv.to_string();
        let p_eval_lv = p_eval(lv, &mut p_env).await?;
        //debug_println!("{}\np_eval =>\n{}", lv.format(0), p_eval_lv.format(0));
        let lv_om = annotate(p_eval_lv);

        let mut lv_expanded = None;
        let mut chronicle = None;

        if self.inner.read().await.is_planner_launched() {
            let st = self.st.clone();
            let mut ch = Chronicle::new(debug, ChronicleKind::Method, st.clone());
            let mut name = vec![];
            if let LValue::List(list) = &lv {
                for arg in list.as_slice() {
                    let id = match arg {
                        LValue::Symbol(s) => st.new_symbol(s),
                        LValue::Number(LNumber::Int(i)) => st.new_int(*i),
                        LValue::Number(LNumber::Float(f)) => st.new_float(*f),
                        LValue::True => st.new_bool(true),
                        LValue::Nil => st.new_bool(false),
                        _ => unreachable!(),
                    };
                    ch.add_var(id);
                    name.push(id);
                }
            } else {
                panic!()
            };
            ch.set_name(name);

            let ch = Some(ch);
            let pp_lv = pre_processing(&lv_om, &p_env).await?;
            //debug_println!("pre_processing =>\n{}", pp_lv.format(0));

            chronicle = match _convert(ch, &pp_lv, &mut p_env, st).await {
                Ok(ch) => Some(ch),
                Err(e) => {
                    println!("convert error: {}", e);
                    None
                }
            };

            lv_expanded = Some(pp_lv);
        }

        Ok(ActingModel {
            lv: lv.clone(),
            lv_om,
            lv_expanded,
            instantiations: vec![],
            chronicle,
        })
    }

    pub async fn update_acting_tree(&self, update: ActingTreeUpdate) {
        self.inner.write().await.update_acting_tree(update).await
    }
}
