use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::interface::stat::OMPASRunStat;
use crate::ompas::manager::acting::filter::ProcessFilter;
use crate::ompas::manager::acting::inner::ActingProcessKind;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::process::task::RefinementTrace;
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::monitor::MonitorManager;
use crate::ompas::manager::planning::plan_update::ActingTreeUpdate;
use crate::ompas::manager::planning::problem_update::ExecutionProblem;
use crate::ompas::manager::planning::PlannerManager;
use crate::ompas::manager::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::StateManager;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use crate::planning::planner::solver::PMetric;
use inner::InnerActingManager;
use ompas_language::exec::acting_context::DEF_PROCESS_ID;
use ompas_language::process::{LOG_TOPIC_OMPAS, PROCESS_TOPIC_OMPAS};
use ompas_middleware::{Master, ProcessInterface, OMPAS_WORKING_DIR};
use ompas_utils::task_handler::EndSignal;
use sompas_structs::lenv::LEnv;
use sompas_structs::list;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::{LValue, Sym};
use std::fs::OpenOptions;
use std::io::Write;
use std::path::PathBuf;
use std::process::{Command, Stdio};
use std::sync::Arc;
use std::{fs, thread};
use tokio::runtime::Handle;
use tokio::sync::{broadcast, mpsc, watch, RwLock};

pub mod acting_stat;
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

pub struct ActingTreeDisplayer {
    killer: mpsc::Sender<EndSignal>,
}

pub type RefInnerActingManager = Arc<RwLock<InnerActingManager>>;
#[derive(Clone)]
pub struct ActingManager {
    pub st: RefSymTable,
    pub resource_manager: ResourceManager,
    pub monitor_manager: MonitorManager,
    pub domain_manager: DomainManager,
    pub state_manager: StateManager,
    pub inner: RefInnerActingManager,
    pub clock_manager: ClockManager,
    acting_tree_displayer: Arc<RwLock<Option<ActingTreeDisplayer>>>,
}

impl ActingManager {
    pub fn new(st: RefSymTable) -> Self {
        let clock_manager = ClockManager::default();
        let resource_manager = ResourceManager::default();
        let mut ompas_domain = OMPASDomain::default();
        ompas_domain.init(&st);
        let domain: DomainManager = ompas_domain.into();
        Self {
            st: st.clone(),
            resource_manager: resource_manager.clone(),
            monitor_manager: MonitorManager::from(clock_manager.clone()),
            domain_manager: domain.clone(),
            state_manager: StateManager::new(clock_manager.clone(), st.clone()),
            inner: Arc::new(RwLock::new(InnerActingManager::new(
                resource_manager,
                clock_manager.clone(),
                domain,
                st,
            ))),
            clock_manager,
            acting_tree_displayer: Arc::new(Default::default()),
        }
    }
}

impl Default for ActingManager {
    fn default() -> Self {
        Self::new(RefSymTable::default())
    }
}

pub enum MethodModel {
    Raw(LValue, PLEnv),
    ActingModel(ActingModel),
}

impl ActingManager {
    pub async fn clear(&self) {
        self.monitor_manager.clear().await;
        self.resource_manager.clear().await;
        self.inner.write().await.clear().await;
    }

    pub async fn dump_trace(&self, path: Option<PathBuf>) {
        self.inner.read().await.dump_acting_tree(path)
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

    pub fn get_header_stat() -> String {
        InnerActingManager::get_header_stat()
    }

    pub async fn export_to_csv(&self) -> String {
        self.inner.read().await.export_to_csv()
    }

    pub async fn get_run_stat(&self) -> OMPASRunStat {
        self.inner.read().await.get_run_stats().await
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

    pub async fn get_kind(&self, id: &ActingProcessId) -> ActingProcessKind {
        self.inner.read().await.get_kind(id)
    }

    pub async fn get_tried(&self, id: &ActingProcessId) -> Vec<LValue> {
        self.inner.read().await.get_tried(id)
    }

    //ActingProcess declaration

    pub async fn new_high_level_task(&self, debug: String, args: Vec<Cst>) -> ProcessRef {
        self.inner.write().await.new_high_level_task(debug, args)
    }

    pub async fn new_high_level_command(&self, debug: String, args: Vec<Cst>) -> ProcessRef {
        self.inner.write().await.new_high_level_command(debug, args)
    }

    //Task methods
    pub async fn new_task(
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
            .new_task(label, parent, args, debug, origin)
    }

    pub async fn new_command(
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
            .new_command(label, parent, args, debug, origin)
    }

    pub async fn new_refinement(&self, parent: &ActingProcessId) -> usize {
        self.inner.write().await.new_refinement(parent)
    }

    pub async fn set_failed_method(&self, method: &ActingProcessId) {
        self.inner.write().await.set_failed_method(method)
    }

    pub async fn new_executed_method(
        &self,
        parent: &ActingProcessId,
        debug: String,
        args: Vec<Option<Cst>>,
        model: MethodModel,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_executed_method(parent, debug, args, model, origin)
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
    }

    // Acquire methods
    pub async fn new_acquire(
        &self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner.write().await.new_acquire(label, parent, origin)
    }

    pub async fn subscribe(&self, id: &ActingProcessId) -> watch::Receiver<ProcessStatus> {
        self.inner.write().await.subscribe(id)
    }

    // Setters
    //ActingProcess methods
    pub async fn set_start(&self, id: &ActingProcessId, instant: Option<Timepoint>) {
        self.inner.write().await.set_start(id, instant)
    }

    pub async fn set_end(
        &self,
        id: &ActingProcessId,
        instant: Option<Timepoint>,
        status: ProcessStatus,
    ) {
        self.inner.write().await.set_end(id, instant, status)
    }

    pub async fn set_status(&self, id: &ActingProcessId, status: ProcessStatus) {
        self.inner.write().await.set_status(id, status)
    }

    pub async fn set_moment(&self, id: &ActingProcessId, instant: Option<Timepoint>) {
        self.inner.write().await.set_moment(id, instant)
    }

    pub async fn set_executed_refinement(
        &self,
        action: &ActingProcessId,
        method: &ActingProcessId,
        refinement_trace: RefinementTrace,
    ) {
        self.inner
            .write()
            .await
            .set_executed_refinement(action, method, refinement_trace)
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
            .filter(|p| matches!(p, Label::Task(_)))
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
            .filter(|p| matches!(p, Label::ResourceAcquisition(_)))
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
        self.inner.write().await.set_s_acq(acquire_id, instant)
    }

    pub async fn get_task_args(&self, id: &ActingProcessId) -> Vec<Cst> {
        self.inner.read().await.get_task_args(id)
    }

    pub async fn set_action_args(&self, id: &ActingProcessId, args: Vec<Cst>) {
        self.inner.write().await.set_action_args(id, args);
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
            self.state_manager.clone(),
            self.domain_manager.get_inner().await,
            self.st.clone(),
            env,
            opt,
        )
        .await;

        self.inner
            .write()
            .await
            .set_planner_manager_interface(pmi)
            .await;
    }

    pub async fn get_execution_problem(&self) -> ExecutionProblem {
        let mut state = self.state_manager.get_snapshot().await;
        let resource_state = self.resource_manager.get_snapshot(None).await;
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
        self.inner.write().await.plan();
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
            .domain_manager
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

    pub async fn update_acting_tree(&self, update: ActingTreeUpdate) {
        self.inner.write().await.update_acting_tree(update).await
    }

    pub async fn start_acting_tree_display(&self) {
        let inner = self.inner.clone();
        let handle = Handle::current();
        let mut clock = self.clock_manager.subscribe_to_clock().await;

        self.stop_acting_tree_display().await;

        let (tx, mut rx) = mpsc::channel(1);

        *self.acting_tree_displayer.write().await = Some(ActingTreeDisplayer { killer: tx });

        let mut process =
            ProcessInterface::new("ACTING_TREE_DISPLAY", PROCESS_TOPIC_OMPAS, LOG_TOPIC_OMPAS)
                .await;

        thread::spawn(move || {
            let _guard = handle.enter();

            tokio::spawn(async move {
                let path: PathBuf = OMPAS_WORKING_DIR.get_ref().into();
                let mut path = path.canonicalize().unwrap();
                path.push("traces");

                path.push(format!("acting_tree_{}", Master::get_string_date()));
                fs::create_dir_all(&path).unwrap();

                let mut path_dot = path.clone();
                path_dot.push("acting_tree.dot");

                let mut first = true;

                let mut child = None;

                loop {
                    tokio::select! {
                        _ = clock.changed() => {
                            let dot = inner.read().await.export_trace_dot_graph();
                            let mut dot_file = OpenOptions::new()
                                .write(true)
                                .create(true)
                                .truncate(true)
                                .open(&path_dot)
                                .unwrap();
                            let _ = dot_file.write_all(dot.as_bytes());
                            if first {
                                first = false;

                                let mut command = Command::new("xdot");
                                command
                                    .args([&path_dot.display().to_string()])
                                    .stdout(Stdio::null())
                                    .stderr(Stdio::null());
                                child = Some(command.spawn().expect("could not spawn dot displayer"));
                            }
                        }
                        _ = process.recv() => {
                            break;
                        }
                        _ = rx.recv() => {
                            break;
                        }
                    }
                }
                if let Some(mut child) = child {
                    child.kill().expect("could not kill acting tree displayer");
                }
            })
        });
    }

    pub async fn stop_acting_tree_display(&self) {
        let mut acting_tree_displayer = self.acting_tree_displayer.write().await;
        if let Some(display) = acting_tree_displayer.as_ref() {
            let _ = display.killer.send(true).await;
        }
        *acting_tree_displayer = None;
    }
}
