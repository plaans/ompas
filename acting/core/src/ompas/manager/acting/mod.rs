use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::OMPASDomain;
use crate::model::process_ref::{Label, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::ompas::manager::acting::filter::ProcessFilter;
use crate::ompas::manager::acting::inner::ProcessKind;
use crate::ompas::manager::acting::interval::Timepoint;
use crate::ompas::manager::acting::planning::plan_update::PlanUpdateManager;
use crate::ompas::manager::acting::planning::problem_update::{
    ExecutionProblem, ProblemUpdateManager,
};
use crate::ompas::manager::acting::planning::{run_continuous_planning, ContinuousPlanningConfig};
use crate::ompas::manager::acting::process::ProcessOrigin;
use crate::ompas::manager::monitor::MonitorManager;
use crate::ompas::manager::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::ompas::manager::state::world_state::WorldState;
use inner::InnerActingManager;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::path::PathBuf;
use std::sync::Arc;
use tokio::sync::{watch, RwLock};
use tokio::time::Instant;

pub mod acting_var;
pub mod filter;
pub mod inner;
pub mod interval;
pub mod planning;
pub mod process;
pub mod task_network;

pub type ActionId = usize;

//Ids of element of the acting manager
pub type ActingProcessId = usize;
pub type AMId = usize;

#[derive(Clone)]
pub struct ActingManager {
    pub resource_manager: ResourceManager,
    pub monitor_manager: MonitorManager,
    pub state: WorldState,
    pub inner: Arc<RwLock<InnerActingManager>>,
    instant: Instant,
}

impl Default for ActingManager {
    fn default() -> Self {
        let instant = Instant::now();
        let resource_manager = ResourceManager::default();
        Self {
            resource_manager: resource_manager.clone(),
            monitor_manager: Default::default(),
            state: Default::default(),
            inner: Arc::new(RwLock::new(InnerActingManager::new(
                resource_manager,
                instant.clone(),
                RefSymTable::default(),
            ))),
            instant,
        }
    }
}

impl ActingManager {
    pub async fn clear(&self) {
        self.monitor_manager.clear().await;
        self.resource_manager.clear().await;
        self.state.clear().await;
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
        _working_dir: Option<PathBuf>,
        _file: Option<String>,
    ) -> LValue {
        todo!()
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

    pub fn instant(&self) -> Timepoint {
        self.instant.elapsed().as_millis().into()
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
        model: ActingModel,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_refinement(parent, debug, model, origin)
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

    //Command methods
    /*pub async fn new_command(
        &self,
        label: Label,
        parent: &ActingProcessId,
        debug: String,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        self.inner
            .write()
            .await
            .new_command(label, parent, debug, origin)
    }*/

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

    pub async fn reserve(
        &mut self,
        id: &ActingProcessId,
        resource: String,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<(), LRuntimeError> {
        self.inner
            .write()
            .await
            .reserve(id, resource, quantity, priority)
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
        self.inner.read().await.get_task_args(id).clone()
    }

    pub async fn get_lv(&self, id: &ActingProcessId) -> LValue {
        self.inner.read().await.get_lv(id)
    }

    pub async fn get_om(&self, id: &ActingProcessId) -> LValue {
        self.inner.read().await.get_om(id)
    }

    pub async fn get_debug(&self, id: &ActingProcessId) -> Option<String> {
        self.inner.read().await.get_debug(id).clone()
    }

    pub async fn start_continuous_planning(&self, domain: OMPASDomain, st: RefSymTable, env: LEnv) {
        let mut locked = self.inner.write().await;
        let (plan_update_manager, tx) = PlanUpdateManager::new(self.clone());
        let (problem_update_manager, rx, tx_notif) = ProblemUpdateManager::new(self.clone());
        locked.set_update_notifier(tx_notif);
        let config = ContinuousPlanningConfig {
            problem_receiver: rx,
            plan_sender: tx,
            domain,
            st,
            env,
        };
        tokio::spawn(async {
            let manager = problem_update_manager;
            manager.run().await;
        });
        tokio::spawn(async {
            let manager = plan_update_manager;
            manager.run().await;
        });
        tokio::spawn(run_continuous_planning(config));
    }

    pub async fn get_execution_problem(&self) -> ExecutionProblem {
        ExecutionProblem {
            state: self.state.get_snapshot().await,
            chronicles: self.inner.read().await.get_current_chronicles(),
        }
    }

    /*pub async fn absorb_plan_result(&self, result: ActingPlanResult) {
        self.inner.write().await.absorb_planner_result(result).await
    }*/
}
