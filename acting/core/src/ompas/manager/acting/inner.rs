use crate::model::acting_domain::model::{ActingModel, ROOT};
use crate::model::chronicle::acting_binding::ActingBinding;
use crate::model::chronicle::{Chronicle, ChronicleKind, Instantiation};
use crate::model::process_ref::{Label, ProcessRef};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::acting_var::{
    ActingVal, ActingValUpdate, ActingVarCollection, ActingVarId, ActingVarRef, AsCst, ExecutionVar,
};
use crate::ompas::manager::acting::interval::{Interval, Timepoint};
use crate::ompas::manager::acting::planning::plan_update::{Choice, ChoiceInner};
use crate::ompas::manager::acting::planning::problem_update::ActingUpdateNotification;
use crate::ompas::manager::acting::process::acquire::AcquireProcess;
use crate::ompas::manager::acting::process::action::ActionProcess;
use crate::ompas::manager::acting::process::arbitrary::ArbitraryProcess;
use crate::ompas::manager::acting::process::refinement::RefinementProcess;
use crate::ompas::manager::acting::process::root_task::RootProcess;
use crate::ompas::manager::acting::process::{ActingProcess, ActingProcessInner, ProcessOrigin};
use crate::ompas::manager::acting::{AMId, ActingProcessId};
use crate::ompas::manager::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::planning::conversion::flow_graph::graph::Dot;
use crate::planning::planner::problem::ChronicleInstance;
use crate::TOKIO_CHANNEL_SIZE;
use aries_planning::chronicles::ChronicleOrigin;
use async_recursion::async_recursion;
use chrono::{DateTime, Utc};
use im::HashSet;
use ompas_language::supervisor::*;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::env::set_current_dir;
use std::fmt::{Display, Formatter, Write};
use std::fs::{File, OpenOptions};
use std::io::Write as ioWrite;
use std::path::PathBuf;
use std::{env, fs};
use tokio::sync::mpsc::Sender;
use tokio::sync::{broadcast, mpsc, watch};
use tokio::time::Instant;

const COLOR_PLANNING: &str = "red";
const COLOR_EXECUTION: &str = "blue";
const COLOR_DEFAULT: &str = "black";

const TASK_NAME: &str = "task_name";
const TASK_STATUS: &str = "task_status";
const TASK_EXECUTION_TIME: &str = "task_exec_time";
const OMPAS_STATS: &str = "ompas_stats";

struct Reservation {
    id: ActingProcessId,
    resource: String,
    quantity: Quantity,
    priority: WaiterPriority,
}

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProcessKind {
    Method,
    Acquire,
    Arbitrary,
    Action,
    RootTask,
}

impl Display for ProcessKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ProcessKind::Action => ACTION,
                ProcessKind::Method => METHOD,
                ProcessKind::Acquire => ACQUIRE,
                ProcessKind::Arbitrary => ARBITRARY,
                ProcessKind::RootTask => ROOT_TASK,
            }
        )
    }
}

pub struct PlannerManager {
    update_notifier: Sender<ActingUpdateNotification>,
    watcher: (broadcast::Sender<bool>, broadcast::Receiver<bool>),
}

impl PlannerManager {
    pub fn new(update_notifier: mpsc::Sender<ActingUpdateNotification>) -> Self {
        let (tx, rx) = broadcast::channel(TOKIO_CHANNEL_SIZE);
        Self {
            update_notifier,
            watcher: (tx, rx),
        }
    }
}

pub struct InnerActingManager {
    pub(in crate::ompas::manager::acting) processes: Vec<ActingProcess>,
    pub(in crate::ompas::manager::acting) models: Vec<ActingModel>,
    acting_vars: ActingVarCollection,
    pub(in crate::ompas::manager::acting) st: RefSymTable,
    resource_manager: ResourceManager,
    time_reference: Instant,
    planner_manager: Option<PlannerManager>,
}

impl InnerActingManager {
    pub fn new(
        resource_manager: ResourceManager,
        time_reference: Instant,
        st: RefSymTable,
    ) -> Self {
        let model = ActingModel::root(st.clone());
        let chronicle = model.chronicle.as_ref().unwrap();
        let mut acting_vars = ActingVarCollection::default();

        let start =
            acting_vars.new_acting_var(ActingVarRef::new(chronicle.interval.get_start(), 0));
        let end = acting_vars.new_acting_var(ActingVarRef::new(chronicle.interval.get_end(), 0));

        let root = ActingProcess::new(
            0,
            0,
            ProcessOrigin::Execution,
            0,
            Some(ROOT.to_string()),
            ExecutionVar::new_with_ref(start),
            ExecutionVar::new_with_ref(end),
            RootProcess::new(),
        );
        Self {
            st,
            processes: vec![root],
            models: vec![model],
            resource_manager,
            time_reference,
            acting_vars,
            planner_manager: None,
        }
    }

    pub fn set_planner_manager(&mut self, planner_manager: PlannerManager) {
        self.planner_manager = Some(planner_manager)
    }

    pub async fn notify_planner(&mut self, notification: ActingUpdateNotification) {
        if let Some(notifier) = &self.planner_manager {
            notifier
                .update_notifier
                .send(notification)
                .await
                .unwrap_or_else(|_| panic!());
        }
    }

    pub async fn notify_plan_update(&mut self) {
        if let Some(notifier) = &self.planner_manager {
            notifier.watcher.0.send(true).unwrap_or_else(|_| panic!());
        }
    }

    pub async fn subscribe_on_plan_update(&self) -> Option<broadcast::Receiver<bool>> {
        self.planner_manager
            .as_ref()
            .map(|pm| pm.watcher.0.subscribe())
    }

    pub async fn clear(&mut self) {
        self.processes.clear();
        self.models.clear();
        self.acting_vars.clear().await;
        self.st = RefSymTable::default();
    }

    pub fn st(&self) -> RefSymTable {
        self.st.clone()
    }

    pub fn instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_millis().into()
    }

    fn new_model(&mut self, model: ActingModel) -> AMId {
        self.models.push(model);
        self.models.len() - 1
    }

    fn new_acting_var(&mut self, var_ref: ActingVarRef) -> ActingVarId {
        self.acting_vars.new_acting_var(var_ref)
    }

    fn new_execution_var<T: Display + Clone + AsCst>(
        &mut self,
        var_id: &VarId,
        om_id: &AMId,
    ) -> ExecutionVar<T> {
        ExecutionVar::new_with_ref(self.new_acting_var(ActingVarRef::new(*var_id, *om_id)))
    }

    pub fn get_id(&self, pr: impl Into<ProcessRef>) -> Option<ActingProcessId> {
        let pr = pr.into();
        match pr {
            ProcessRef::Id(id) => Some(id),
            ProcessRef::Relative(id, mut labels) => {
                let mut id = id;
                labels.reverse();
                while let Some(label) = labels.pop() {
                    let obj = &self.processes[id];
                    match &label {
                        Label::Action(s) => {
                            if id == 0 {
                                id = obj.inner.as_root().unwrap().nth_task(*s)?;
                            } else {
                                id = if let Some(id) =
                                    obj.inner.as_method().unwrap().childs.get(&label)
                                {
                                    *id
                                } else {
                                    return None;
                                }
                            }
                        }
                        Label::Refinement(m) => {
                            let task = obj.inner.as_action().unwrap();
                            id = match m {
                                Some(m) => {
                                    if let Some(r) = task.refinements.get(*m) {
                                        *r
                                    } else {
                                        return None;
                                    }
                                }
                                None => {
                                    if let Some(id) = task.refinements.last() {
                                        *id
                                    } else {
                                        return None;
                                    }
                                }
                            }
                        }
                        Label::Acquire(_) | Label::Arbitrary(_) => {
                            id = if let Some(id) = obj.inner.as_method().unwrap().childs.get(&label)
                            {
                                *id
                            } else {
                                return None;
                            }
                        }
                    }
                }
                Some(id)
            }
        }
    }

    pub fn get_kind(&self, id: &ActingProcessId) -> ProcessKind {
        self.processes[*id].inner.kind()
    }

    pub fn get_status(&self, id: &ActingProcessId) -> ProcessStatus {
        self.processes[*id].status
    }

    pub fn get_origin(&self, id: &ActingProcessId) -> ProcessOrigin {
        self.processes[*id].origin
    }

    pub fn get_debug(&self, id: &ActingProcessId) -> &Option<String> {
        self.processes[*id].debug()
    }

    fn get_am_id(&self, id: &ActingProcessId) -> AMId {
        self.processes[*id].am_id()
    }

    async fn set_execution_val(&mut self, val: ActingValUpdate) {
        let ActingValUpdate { plan_var_id, val } = val;
        let var = &mut self.acting_vars.plan_vars[plan_var_id];
        var.set_execution_val(val.clone());
        for ActingVarRef { var_id, am_id } in var.refs().clone() {
            self.models[am_id]
                .instantiations
                .push(Instantiation::new(var_id, self.st.new_cst(val.clone())))
        }
        self.notify_planner(ActingUpdateNotification::VarUpdate(plan_var_id))
            .await;
    }

    async fn set_execution_vals(&mut self, vals: Vec<ActingValUpdate>) {
        for val in vals {
            self.set_execution_val(val).await;
        }
    }

    #[inline]
    fn set_planner_val(&mut self, val: ActingValUpdate) {
        self.acting_vars.plan_vars[val.plan_var_id].set_planned_val(val.val)
    }

    fn set_planner_vals(&mut self, vals: Vec<ActingValUpdate>) {
        for val in vals {
            self.set_planner_val(val);
        }
    }

    fn get_plan_var_val(&self, plan_var_id: &ActingVarId) -> &ActingVal {
        self.acting_vars.plan_vars[*plan_var_id].get_val()
    }

    pub async fn set_start(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.instant());
        let val = self.processes[*id].start.set_val(instant);
        if let Some(val) = val {
            self.set_execution_vals(vec![val]).await;
        }
    }

    #[async_recursion]
    pub async fn set_end(
        &mut self,
        id: &ActingProcessId,
        t: Option<Timepoint>,
        status: ProcessStatus,
    ) {
        let instant = t.unwrap_or(self.instant());
        let val = self.processes[*id].end.set_val(instant);
        if let Some(val) = val {
            self.set_execution_vals(vec![val]).await;
        }

        if self.get_kind(id) == ProcessKind::Action {
            if let Some(refinement) = self.processes[*id]
                .inner
                .as_action()
                .unwrap()
                .refinements
                .last()
                .cloned()
            {
                self.set_end(&refinement, Some(instant), status).await;
            }
        }
        self.set_status(id, status)
    }

    pub fn get_task_args(&self, id: &ActingProcessId) -> Vec<Cst> {
        self.processes[*id].inner.as_action().unwrap().get_args()
    }

    pub async fn set_action_args(&mut self, id: &ActingProcessId, args: Vec<Cst>) {
        let action = self.processes[*id].inner.as_mut_action().unwrap();
        let updates = action.set_args(args);
        self.set_execution_vals(updates).await
    }

    pub async fn set_moment(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.instant());
        self.set_start(id, Some(instant)).await;
        self.set_end(id, Some(instant), self.get_status(id)).await;
    }

    pub fn set_status(&mut self, id: &ActingProcessId, status: ProcessStatus) {
        self.processes[*id].set_status(status);
        if self.get_kind(id) == ProcessKind::Action {
            if let Some(refinement) = self.processes[*id]
                .inner
                .as_action()
                .unwrap()
                .refinements
                .last()
                .cloned()
            {
                self.set_status(&refinement, status);
            }
        }
    }

    //New processes
    pub async fn new_high_level_task(&mut self, debug: String, mut args: Vec<Cst>) -> ProcessRef {
        let id = self.processes.len();
        let task_ref = self.models[0].add_subtask(args.clone());

        let start = self.new_execution_var(&task_ref.start, &0);
        let end = self.new_execution_var(&task_ref.end, &0);

        let mut new_args = vec![];

        for (val, arg_id) in args.drain(..).zip(task_ref.name) {
            let mut arg = self.new_execution_var(&arg_id, &0);
            self.set_execution_val(arg.set_val(val).unwrap()).await;
            new_args.push(arg)
        }

        self.processes.push(ActingProcess::new(
            id,
            0,
            ProcessOrigin::Execution,
            0,
            Some(debug),
            start,
            end,
            ActionProcess::new(new_args),
        ));

        let root: &mut RootProcess = self.processes[0].inner.as_mut_root().unwrap();
        let rank = root.n_task();
        root.add_top_level_task(id);
        self.notify_planner(ActingUpdateNotification::NewProcess(id))
            .await;

        ProcessRef::Relative(0, vec![Label::Action(rank)])
    }

    //Task methods
    pub async fn new_action(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        mut args: Vec<Option<Cst>>,
        debug: String,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();

        let om_id = self.processes[*parent].am_id();
        let model = &self.models[om_id];
        let (start, end, name) = if let Some(chronicle) = &model.chronicle {
            let binding = chronicle
                .bindings
                .get_binding(&label)
                .unwrap()
                .as_action()
                .unwrap()
                .clone();
            let name = binding.name;
            let start = self.new_execution_var(&binding.interval.get_start(), &om_id);
            let end = self.new_execution_var(&binding.interval.get_end(), &om_id);
            let mut new_name = vec![];

            for (cst, var_id) in args.drain(..).zip(name) {
                let mut exec = self.new_execution_var(&var_id, &om_id);
                if let Some(val) = cst {
                    let update = exec.set_val(val).unwrap();
                    if origin == ProcessOrigin::Planner {
                        self.set_planner_val(update)
                    } else {
                        self.set_execution_val(update).await
                    }
                }
                new_name.push(exec)
            }
            (start, end, new_name)
        } else {
            let start = ExecutionVar::new();
            let end = ExecutionVar::new();
            let name = args
                .drain(..)
                .map(|cst| {
                    let mut exec = ExecutionVar::new();
                    if let Some(val) = cst {
                        exec.set_val(val);
                    }
                    exec
                })
                .collect();
            (start, end, name)
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            Some(debug),
            start,
            end,
            ActionProcess::new(name),
        ));

        self.processes[*parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        if origin != ProcessOrigin::Planner {
            /*self.notify_planner(ActingUpdateNotification::NewProcess(id))
            .await;*/
        }
        id
    }

    pub async fn new_refinement(
        &mut self,
        parent: &ActingProcessId,
        debug: String,
        mut args: Vec<Option<Cst>>,
        model: ActingModel,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();
        let am_id = self.new_model(model);
        let model = &self.models[am_id];

        let (start, end, args) = if let Some(chronicle) = &model.chronicle {
            let name = chronicle.get_name().clone();
            let interval = chronicle.interval;
            let start = self.new_execution_var(&interval.get_start(), &am_id);
            let end = self.new_execution_var(&interval.get_end(), &am_id);
            let mut new_name = vec![];

            for (cst, var_id) in args.drain(..).zip(name) {
                let mut exec = self.new_execution_var(&var_id, &am_id);
                if let Some(val) = cst {
                    let update = exec.set_val(val).unwrap();
                    if origin == ProcessOrigin::Planner {
                        self.set_planner_val(update)
                    } else {
                        self.set_execution_val(update).await
                    }
                }
                new_name.push(exec)
            }
            (start, end, new_name)
        } else {
            let start = ExecutionVar::new();
            let end = ExecutionVar::new();
            let name = args
                .drain(..)
                .map(|cst| {
                    let mut exec = ExecutionVar::new();
                    if let Some(val) = cst {
                        exec.set_val(val);
                    }
                    exec
                })
                .collect();
            (start, end, name)
        };
        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            am_id,
            Some(debug),
            start,
            end,
            RefinementProcess::new(args),
        ));

        let action: &mut ActionProcess = self.processes[*parent].inner.as_mut_action().unwrap();
        action.add_refinement(id);
        if origin != ProcessOrigin::Planner {
            /*self.notify_planner(ActingUpdateNotification::NewProcess(id))
            .await;*/
        }
        id
    }

    pub async fn new_arbitrary(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();

        let om_id = self.processes[*parent].am_id();
        let model = &self.models[om_id];
        let (start, end, var) = match &model.chronicle {
            Some(chronicle) => {
                let binding = chronicle
                    .bindings
                    .get_binding(&label)
                    .unwrap()
                    .as_arbitrary()
                    .unwrap();
                let timepoint = binding.timepoint;
                let var_id = binding.var_id;

                let start: ExecutionVar<Timepoint> = self.new_execution_var(&timepoint, &om_id);
                let end = start.clone();
                let var = self.new_execution_var(&var_id, &om_id);
                (start, end, var)
            }
            None => Default::default(),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            None,
            start,
            end,
            ArbitraryProcess::new(var),
        ));

        self.processes[*parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        if origin != ProcessOrigin::Planner {
            /*self.notify_planner(ActingUpdateNotification::NewProcess(id))
            .await;*/
        }
        id
    }

    pub async fn new_acquire(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();
        let om_id = self.processes[*parent].am_id();
        let model = &self.models[om_id];
        let (start, end, s_acq, resource, quantity) = match &model.chronicle {
            Some(chronicle) => {
                let binding = *chronicle
                    .bindings
                    .get_binding(&label)
                    .unwrap()
                    .as_acquire()
                    .unwrap();

                let start = self.new_execution_var(&binding.request, &om_id);
                let s_acq = self.new_execution_var(&binding.acquisition.get_start(), &om_id);
                let end = self.new_execution_var(&binding.acquisition.get_end(), &om_id);
                let resource = self.new_execution_var(&binding.resource, &om_id);
                let quantity = self.new_execution_var(&binding.quantity, &om_id);
                (start, end, s_acq, resource, quantity)
            }
            None => Default::default(),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            None,
            start,
            end,
            AcquireProcess::new(resource, quantity, s_acq),
        ));
        self.processes[*parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        if origin != ProcessOrigin::Planner {
            /*self.notify_planner(ActingUpdateNotification::NewProcess(id))
            .await;*/
        }
        id
    }

    pub fn subscribe(&mut self, id: &ActingProcessId) -> watch::Receiver<ProcessStatus> {
        let p = &mut self.processes[*id];
        match &p.status_update {
            Some(update) => update.subscribe(),
            None => {
                let (tx, rx) = watch::channel(ProcessStatus::Pending);
                p.status_update = Some(tx);
                rx
            }
        }
    }

    /*
    Get process inner struct
     */
    pub fn get(&self, process_ref: impl Into<ProcessRef>) -> Option<&ActingProcess> {
        self.get_id(process_ref.into())
            .map(|id| &self.processes[id])
    }

    pub fn get_mut(&mut self, process_ref: impl Into<ProcessRef>) -> Option<&mut ActingProcess> {
        self.get_id(process_ref.into())
            .map(|id| &mut self.processes[id])
    }

    pub fn get_last_planned_refinement(&self, id: &ActingProcessId) -> Option<ActingProcessId> {
        let refinements = &self.processes[*id].inner.as_action().unwrap().refinements;
        //println!("{}.refinements = {:?}", id, refinements);
        match refinements.last() {
            Some(id) => {
                let origin = self.get_origin(id);
                //println!("{id}.origin = {:?}", origin);
                if origin == ProcessOrigin::Planner {
                    Some(*id)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub fn get_last_executed_refinement(&self, id: &ActingProcessId) -> Option<ActingProcessId> {
        match self.processes[*id]
            .inner
            .as_action()
            .unwrap()
            .refinements
            .last()
        {
            Some(id) => {
                if self.get_origin(id) == ProcessOrigin::Execution {
                    Some(*id)
                } else {
                    None
                }
            }
            None => None,
        }
    }

    pub fn executed(&mut self, id: &ActingProcessId) {
        let origin = &mut self.processes[*id].origin;
        if ProcessOrigin::Planner == *origin {
            *origin = ProcessOrigin::ExecPlanInherited
        }
    }

    pub fn dropped(&mut self, id: &ActingProcessId) {
        let origin = &mut self.processes[*id].origin;
        if ProcessOrigin::Planner == *origin {
            *origin = ProcessOrigin::PlannerDropped
        }
    }

    pub fn get_lv(&self, id: &ActingProcessId) -> LValue {
        if self.get_kind(id) == ProcessKind::Method {
            let om_id = self.get_am_id(id);
            self.models[om_id].lv.clone()
        } else {
            panic!()
        }
    }

    pub fn get_refinement_lv(&self, id: &ActingProcessId) -> LValue {
        self.processes[*id]
            .inner
            .as_method()
            .unwrap()
            .get_name_as_lvalue()
    }

    pub fn get_om(&self, id: &ActingProcessId) -> LValue {
        if self.get_kind(id) == ProcessKind::Method {
            let om_id = self.get_am_id(id);
            self.models[om_id].lv_om.clone()
        } else {
            panic!()
        }
    }

    pub fn get_tried(&self, id: &ActingProcessId) -> Vec<LValue> {
        let refinements = self.processes[*id]
            .inner
            .as_action()
            .unwrap()
            .get_refinements();

        refinements
            .iter()
            .filter_map(|id| {
                if self.processes[*id].origin.is_exec() {
                    Some(self.processes[*id].am_id())
                } else {
                    None
                }
            })
            .map(|id| self.models[id].lv.clone())
            .collect()
    }

    pub async fn set_s_acq(&mut self, id: &ActingProcessId, instant: Option<Timepoint>) {
        let instant = instant.unwrap_or(self.instant());
        let val = self.processes[*id]
            .inner
            .as_mut_acquire()
            .unwrap()
            .set_s_acq(instant);
        if let Some(val) = val {
            self.set_execution_val(val).await;
        }
    }

    pub async fn set_arbitrary_value(
        &mut self,
        id: &ActingProcessId,
        set: Vec<LValue>,
        greedy: LValue,
    ) -> LValue {
        let plan_var_id = self.processes[*id]
            .inner
            .as_arbitrary()
            .unwrap()
            .get_plan_var_id();
        let value = if let Some(plan_var_id) = plan_var_id {
            match self.get_plan_var_val(plan_var_id) {
                ActingVal::Planned(val) => {
                    let value: LValue = val.clone().into();
                    if set.contains(&value) {
                        println!("{}.arb = {}", id, value);
                        value
                    } else {
                        greedy
                    }
                }
                ActingVal::None => greedy,
                _ => unreachable!(),
            }
        } else {
            greedy
        };

        let arbitrary = self.processes[*id].inner.as_mut_arbitrary().unwrap();
        arbitrary.set_set(set);
        if let Some(val) = arbitrary.set_var(value.clone()) {
            self.set_execution_vals(vec![val]).await;
        }
        self.executed(id);
        value
    }

    pub async fn acquire(
        &mut self,
        id: &ActingProcessId,
        resource: String,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        let mut updates: Vec<ActingValUpdate> = vec![];
        self.executed(id);
        let acquire = self.processes[*id].inner.as_mut_acquire().unwrap();
        let r = match acquire.move_reservation() {
            Some(reservation) => {
                println!("{} reserved", resource);
                Ok(reservation)
            }
            None => {
                if let Some(val) = acquire.set_resource(resource.to_string()) {
                    updates.push(val);
                }
                let waiter: WaitAcquire = self
                    .resource_manager
                    .acquire(&resource, quantity, priority)
                    .await?;

                let quantity = self
                    .resource_manager
                    .get_client_quantity(&waiter.get_resource_id(), &waiter.get_client_id())
                    .await;

                if let Some(val) = acquire.set_quantity(quantity) {
                    updates.push(val);
                }

                acquire.set_acquire_id(&waiter);
                Ok(waiter)
            }
        };

        self.set_execution_vals(updates).await;

        r
    }

    async fn reserve(
        &mut self,
        Reservation {
            id,
            resource,
            quantity,
            priority,
        }: Reservation,
    ) -> Result<(), LRuntimeError> {
        let acquire = self.processes[id].inner.as_mut_acquire().unwrap();
        acquire.set_reservation(
            self.resource_manager
                .reserve(&resource, quantity, priority)
                .await?,
        );
        Ok(())
    }

    async fn reserve_all(&mut self, reservations: Vec<Reservation>) {
        let mut resources: HashSet<String> = Default::default();
        for reservation in reservations {
            resources.insert(reservation.resource.to_string());
            self.reserve(reservation).await.unwrap();
        }
        for resource in resources {
            self.resource_manager.update_queue(&resource).await.unwrap();
        }
    }

    pub fn get_current_chronicles(&self) -> Vec<ChronicleInstance> {
        struct ExecChronicle {
            id: ActingProcessId,
            origin: ChronicleOrigin,
        }

        let mut exec_chronicles = vec![ExecChronicle {
            id: 0,
            origin: ChronicleOrigin::Original,
        }];

        let mut chronicles = vec![];
        'main: while let Some(ExecChronicle { id, origin }) = exec_chronicles.pop() {
            let instance_id = chronicles.len();
            let process = &self.processes[id];
            let chronicle: Chronicle = match &process.inner {
                ActingProcessInner::RootTask(root) => {
                    for (task_id, task) in root.tasks.iter().enumerate() {
                        exec_chronicles.insert(
                            0,
                            ExecChronicle {
                                id: *task,
                                origin: ChronicleOrigin::Refinement {
                                    instance_id,
                                    task_id,
                                },
                            },
                        );
                    }
                    self.models[process.am_id()]
                        .get_instantiated_chronicle()
                        .unwrap()
                }
                ActingProcessInner::Action(a) => {
                    //Verify if we take the model of the action of the refinement
                    if let Some(am_id) = &a.abstract_am_id {
                        //An abstract model is used, meaning that no subtask is present.
                        self.models[*am_id].chronicle.clone().unwrap()
                    }
                    //Otherwise we check if there is a refinement
                    else if let Some(&refinement) = a.refinements.last() {
                        let method_process = &self.processes[refinement];
                        let model = self.models[method_process.get_am_id()]
                            .get_instantiated_chronicle()
                            .unwrap();
                        let method = method_process.inner.as_method().unwrap();
                        for (label, id) in &method.childs {
                            if matches!(label, Label::Action(_)) {
                                let action_binding = model
                                    .bindings
                                    .get_binding(label)
                                    .unwrap()
                                    .as_action()
                                    .unwrap();
                                exec_chronicles.insert(
                                    0,
                                    ExecChronicle {
                                        id: *id,
                                        origin: ChronicleOrigin::Refinement {
                                            instance_id,
                                            task_id: action_binding.task_id,
                                        },
                                    },
                                );
                            }
                        }
                        model
                    } else {
                        continue 'main;
                    }
                }
                _ => panic!(),
            };
            chronicles.push(ChronicleInstance {
                generated: false,
                origin,
                am: ActingModel {
                    lv: LValue::Nil,
                    lv_om: LValue::Nil,
                    lv_expanded: Some(LValue::Nil),
                    instantiations: vec![],
                    chronicle: Some(chronicle),
                },
                pr: ProcessRef::Id(id),
            })
        }
        //Add all chronicles of the problem

        chronicles
    }

    pub fn export_trace_dot_graph(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();
        let mut queue = vec![0];

        while let Some(id) = queue.pop() {
            let ap = &self.processes[id];
            let label = format_acting_process(&self.acting_vars, ap);
            let color = match ap.origin {
                ProcessOrigin::Planner => COLOR_EXECUTION,
                ProcessOrigin::Execution => COLOR_PLANNING,
                _ => COLOR_DEFAULT,
            };

            writeln!(dot, "P{id} [label = \"{label}\", color = {color}];").unwrap();
            match &ap.inner {
                ActingProcessInner::RootTask(rt) => {
                    for st in &rt.tasks {
                        writeln!(dot, "P{id} -> P{st};").unwrap();
                        queue.push(*st)
                    }
                }
                ActingProcessInner::Action(t) => {
                    for r in &t.refinements {
                        writeln!(dot, "P{id} -> P{};", r).unwrap();
                        queue.push(*r)
                    }
                }
                ActingProcessInner::Method(m) => {
                    for sub in m.childs.values() {
                        writeln!(dot, "P{id} -> P{sub};").unwrap();
                        queue.push(*sub)
                    }
                }
                ActingProcessInner::Arbitrary(_) => {}
                ActingProcessInner::Acquire(_) => {}
            }
        }

        dot.push('}');
        dot
    }

    pub fn dump_trace(&self, path: Option<PathBuf>) {
        let mut path = match path {
            None => "/tmp".into(),
            Some(p) => p,
        };
        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
        path.push(format!("supervisor-trace_{}", string_date));
        fs::create_dir_all(&path).unwrap();

        let mut path_dot = path.clone();
        let dot_file_name = "trace.dot";
        path_dot.push(dot_file_name);
        let mut file = File::create(&path_dot).unwrap();
        let dot = self.export_trace_dot_graph();
        file.write_all(dot.as_bytes()).unwrap();
        set_current_dir(&path).unwrap();
        let trace = "trace.png";
        std::process::Command::new("dot")
            .args(["-Tpng", dot_file_name, "-o", trace])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut md_path = path.clone();
        let md_file_name = "trace.md";
        md_path.push(md_file_name);
        let mut md_file = File::create(&md_path).unwrap();
        let md: String = format!(
            "
## Trace
\n
![]({})
\n",
            trace
        );

        md_file.write_all(md.as_bytes()).unwrap();

        std::process::Command::new("google-chrome")
            .arg(md_file_name)
            .spawn()
            .unwrap();
    }

    pub(crate) async fn add_processes_from_chronicles(
        &mut self,
        instances: Vec<ChronicleInstance>,
    ) {
        for instance in instances {
            if instance.generated {
                let chronicle = instance.am.chronicle.as_ref().unwrap();
                let bindings = chronicle.bindings.clone();
                let st = chronicle.st.clone();
                let mut pr = instance.pr.clone();
                if chronicle.meta_data.kind == ChronicleKind::Method {
                    let _ = pr.pop().unwrap();
                }
                let parent = match self.get_id(pr.clone()) {
                    Some(id) => id,
                    None =>
                    //We need to create the task before adding the method
                    {
                        let label = pr.pop().unwrap();
                        let parent = self.get_id(pr).unwrap();
                        let task = chronicle.get_task();
                        let debug = task.format(&st, true);
                        let mut args: Vec<Option<Cst>> = chronicle
                            .get_task()
                            .iter()
                            .map(|var_id| st.var_as_cst(var_id))
                            .collect();
                        match parent {
                            0 => {
                                let pr = self
                                    .new_high_level_task(
                                        debug,
                                        args.drain(..).map(|c| c.unwrap()).collect(),
                                    )
                                    .await;
                                let id = self.get_id(pr).unwrap();
                                self.processes[id].origin = ProcessOrigin::Planner;
                                id
                            }
                            _ => {
                                self.new_action(label, &parent, args, debug, ProcessOrigin::Planner)
                                    .await
                            }
                        }
                    }
                };

                let debug = chronicle.get_name().format(&st, true);

                match chronicle.meta_data.kind {
                    ChronicleKind::Command | ChronicleKind::Task => {
                        let am_id = self.new_model(instance.am);
                        self.processes[parent]
                            .inner
                            .as_mut_action()
                            .unwrap()
                            .abstract_am_id = Some(am_id);
                    }
                    ChronicleKind::Method => {
                        let args: Vec<Option<Cst>> = chronicle
                            .get_name()
                            .iter()
                            .map(|var_id| st.var_as_cst(var_id))
                            .collect();
                        let parent = self
                            .new_refinement(
                                &parent,
                                debug,
                                args,
                                instance.am,
                                ProcessOrigin::Planner,
                            )
                            .await;

                        for (label, binding) in bindings.inner {
                            match binding {
                                ActingBinding::Arbitrary(_arbitrary) => {
                                    let _id = self
                                        .new_arbitrary(label, &parent, ProcessOrigin::Planner)
                                        .await;
                                }
                                ActingBinding::Action(action_binding) => {
                                    let action = &action_binding.name;
                                    let debug = action.format(&st, true);
                                    let args: Vec<_> =
                                        action.iter().map(|var_id| st.var_as_cst(var_id)).collect();
                                    let _id = self
                                        .new_action(
                                            label,
                                            &parent,
                                            args,
                                            debug,
                                            ProcessOrigin::Planner,
                                        )
                                        .await;
                                }
                                ActingBinding::Acquire(_acq) => {
                                    let _id = self
                                        .new_acquire(label, &parent, ProcessOrigin::Planner)
                                        .await;
                                }
                            }
                        }
                    }
                    _ => {}
                }
            }
        }
    }

    pub(crate) async fn absorb_choices(&mut self, choices: Vec<Choice>) {
        let mut updates: Vec<ActingValUpdate> = vec![];
        let mut reservations: Vec<Reservation> = vec![];

        let mut update = |plan_var_id: &Option<ActingVarId>, val: Cst| {
            updates.push(ActingValUpdate {
                plan_var_id: plan_var_id.unwrap(),
                val,
            })
        };

        for Choice {
            process_ref,
            choice_inner,
        } in choices
        {
            let id = self
                .get_id(process_ref.clone())
                .unwrap_or_else(|| panic!("path {process_ref} does not exist."));
            let process = &self.processes[id];
            match choice_inner {
                ChoiceInner::Arbitrary(a) => {
                    let arb = process.inner.as_arbitrary().unwrap();
                    update(arb.get_plan_var_id(), a.val);
                }
                ChoiceInner::Acquire(a) => {
                    reservations.push(Reservation {
                        id,
                        resource: a.resource.to_string(),
                        quantity: Quantity::Some(a.quantity.as_int().unwrap() as usize),
                        priority: a.priority,
                    });
                    let acq = process.inner.as_acquire().unwrap();
                    update(acq.resource.get_plan_var_id(), a.resource);
                    update(acq.quantity.get_plan_var_id(), a.quantity);
                    update(acq.s_acq.get_plan_var_id(), a.s_acq);
                    update(process.start.get_plan_var_id(), a.request);
                    update(process.end.get_plan_var_id(), a.e_acq);
                }
                ChoiceInner::SubTask(s) => {
                    update(process.start.get_plan_var_id(), s.start);
                    update(process.end.get_plan_var_id(), s.end);
                }
                ChoiceInner::Refinement(r) => {
                    update(process.start.get_plan_var_id(), r.start);
                    update(process.end.get_plan_var_id(), r.end);
                }
            }
        }
        self.reserve_all(reservations).await;
        self.set_planner_vals(updates)
    }

    pub async fn export_to_csv(&self, working_dir: Option<PathBuf>, file: Option<String>) {
        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

        let dir_path: PathBuf = match working_dir {
            Some(wd) => {
                let mut dir_path = wd;
                dir_path.push(OMPAS_STATS);
                dir_path
            }
            None => format!(
                "{}/ompas/{}",
                match env::var("HOME") {
                    Ok(val) => val,
                    Err(_) => ".".to_string(),
                },
                OMPAS_STATS
            )
            .into(),
        };

        fs::create_dir_all(&dir_path).expect("could not create stats directory");
        let mut file_path = dir_path;
        file_path.push(match file {
            Some(f) => format!("{}.csv", f),
            None => format!("{}{}.csv", OMPAS_STATS, string_date),
        });
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&file_path)
            .expect("error creating stat file");
        let header = format!(
            "\"{}\";\"{}\";\"{}\"\n",
            TASK_NAME, TASK_STATUS, TASK_EXECUTION_TIME,
        );
        if file.metadata().unwrap().len() == 0 {
            file.write_all(header.as_bytes())
                .expect("could not write to stat file");
        }
        let parent: Vec<ActingProcessId> = self.processes[0].inner.as_root().unwrap().tasks.clone();
        for p in &parent {
            let process = &self.processes[*p];
            file.write_all(
                format!(
                    "\"{}\";\"{}\";\"{}\"\n", //";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\"\n",
                    process.debug().clone().unwrap(),
                    process.status,
                    {
                        let start = process.start.get_val().unwrap();
                        let end = *process.end.get_val();
                        let interval = Interval::new(start, end);
                        let duration = interval.duration();
                        if duration.is_finite() {
                            duration.as_secs().to_string()
                        } else {
                            duration.to_string()
                        }
                    },
                )
                .as_bytes(),
            )
            .expect("could not write to stat file")
        }
    }
}

fn format_acting_process(
    planner_manager: &ActingVarCollection,
    acting_process: &ActingProcess,
) -> String {
    let mut f = String::new();
    write!(
        f,
        "({}, {})[{},{}]",
        acting_process.id(),
        acting_process.status,
        planner_manager.format_execution_var(&acting_process.start),
        planner_manager.format_execution_var(&acting_process.end),
    )
    .unwrap();
    let debug = if let Some(debug) = acting_process.debug() {
        debug.to_string()
    } else {
        "".to_string()
    };
    match &acting_process.inner {
        ActingProcessInner::RootTask(_) => {
            write!(f, "root").unwrap();
        }
        ActingProcessInner::Action(_) => {
            write!(f, "{}", debug).unwrap();
        }
        ActingProcessInner::Method(_) => {
            write!(f, "{}", debug).unwrap();
        }
        ActingProcessInner::Arbitrary(arb) => {
            write!(f, "arb({})", planner_manager.format_execution_var(&arb.var)).unwrap();
        }
        ActingProcessInner::Acquire(acq) => {
            write!(
                f,
                "{}: acq({},{})",
                planner_manager.format_execution_var(&acq.s_acq),
                planner_manager.format_execution_var(&acq.resource),
                planner_manager.format_execution_var(&acq.quantity)
            )
            .unwrap();
        }
    }
    f
}

/*
impl InnerSupervisor {
    pub async fn clear(&self) {
        *self.trc.inner.write().await = Default::default();
        *self.tn.subtasks.write().await = Default::default();
        loop {
            let id = self.next_id.load(Ordering::Relaxed);
            if self
                .next_id
                .compare_exchange(id, id + 1, Ordering::Acquire, Ordering::Relaxed) //Equivalent to compare_and_swap
                .is_ok()
            {
                break;
            }
        }
    }

    /*
    GETTERS
     */
    pub async fn get_refinement_method(&self, id: &ActionId) -> Option<SelectMode> {
        if let ActionMetaData::Task(task) = self.trc.get(id).await {
            let r = task.get_last_refinement();
            r.map(|ok| ok.refinement_type)
        } else {
            None
        }
    }

    pub async fn get_execution_time(&self, id: &ActionId) -> Duration {
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        task.get_duration()
    }

    pub async fn get_number_of_subtasks(&self, id: &ActionId) -> usize {
        self.tn.get_subtasks(id).await.len()
    }

    pub async fn get_number_of_subtasks_recursive(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            n += 1;
            subtasks.append(&mut self.tn.get_subtasks(&c).await);
        }

        n
    }

    pub async fn get_number_of_abstract_tasks(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(id).await {
                n += 1;
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_number_of_actions(&self, id: &ActionId) -> usize {
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        let mut n = 0;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_command(&c).await {
                n += 1;
            } else {
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
            }
        }
        n
    }

    pub async fn get_total_number_of_refinement(&self, id: &ActionId) -> usize {
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        let mut n = task.get_number_of_refinement();
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(&c).await {
                let task: TaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                n += task.get_number_of_refinement();
            }
        }
        n
    }

    pub async fn get_total_refinement_time(&self, id: &ActionId) -> Duration {
        let mut total_time: Duration = Duration::Finite(0);
        let task: TaskMetaData = self.trc.get(id).await.try_into().unwrap();
        total_time += task.get_total_refinement_time();
        let mut subtasks: Vec<ActionId> = self.tn.get_subtasks(id).await;
        while let Some(c) = subtasks.pop() {
            if self.trc.is_task(&c).await {
                let task: TaskMetaData = self.trc.get(&c).await.try_into().unwrap();
                subtasks.append(&mut self.tn.get_subtasks(&c).await);
                total_time += task.get_total_refinement_time();
            }
        }
        total_time
    }

    pub async fn get_stats(&self) -> LValue {
        let mut map: im::HashMap<LValue, LValue> = Default::default();
        let task_collection: im::HashMap<ActionId, ActionMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<ActionId> = self.tn.get_parents().await;
        for p in &parent {
            let mut task_stats: im::HashMap<LValue, LValue> = Default::default();
            task_stats.insert(
                string!(REFINEMENT_METHOD),
                match self.get_refinement_method(p).await {
                    Some(s) => s.to_string(),
                    None => "none".to_string(),
                }
                    .into(),
            );
            task_stats.insert(
                string!(REFINEMENT_NUMBER),
                self.get_total_number_of_refinement(p).await.into(),
            );
            task_stats.insert(
                string!(SUBTASK_NUMBER),
                self.get_number_of_subtasks_recursive(p).await.into(),
            );
            task_stats.insert(
                string!(TASK_EXECUTION_TIME),
                self.get_execution_time(p).await.to_string().into(),
            );
            task_stats.insert(
                string!(TASK_STATUS),
                self.get_status(p).await.to_string().into(),
            );
            task_stats.insert(
                string!(ACTION_NUMBER),
                self.get_number_of_actions(p).await.into(),
            );
            task_stats.insert(
                string!(TOTAL_REFINEMENT_TIME),
                self.get_total_refinement_time(p).await.to_string().into(),
            );
            map.insert(
                task_collection.get(p).unwrap().get_label(),
                task_stats.into(),
            );
        }

        map.into()
    }

    pub async fn export_to_csv(&self, working_dir: Option<PathBuf>, file: Option<String>) {
        let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
        let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();

        let dir_path: PathBuf = match working_dir {
            Some(wd) => {
                let mut dir_path = wd;
                dir_path.push(RAE_STATS);
                dir_path
            }
            None => format!(
                "{}/ompas/{}",
                match env::var("HOME") {
                    Ok(val) => val,
                    Err(_) => ".".to_string(),
                },
                RAE_STATS
            )
                .into(),
        };

        fs::create_dir_all(&dir_path).expect("could not create stats directory");
        let mut file_path = dir_path.clone();
        file_path.push(match file {
            Some(f) => format!("{}.csv", f),
            None => format!("{}{}.csv", RAE_STATS, string_date),
        });
        let mut file = OpenOptions::new()
            .append(true)
            .create(true)
            .open(&file_path)
            .expect("error creating stat file");
        let header = format!(
            "\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\"\n",
            TASK_NAME,
            TASK_STATUS,
            TASK_EXECUTION_TIME,
            REFINEMENT_METHOD,
            REFINEMENT_NUMBER,
            TOTAL_REFINEMENT_TIME,
            SUBTASK_NUMBER,
            ACTION_NUMBER
        );
        if file.metadata().unwrap().len() == 0 {
            file.write_all(header.as_bytes())
                .expect("could not write to stat file");
        }
        let task_collection: im::HashMap<ActionId, ActionMetaData> =
            self.trc.inner.read().await.clone();
        let parent: Vec<ActionId> = self.tn.get_parents().await;
        for p in &parent {
            file.write_all(
                format!(
                    "\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\";\"{}\"\n",
                    task_collection.get(p).unwrap().get_label(),
                    self.get_status(p).await,
                    {
                        let u: Duration = self.get_execution_time(p).await;
                        if u.is_finite() {
                            u.as_secs().to_string()
                        } else {
                            u.to_string()
                        }
                    },
                    match self.get_refinement_method(p).await {
                        Some(s) => s.to_string(),
                        None => "none".to_string(),
                    },
                    self.get_total_number_of_refinement(p).await,
                    self.get_total_refinement_time(p).await.as_secs(),
                    self.get_number_of_subtasks_recursive(p).await,
                    self.get_number_of_actions(p).await,
                )
                    .as_bytes(),
            )
                .expect("could not write to stat file")
        }
    }
    }
 */
