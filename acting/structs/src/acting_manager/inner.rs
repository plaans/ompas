use crate::acting_manager::action_status::ProcessStatus;
use crate::acting_manager::interval::Timepoint;
use crate::acting_manager::operational_model::{ActingModel, ROOT};
use crate::acting_manager::planner_manager::{ActingPlanResult, PlannerManager};
use crate::acting_manager::process::acquire::AcquireProcess;
use crate::acting_manager::process::arbitrary::ArbitraryProcess;
use crate::acting_manager::process::command::CommandProcess;
use crate::acting_manager::process::method::RefinementProcess;
use crate::acting_manager::process::plan_var::{
    ActingVal, AsCst, ExecutionVar, PlanVal, PlanVar, PlanVarId,
};
use crate::acting_manager::process::process_ref::{Label, ProcessRef};
use crate::acting_manager::process::root_task::RootProcess;
use crate::acting_manager::process::task::TaskProcess;
use crate::acting_manager::process::{ActingProcess, ActingProcessInner, ProcessOrigin};
use crate::acting_manager::{AMId, ActingProcessId};
use crate::conversion::flow_graph::graph::Dot;
use crate::execution::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::planning::instance::ChronicleInstance;
use crate::planning::om_binding::ChronicleBinding;
use crate::sym_table::domain::cst;
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::r#trait::FormatWithSymTable;
use crate::sym_table::VarId;
use aries_planning::chronicles::Chronicle;
use chrono::{DateTime, Utc};
use ompas_language::supervisor::*;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::env::set_current_dir;
use std::fmt::{Display, Formatter, Write};
use std::fs;
use std::fs::File;
use std::io::Write as ioWrite;
use std::path::PathBuf;
use tokio::sync::watch;
use tokio::time::Instant;

const COLOR_PLANNING: &str = "red";
const COLOR_EXECUTION: &str = "blue";
const COLOR_DEFAULT: &str = "black";

#[derive(Copy, Clone, Debug, PartialEq, Eq)]
pub enum ProcessKind {
    Method,
    Acquire,
    Arbitrary,
    Command,
    Task,
    RootTask,
}

impl Display for ProcessKind {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(
            f,
            "{}",
            match self {
                ProcessKind::Task => TASK,
                ProcessKind::Command => COMMAND,
                ProcessKind::Method => METHOD,
                ProcessKind::Acquire => ACQUIRE,
                ProcessKind::Arbitrary => ARBITRARY,
                ProcessKind::RootTask => ROOT_TASK,
            }
        )
    }
}

pub struct InnerActingManager {
    processes: Vec<ActingProcess>,
    models: Vec<ActingModel>,
    pub planner_manager: PlannerManager,
    st: RefSymTable,
    resource_manager: ResourceManager,
    time_reference: Instant,
}

impl InnerActingManager {
    pub fn new(
        resource_manager: ResourceManager,
        time_reference: Instant,
        st: RefSymTable,
    ) -> Self {
        let model = ActingModel::root(st.clone());
        let chronicle = model.chronicle.as_ref().unwrap();
        let start = PlanVar::new(chronicle.interval.get_start(), 0);
        let end = PlanVar::new(chronicle.interval.get_end(), 0);

        let root = ActingProcess::new(
            0,
            0,
            ProcessOrigin::Execution,
            0,
            Some(ROOT.to_string()),
            ExecutionVar::new(Some(0)),
            ExecutionVar::new(Some(1)),
            RootProcess::new(),
        );
        Self {
            st,
            processes: vec![root],
            models: vec![model],
            resource_manager,
            time_reference,
            planner_manager: PlannerManager {
                plan_vars: vec![start, end],
                bindings: Default::default(),
            },
        }
    }

    pub async fn clear(&mut self) {
        self.processes.clear();
        self.models.clear();
        self.planner_manager.clear().await;
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

    fn new_plan_var(&mut self, var_id: VarId, om_id: AMId) -> PlanVarId {
        self.planner_manager
            .plan_vars
            .push(PlanVar::new(var_id, om_id));
        self.planner_manager.plan_vars.len() - 1
    }

    fn new_execution_var<T: Display + Clone + AsCst>(
        &mut self,
        var_id: VarId,
        om_id: AMId,
    ) -> ExecutionVar<T> {
        ExecutionVar::new(Some(self.new_plan_var(var_id, om_id)))
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
                                id = if let Some(id) = obj.inner.as_root().unwrap().nth_task(*s) {
                                    id
                                } else {
                                    return None;
                                }
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
                            id = if let Some(r) = obj.inner.as_task().unwrap().refinements.get(*m) {
                                *r
                            } else {
                                return None;
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

    fn get_om_id(&self, id: &ActingProcessId) -> AMId {
        self.processes[*id].om_id()
    }

    fn set_plan_var(&mut self, val: Option<PlanVal>) {
        if let Some(val) = val {
            self.planner_manager.plan_vars[val.plan_var_id].set_execution_val(val.val)
        }
    }

    fn get_plan_var_val(&mut self, plan_var_id: &PlanVarId) -> &ActingVal {
        self.planner_manager.plan_vars[*plan_var_id].get_val()
    }

    pub fn set_start(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.instant());
        let val = self.processes[*id].start.set_val(instant);
        self.set_plan_var(val);
    }

    pub fn set_end(&mut self, id: &ActingProcessId, t: Option<Timepoint>, status: ProcessStatus) {
        let instant = t.unwrap_or(self.instant());
        let val = self.processes[*id].end.set_val(instant);
        self.set_plan_var(val);

        if self.get_kind(&id) == ProcessKind::Task {
            if let Some(refinement) = self.processes[*id]
                .inner
                .as_task()
                .unwrap()
                .refinements
                .last()
                .cloned()
            {
                self.set_end(&refinement, Some(instant), status);
            }
        }
        self.set_status(id, status)
    }

    pub fn get_task_args(&self, id: &ActingProcessId) -> &Vec<cst::Cst> {
        self.processes[*id].inner.as_task().unwrap().get_args()
    }

    pub fn set_moment(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.instant());
        self.set_start(id, Some(instant));
        self.set_end(id, Some(instant), self.get_status(id));
    }

    pub fn set_status(&mut self, id: &ActingProcessId, status: ProcessStatus) {
        self.processes[*id].set_status(status);
        if self.get_kind(&id) == ProcessKind::Task {
            if let Some(refinement) = self.processes[*id]
                .inner
                .as_task()
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
    pub fn new_high_level_task(&mut self, debug: String, args: Vec<Cst>) -> ProcessRef {
        let id = self.processes.len();
        let (start, end) = self.models[0].add_subtask(args.clone());

        let start = self.new_execution_var(start, 0);
        let end = self.new_execution_var(end, 0);

        self.processes.push(ActingProcess::new(
            id,
            0,
            ProcessOrigin::Execution,
            0,
            Some(debug),
            start,
            end,
            TaskProcess::new(args),
        ));

        let root: &mut RootProcess = self.processes[0].inner.as_mut_root().unwrap();
        let rank = root.n_task();
        root.add_top_level_task(id);

        ProcessRef::Relative(0, vec![Label::Action(rank)])
    }

    //Task methods
    pub fn new_task(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        args: Vec<Cst>,
        debug: String,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();

        let om_id = self.processes[*parent].om_id();
        let model = &self.models[om_id];
        let (start, end) = if let Some(chronicle) = &model.chronicle {
            let interval = chronicle
                .bindings
                .get_binding(&label)
                .unwrap()
                .as_action()
                .unwrap()
                .interval;
            let start = self.new_execution_var(interval.get_start(), om_id);
            let end = self.new_execution_var(interval.get_end(), om_id);
            (start, end)
        } else {
            let start = ExecutionVar::new(None);
            let end = ExecutionVar::new(None);
            (start, end)
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            Some(debug),
            start,
            end,
            TaskProcess::new(args),
        ));

        self.processes[*parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
        id
    }

    pub fn new_refinement(
        &mut self,
        parent: &ActingProcessId,
        debug: String,
        model: ActingModel,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();
        let interval = model.chronicle.as_ref().map(|c| c.interval);
        let om_id = self.new_model(model);
        let (start, end) = match interval {
            Some(interval) => {
                let start = self.new_execution_var(interval.get_start(), om_id);
                let end = self.new_execution_var(interval.get_end(), om_id);
                (start, end)
            }
            None => Default::default(),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            Some(debug),
            start,
            end,
            RefinementProcess::new(),
        ));

        let task: &mut TaskProcess = self.processes[*parent].inner.as_mut_task().unwrap();
        task.add_refinement(id);
        id
    }

    pub fn new_arbitrary(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();

        let om_id = self.processes[*parent].om_id();
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

                let start: ExecutionVar<Timepoint> = self.new_execution_var(timepoint, om_id);
                let end = start.clone();
                let var = self.new_execution_var(var_id, om_id);
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
        id
    }

    pub fn new_acquire(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();
        let om_id = self.processes[*parent].om_id();
        let model = &self.models[om_id];
        let (start, end, s_acq, resource, quantity) = match &model.chronicle {
            Some(chronicle) => {
                let binding = *chronicle
                    .bindings
                    .get_binding(&label)
                    .unwrap()
                    .as_acquire()
                    .unwrap();

                let start = self.new_execution_var(binding.request, om_id);
                let s_acq = self.new_execution_var(binding.acquisition.get_start(), om_id);
                let end = self.new_execution_var(binding.acquisition.get_end(), om_id);
                let resource = self.new_execution_var(binding.resource, om_id);
                let quantity = self.new_execution_var(binding.quantity, om_id);
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
        id
    }

    pub fn new_command(
        &mut self,
        label: Label,
        parent: &ActingProcessId,
        debug: String,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let id = self.processes.len();

        let om_id = self.processes[*parent].om_id();
        let model = &self.models[om_id];

        let (start, end) = match &model.chronicle {
            Some(chronicle) => {
                let interval = chronicle
                    .bindings
                    .get_binding(&label)
                    .unwrap()
                    .as_action()
                    .unwrap()
                    .interval;
                let start = self.new_execution_var(interval.get_start(), om_id);
                let end = self.new_execution_var(interval.get_end(), om_id);
                (start, end)
            }
            None => Default::default(),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            origin,
            om_id,
            Some(debug),
            start,
            end,
            CommandProcess::new(),
        ));

        self.processes[*parent]
            .inner
            .as_mut_method()
            .unwrap()
            .add_process(label, id);
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
        match self.processes[*id]
            .inner
            .as_task()
            .unwrap()
            .refinements
            .last()
        {
            Some(id) => {
                if self.get_origin(id) == ProcessOrigin::Planner {
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
            .as_task()
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
            let om_id = self.get_om_id(id);
            self.models[om_id].lv.clone()
        } else {
            panic!()
        }
    }

    pub fn get_om(&self, id: &ActingProcessId) -> LValue {
        if self.get_kind(id) == ProcessKind::Method {
            let om_id = self.get_om_id(id);
            self.models[om_id].lv_om.clone()
        } else {
            panic!()
        }
    }

    pub fn get_tried(&self, id: &ActingProcessId) -> Vec<LValue> {
        let refinements = self.processes[*id]
            .inner
            .as_task()
            .unwrap()
            .get_refinements();

        refinements
            .iter()
            .filter_map(|id| {
                if self.processes[*id].origin.is_exec() {
                    Some(self.processes[*id].om_id())
                } else {
                    None
                }
            })
            .map(|id| self.models[id].lv.clone())
            .collect()
    }

    pub fn set_s_acq(&mut self, id: &ActingProcessId, instant: Option<Timepoint>) {
        let instant = instant.unwrap_or(self.instant());
        let val = self.processes[*id]
            .inner
            .as_mut_acquire()
            .unwrap()
            .set_s_acq(instant);
        self.set_plan_var(val);
    }

    pub fn set_arbitrary_value(
        &mut self,
        id: &ActingProcessId,
        set: Vec<LValue>,
        greedy: LValue,
    ) -> LValue {
        let plan_var_id = &self.processes[*id]
            .inner
            .as_arbitrary()
            .unwrap()
            .get_plan_var_id();
        let value = match plan_var_id {
            None => greedy,
            Some(var_id) => match self.get_plan_var_val(var_id) {
                ActingVal::Planned(val) => {
                    let value: LValue = val.clone().into();
                    if set.contains(&value) {
                        value
                    } else {
                        greedy
                    }
                }
                ActingVal::None => greedy,
                _ => unreachable!(),
            },
        };

        let arbitrary = self.processes[*id].inner.as_mut_arbitrary().unwrap();
        arbitrary.set_set(set);
        let val = arbitrary.set_var(value.clone());
        self.set_plan_var(val);
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
        let mut updates: Vec<_> = vec![];
        self.executed(id);
        let acquire = self.processes[*id].inner.as_mut_acquire().unwrap();
        let r = match acquire.move_reservation() {
            Some(reservation) => Ok(reservation),
            None => {
                updates.push(acquire.set_resource(resource.to_string()));
                let waiter: WaitAcquire = self
                    .resource_manager
                    .acquire(resource.to_string(), quantity, priority)
                    .await?;

                let quantity = self
                    .resource_manager
                    .get_client_quantity(&waiter.get_resource_id(), &waiter.get_client_id())
                    .await;

                updates.push(acquire.set_quantity(quantity));

                acquire.set_acquire_id(&waiter);
                Ok(waiter)
            }
        };

        for update in updates {
            self.set_plan_var(update);
        }

        r
    }

    pub async fn reserve(
        &mut self,
        id: &ActingProcessId,
        resource: String,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<(), LRuntimeError> {
        let acquire = self.processes[*id].inner.as_mut_acquire().unwrap();
        acquire.set_reservation(
            self.resource_manager
                .reserve(resource, quantity, priority)
                .await?,
        );
        Ok(())
    }

    pub fn export_trace_dot_graph(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();
        let mut queue = vec![0];

        while let Some(id) = queue.pop() {
            let ap = &self.processes[id];
            let label = format_acting_process(&self.planner_manager, &ap);
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
                ActingProcessInner::Command(_) => {}
                ActingProcessInner::Task(t) => {
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
        path_dot.push(&dot_file_name);
        let mut file = File::create(&path_dot).unwrap();
        let dot = self.export_trace_dot_graph();
        file.write_all(dot.as_bytes()).unwrap();
        set_current_dir(&path).unwrap();
        let trace = "trace.png";
        std::process::Command::new("dot")
            .args(["-Tpng", &dot_file_name, "-o", &trace])
            .spawn()
            .unwrap()
            .wait()
            .unwrap();

        let mut md_path = path.clone();
        let md_file_name = "trace.md";
        md_path.push(&md_file_name);
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
            .arg(&md_file_name)
            .spawn()
            .unwrap();
    }

    fn add_processes_from_chronicles(&mut self, instances: Vec<ChronicleInstance>) {
        let table: HashMap<ProcessRef, usize> = instances
            .iter()
            .enumerate()
            .map(|(i, c)| (c.pr.clone(), i))
            .collect();

        for instance in instances {
            if instance.generated {
                let chronicle = instance.om.chronicle.as_ref().unwrap();
                let bindings = chronicle.bindings.clone();
                let st = &chronicle.st;
                let mut pr = instance.pr.clone();
                let m_label = pr.pop().unwrap();
                let parent = match self.get_id(pr.clone()) {
                    Some(id) => id,
                    None =>
                    //We need to create the task before adding the method
                    {
                        let label = pr.pop().unwrap();
                        let parent = self.get_id(pr).unwrap();
                        let task = chronicle.get_task();
                        let debug = task.format(st, true);
                        let args = chronicle
                            .get_task()
                            .iter()
                            .map(|var_id| st.var_as_cst(var_id).unwrap())
                            .collect();
                        match parent {
                            0 => {
                                let pr = self.new_high_level_task(debug, args);
                                let id = self.get_id(pr).unwrap();
                                self.processes[id].origin = ProcessOrigin::Planner;
                                id
                            }
                            _ => {
                                let id = self.new_task(
                                    label,
                                    &parent,
                                    args,
                                    debug,
                                    ProcessOrigin::Planner,
                                );
                                id
                            }
                        }
                    }
                };

                let debug = chronicle.get_name().format(st, true);

                let parent =
                    self.new_refinement(&parent, debug, instance.om, ProcessOrigin::Planner);

                for (label, binding) in bindings.inner {
                    match binding {
                        ChronicleBinding::Arbitrary(_) => {
                            self.new_arbitrary(label, &parent, ProcessOrigin::Planner);
                        }
                        ChronicleBinding::Action(_) => {}
                        ChronicleBinding::Acquire(_) => {
                            self.new_acquire(label, &parent, ProcessOrigin::Planner);
                        }
                    }
                }
            }
        }
    }

    pub fn absorb_planner_result(&mut self, result: ActingPlanResult) {
        self.add_processes_from_chronicles(result.instances);
    }
}

fn format_acting_process(
    planner_manager: &PlannerManager,
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
        ActingProcessInner::RootTask(r) => {
            write!(f, "root").unwrap();
        }
        ActingProcessInner::Command(c) => {
            write!(f, "{}", debug).unwrap();
        }
        ActingProcessInner::Task(t) => {
            write!(f, "{}", debug).unwrap();
        }
        ActingProcessInner::Method(m) => {
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

    pub fn reset_time_reference(&mut self) {
        self.time_reference = Instant::now();
    }

    pub fn get_instant(&self) -> Timepoint {
        self.time_reference.elapsed().as_micros()
    }

    pub async fn format_task_collection(&self, filter: TaskFilter) -> String {
        self.trc.format(filter).await
    }

    pub async fn format_task_network(&self) -> String {
        self.tn.format().await
    }

    pub async fn add_task(&self, task: LValue, parent_task: Option<usize>) -> TaskMetaData {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let stack = TaskMetaData::new(task, task_id, parent_task, start);
        self.trc.insert(task_id, stack.clone()).await;
        if let Some(parent_task) = parent_task {
            self.tn.add_task_to_parent(parent_task, task_id).await;
        } else {
            self.tn.add_new_root_task(task_id).await;
        }
        stack
    }

    pub async fn add_command(
        &self,
        action: LValue,
        parent_task: Option<usize>,
    ) -> (ActionId, watch::Receiver<ActionStatus>) {
        let task_id = self.get_next_id();
        let start = self.time_reference.elapsed().as_micros();
        let mut parent = false;
        let parent_task = match parent_task {
            None => task_id,
            Some(id) => {
                parent = true;
                id
            }
        };

        let (action, rx) = CommandMetaData::new(task_id, parent_task, action, start);
        self.trc.insert(task_id, action).await;
        if parent {
            self.tn.add_task_to_parent(parent_task, task_id).await;
        }
        (task_id, rx)
    }

    #[function_name::named]
    pub async fn get_task(&self, task_id: &ActionId) -> Result<TaskMetaData, LRuntimeError> {
        match self.trc.get(task_id).await {
            ActionMetaData::Task(a) => Ok(a),
            ActionMetaData::Command(_) => Err(lruntimeerror!(
                function_name!(),
                format!("{} does not exist", task_id)
            )),
        }
    }

    pub async fn update_task(&self, id: &ActionId, task: impl Into<ActionMetaData>) {
        //println!("in update stack\n stack: {}", rs);
        self.trc.update(id, task).await
    }

    pub async fn update_status(&self, id: &ActionId, status: ActionStatus) {
        self.trc.update_status(id, status).await
    }

    pub async fn get_status(&self, id: &ActionId) -> ActionStatus {
        self.trc.get_status(id).await
    }

    pub async fn get_action_collection(&self) -> im::HashMap<ActionId, ActionMetaData> {
        self.trc.get_inner().await
    }

    pub async fn set_end_time(&self, id: &ActionId) {
        let end = self.time_reference.elapsed().as_micros();
        let mut task: ActionMetaData = self.trc.get(id).await;
        task.set_end_timepoint(end);
        self.trc.update(id, task).await;
    }

    pub fn get_next_id(&self) -> usize {
        get_and_update_id_counter(self.next_id.clone())
    }
}
*/
