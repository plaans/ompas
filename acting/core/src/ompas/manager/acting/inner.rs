use crate::model::acting_domain::model::ModelKind::PlanModel;
use crate::model::acting_domain::model::{ActingModel, NewTask, ROOT};
use crate::model::chronicle::acting_process_model::{ActingProcessModel, ActingProcessModelLabel};
use crate::model::chronicle::{Chronicle, ChronicleKind, Instantiation};
use crate::model::process_ref::{Label, MethodLabel, ProcessRef, RefinementLabel};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::acting::acting_var::{
    ActingVal, ActingValUpdate, ActingVarCollection, ActingVarId, ActingVarRef, AsCst, PlanVarRef,
};
use crate::ompas::manager::acting::interval::{Interval, Timepoint};
use crate::ompas::manager::acting::process::acquire::AcquireProcess;
use crate::ompas::manager::acting::process::action::ActionProcess;
use crate::ompas::manager::acting::process::arbitrary::ArbitraryProcess;
use crate::ompas::manager::acting::process::refinement::RefinementProcess;
use crate::ompas::manager::acting::process::root_task::RootProcess;
use crate::ompas::manager::acting::process::{ActingProcess, ActingProcessInner, ProcessOrigin};
use crate::ompas::manager::acting::{AMId, ActingProcessId, MethodModel};
use crate::ompas::manager::clock::ClockManager;
use crate::ompas::manager::domain::DomainManager;
use crate::ompas::manager::planning::plan_update::{ActingTreeUpdate, Choice, ChoiceInner};
use crate::ompas::manager::planning::planner_manager_interface::PlannerManagerInterface;
use crate::ompas::manager::planning::problem_update::{PlannerUpdate, VarUpdate};
use crate::ompas::manager::resource::{Quantity, ResourceManager, WaitAcquire, WaiterPriority};
use crate::ompas::manager::state::action_status::ProcessStatus;
use crate::planning::conversion::_convert;
use crate::planning::conversion::flow_graph::algo::annotate::annotate;
use crate::planning::conversion::flow_graph::algo::p_eval::p_eval;
use crate::planning::conversion::flow_graph::algo::p_eval::r#struct::PLEnv;
use crate::planning::conversion::flow_graph::algo::pre_processing::pre_processing;
use crate::planning::conversion::flow_graph::graph::Dot;
use crate::planning::planner::problem::ChronicleInstance;
use aries_planning::chronicles::{ChronicleOrigin, TaskId};
use chrono::{DateTime, Utc};
use im::HashSet;
use ompas_language::supervisor::*;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::any::Any;
use std::env::set_current_dir;
use std::fmt::{Debug, Display, Formatter, Write};
use std::fs::{File, OpenOptions};
use std::io::Write as ioWrite;
use std::path::PathBuf;
use std::{env, fs};
use tokio::sync::{broadcast, watch};

const COLOR_PLANNING: &str = "grey";
const COLOR_PLANNING_SUGGESTED: &str = "blue";
const COLOR_EXECUTION: &str = "red";
//const COLOR_DEFAULT: &str = "black";

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
    AbstractModel,
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
                ProcessKind::AbstractModel => ABSTRACT_MODEL,
            }
        )
    }
}

pub struct InnerActingManager {
    pub(in crate::ompas::manager::acting) processes: Vec<ActingProcess>,
    pub(in crate::ompas::manager::acting) models: Vec<ActingModel>,
    acting_vars: ActingVarCollection,
    pub(in crate::ompas::manager::acting) st: RefSymTable,
    resource_manager: ResourceManager,
    clock_manager: ClockManager,
    domain_manager: DomainManager,
    planner_manager_interface: Option<PlannerManagerInterface>,
}

impl InnerActingManager {
    pub fn new(
        resource_manager: ResourceManager,
        clock_manager: ClockManager,
        domain_manager: DomainManager,
        st: RefSymTable,
    ) -> Self {
        let mut new = Self {
            processes: vec![],
            models: vec![],
            acting_vars: Default::default(),
            st,
            resource_manager,
            clock_manager,
            domain_manager,
            planner_manager_interface: None,
        };
        new.init();
        new
    }

    pub fn init(&mut self) {
        let mut model = ActingModel::root(&self.st);
        let chronicle = model.chronicle.as_ref().unwrap();
        let start = self.new_acting_var_with_ref(&chronicle.interval.get_start(), &0);
        let end = self.new_acting_var_with_ref(&chronicle.interval.get_end(), &0);
        let now = self.clock_manager.now();
        let cst = now.as_cst().unwrap();
        model.runtime_info.add_instantiation(Instantiation::new(
            chronicle.interval.get_start(),
            self.st.new_cst(cst.clone()),
        ));
        self.acting_vars.set_execution_val(&start, now);

        let root = ActingProcess::new(
            0,
            0,
            None,
            ProcessOrigin::Execution,
            0,
            Some(ROOT.to_string()),
            start,
            end,
            RootProcess::new(),
        );
        self.processes = vec![root];
        self.models = vec![model];
    }

    pub fn notify_planner(&mut self, pu: PlannerUpdate) {
        if let Some(planner) = &self.planner_manager_interface {
            planner.send_update(pu);
        }
    }

    pub fn plan(&mut self) {
        if let Some(planner) = &self.planner_manager_interface {
            planner.send_update(PlannerUpdate::Plan);
            //.unwrap_or_else(|_| panic!());
        }
    }

    pub async fn clear(&mut self) {
        self.processes.clear();
        self.models.clear();
        self.acting_vars.clear().await;
        self.st.clear();
        self.init();
    }

    pub fn st(&self) -> RefSymTable {
        self.st.clone()
    }

    fn new_model(&mut self, model: ActingModel) -> AMId {
        self.models.push(model);
        self.models.len() - 1
    }

    fn new_acting_var_with_ref<
        T: Clone + Any + Send + Sync + AsCst + Display + Debug + PartialEq,
    >(
        &mut self,
        var_id: &VarId,
        am_id: &AMId,
    ) -> ActingVarRef<T> {
        self.acting_vars
            .new_acting_var_with_ref(PlanVarRef::new(*var_id, *am_id))
    }

    fn new_acting_var<T: Clone + Any + Send + Sync + AsCst + Display + Debug + PartialEq>(
        &mut self,
    ) -> ActingVarRef<T> {
        self.acting_vars.new_acting_var()
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
                                id = if let Some(id) = obj
                                    .inner
                                    .as_method()
                                    .unwrap_or_else(|| {
                                        panic!("{id} is not a method but a {}.", obj.inner.kind())
                                    })
                                    .childs
                                    .get(&label)
                                {
                                    *id
                                } else {
                                    return None;
                                }
                            }
                        }
                        Label::Refinement(refinement_label) => {
                            let action = obj.inner.as_action().unwrap();
                            id = if let Some(refinement) =
                                action.refinements.get(refinement_label.refinement_id)
                            {
                                if let Some(id) =
                                    refinement.get_method(&refinement_label.method_label)
                                {
                                    *id
                                } else {
                                    return None;
                                }
                            } else {
                                return None;
                            }
                        }
                        Label::ResourceAcquisition(_) | Label::Arbitrary(_) => {
                            id = if let Some(id) = obj.inner.as_method().unwrap().childs.get(&label)
                            {
                                *id
                            } else {
                                return None;
                            }
                        }
                        Label::AbstractModel => {
                            id = obj.inner.as_action().unwrap().abstract_model?;
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

    fn set_execution_val<T: Any + Sync + Send + Clone + PartialEq + AsCst + Display + Debug>(
        &mut self,
        acting_var_ref: &ActingVarRef<T>,
        val_t: T,
    ) {
        let cst = val_t.as_cst().unwrap();

        for PlanVarRef { var_id, am_id } in
            self.acting_vars.set_execution_val(&acting_var_ref, val_t)
        {
            /*println!(
                "set_execution_val: {} => {}",
                var_id.format(&self.st, true),
                val
            );*/
            self.models[am_id]
                .runtime_info
                .add_instantiation(Instantiation::new(var_id, self.st.new_cst(cst.clone())));
            self.notify_planner(PlannerUpdate::VarUpdate(VarUpdate {
                var_ref: PlanVarRef { var_id, am_id },
                value: cst.clone(),
            }));
        }
    }

    #[inline]
    fn set_planned_val(&mut self, update: ActingValUpdate) {
        self.acting_vars.set_planned_val(update)
    }

    fn set_planner_vals(&mut self, vals: Vec<ActingValUpdate>) {
        for val in vals {
            self.set_planned_val(val);
        }
    }

    fn add_new_plan_var_ref(&mut self, acting_var_id: &ActingVarId, plan_var_ref: PlanVarRef) {
        self.acting_vars
            .add_new_plan_var_ref(acting_var_id, plan_var_ref)
    }

    fn set_val<T: Any + Sync + Send + Clone + PartialEq + AsCst + Display + Debug>(
        &mut self,
        acting_var_ref: &ActingVarRef<T>,
        val: T,
        origin: ProcessOrigin,
    ) {
        match origin {
            ProcessOrigin::Planner => self.set_planned_val(ActingValUpdate {
                acting_var_id: acting_var_ref.id,
                val: val.as_cst().unwrap(),
            }),
            ProcessOrigin::Execution => self.set_execution_val(acting_var_ref, val),
            _ => unreachable!(),
        }
    }

    fn get_acting_var_acting_val(&self, acting_var_id: &ActingVarId) -> &ActingVal {
        self.acting_vars.get_acting_val(acting_var_id)
    }

    fn get_acting_var_val<T: Any + Sync + Send + Clone + PartialEq + AsCst + Display + Debug>(
        &self,
        r: &ActingVarRef<T>,
    ) -> Option<T> {
        self.acting_vars.get_val(r)
    }

    pub fn set_start(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.clock_manager.now());
        self.set_execution_val(&self.processes[*id].start.clone(), instant);
    }

    pub fn set_end(&mut self, id: &ActingProcessId, t: Option<Timepoint>, status: ProcessStatus) {
        let instant = t.unwrap_or(self.clock_manager.now());
        self.remove_process_from_model(id, None);
        let end = self.processes[*id].end.clone();
        self.set_execution_val(&end, instant);

        if self.get_kind(id) == ProcessKind::Action {
            if let Some(refinement) = self.processes[*id]
                .inner
                .as_action()
                .unwrap()
                .refinements
                .last()
                .cloned()
            {
                if let Some(chosen) = refinement.get_executed() {
                    self.set_end(chosen, Some(instant), status);
                }
            }
        }
        self.set_status(id, status)
    }

    pub fn get_task_args(&self, id: &ActingProcessId) -> Vec<Cst> {
        self.processes[*id]
            .inner
            .as_action()
            .unwrap()
            .get_args()
            .iter()
            .map(|r| self.get_acting_var_val(&r).unwrap())
            .collect()
    }

    pub fn set_action_args(&mut self, id: &ActingProcessId, args: Vec<Cst>) {
        let mut debug = "(".to_string();

        let action = self.processes[*id].inner.as_mut_action().unwrap();
        for (r, arg) in action.get_args().iter().zip(args) {
            write!(debug, "{} ", arg).unwrap();
            self.set_execution_val(r, arg);
        }
        let process = &mut self.processes[*id];
        debug.pop();
        debug.push(')');
        process.set_debug(debug)
    }

    pub fn set_moment(&mut self, id: &ActingProcessId, t: Option<Timepoint>) {
        let instant = t.unwrap_or(self.clock_manager.now());
        self.set_start(id, Some(instant));
        self.set_end(id, Some(instant), self.get_status(id));
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
                if let Some(chosen) = refinement.get_executed() {
                    self.set_status(chosen, status)
                };
            }
        }
    }

    //New processes
    pub fn new_high_level_task(&mut self, debug: String, mut args: Vec<Cst>) -> ProcessRef {
        let id = self.processes.len();
        let task_ref = self.models[0].add_new_task(NewTask {
            start: None,
            args: args.clone(),
        });

        let start = self.new_acting_var_with_ref(&task_ref.start, &0);
        let end = self.new_acting_var_with_ref(&task_ref.end, &0);

        let mut new_args = vec![];

        for (val, arg_id) in args.drain(..).zip(task_ref.name) {
            let arg = self.new_acting_var_with_ref(&arg_id, &0);
            self.set_execution_val(&arg, val);
            new_args.push(arg)
        }

        let root: &mut RootProcess = self.processes[0].inner.as_mut_root().unwrap();
        let rank = root.n_task();
        root.add_top_level_task(id);

        let label = Label::Action(rank);
        self.processes.push(ActingProcess::new(
            id,
            0,
            Some(label),
            ProcessOrigin::Execution,
            0,
            Some(debug),
            start,
            end,
            ActionProcess::new(new_args),
        ));
        self.notify_planner(PlannerUpdate::ProblemUpdate(id));

        ProcessRef::Relative(0, vec![label])
    }

    //Task methods
    pub fn new_action(
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
                .acting_process_models
                .get_process_model(label)
                .unwrap()
                .as_action()
                .unwrap()
                .clone();
            let name = binding.task.name;
            let start = self.new_acting_var_with_ref(&binding.task.interval.get_start(), &om_id);
            let end = self.new_acting_var_with_ref(&binding.task.interval.get_end(), &om_id);
            let mut new_name = vec![];

            for (cst, var_id) in args.drain(..).zip(name) {
                let arg = self.new_acting_var_with_ref(&var_id, &om_id);
                if let Some(val) = cst {
                    self.set_val(&arg, val, origin);
                }
                new_name.push(arg)
            }
            (start, end, new_name)
        } else {
            let start = self.new_acting_var();
            let end = self.new_acting_var();
            let mut new_name = vec![];
            for cst in args {
                let arg = self.new_acting_var();
                if let Some(val) = cst {
                    self.set_val(&arg, val, origin);
                }
                new_name.push(arg)
            }
            (start, end, new_name)
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            Some(label),
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

    pub fn new_abstract_model(
        &mut self,
        parent: &ActingProcessId,
        model: ActingModel,
    ) -> ActingProcessId {
        let id = self.processes.len();
        let am_id = self.new_model(model);
        let model = &self.models[am_id];
        let chronicle = model.chronicle.as_ref().unwrap();
        let parent_process = &self.processes[*parent];
        let debug = parent_process.debug().clone();
        let action = parent_process.inner.as_action().unwrap();
        let chronicle_name = chronicle.get_name().clone();

        let name = action.args.clone();
        if chronicle_name.len() != name.len() {
            panic!(
                "no matching between chronicle_name:{:?} and args:{:?}",
                chronicle_name, name
            )
        }

        let interval = chronicle.interval;
        let start = parent_process.start.clone();
        let end = parent_process.end.clone();

        self.add_new_plan_var_ref(
            &start.id,
            PlanVarRef {
                var_id: interval.get_start(),
                am_id,
            },
        );
        self.add_new_plan_var_ref(
            &end.id,
            PlanVarRef {
                var_id: interval.get_end(),
                am_id,
            },
        );

        //let start = self.new_execution_var(&interval.get_start(), &am_id);
        //let end = self.new_execution_var(&interval.get_end(), &am_id);

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            Some(Label::AbstractModel),
            ProcessOrigin::Planner,
            am_id,
            debug,
            start,
            end,
            ActingProcessInner::AbstractModel(RefinementProcess::new(
                RefinementLabel {
                    refinement_id: 0,
                    method_label: MethodLabel::Possibility(0),
                },
                name,
            )),
        ));

        self.processes[*parent]
            .inner
            .as_mut_action()
            .unwrap()
            .add_abstract_model(&id);
        id
    }

    pub fn new_refinement(&mut self, action: &ActingProcessId) -> usize {
        let action = self.processes[*action].inner.as_mut_action().unwrap();
        action.new_refinement();
        action.get_refinements().len()
    }

    pub fn set_failed_method(&mut self, method: &ActingProcessId) {
        self.set_end(method, None, ProcessStatus::Failure);
        let parent = self.processes[*method].parent();
        self.new_refinement(&parent);
    }

    pub fn set_executed_refinement(&mut self, action: &ActingProcessId, method: &ActingProcessId) {
        let refinement_label = self.processes[*method].inner.as_method().unwrap().label;
        self.processes[*action]
            .inner
            .as_mut_action()
            .unwrap()
            .set_executed(refinement_label.refinement_id, method);
        let instant = self.clock_manager.now();
        if self.get_status(&action) != ProcessStatus::Pending {
            self.set_status(action, ProcessStatus::Running(None));
            self.set_start(action, Some(instant));
        }
        self.set_status(method, ProcessStatus::Running(None));
        self.set_start(method, Some(instant));
    }

    pub async fn new_executed_method(
        &mut self,
        parent: &ActingProcessId,
        debug: String,
        args: Vec<Option<Cst>>,
        model: MethodModel,
        origin: ProcessOrigin,
    ) -> ActingProcessId {
        let action = self.processes[*parent].inner.as_mut_action().unwrap();
        if action.refinements.is_empty() {
            action.refinements.push(Default::default())
        }
        let refinement_id = action.refinements.len() - 1;

        let model = match model {
            MethodModel::Raw(lv, plenv) => self
                .generate_acting_model_for_method(parent, lv, plenv)
                .await
                .unwrap(),
            MethodModel::ActingModel(am) => am,
        };

        self.new_method(
            RefinementLabel {
                refinement_id,
                method_label: MethodLabel::Executed,
            },
            parent,
            debug,
            args,
            model,
            origin,
        )
    }

    async fn generate_acting_model_for_method(
        &mut self,
        task_id: &ActingProcessId,
        lv: LValue,
        mut p_env: PLEnv,
    ) -> Result<ActingModel, LRuntimeError> {
        let debug = lv.to_string();
        let p_eval_lv = p_eval(&lv, &mut p_env).await?;
        let lv_om = annotate(p_eval_lv);

        let mut lv_expanded = None;
        let mut chronicle = None;

        let task = self.processes[*task_id].inner.as_action().unwrap();

        let mut task_name: Vec<_> = task
            .args
            .iter()
            .map(|var_ref| self.get_acting_var_val(var_ref).unwrap())
            .collect();

        let has_abstract_model = task.abstract_model.is_some();

        if self.is_planner_launched() {
            if !has_abstract_model {
                let task = self
                    .domain_manager
                    .get_task(&task_name[0].to_string())
                    .await
                    .unwrap();
                match task.get_model(&PlanModel) {
                    Some(model) => {
                        let mut lv = vec![model];
                        for arg in &task_name[1..] {
                            lv.push(arg.clone().into())
                        }
                        let lv = lv.into();
                        let p_eval_lv = p_eval(&lv, &mut p_env).await?;
                        let lv_om = annotate(p_eval_lv);

                        let st = self.st.clone();
                        let mut ch = Chronicle::new(debug, ChronicleKind::Task, st.clone());
                        let task_name = task_name.drain(..).map(|cst| st.new_cst(cst)).collect();
                        let mut name = vec![];
                        if let LValue::List(list) = &lv {
                            let list: Vec<_> = list.iter().map(|lv| lv.as_cst().unwrap()).collect();
                            for arg in list.as_slice() {
                                let id = st.new_cst(arg.clone());
                                ch.add_var(id);
                                name.push(id);
                            }
                        } else {
                            panic!()
                        };
                        ch.set_task(task_name);
                        ch.set_name(name);

                        let ch = Some(ch);
                        let pp_lv = pre_processing(&lv_om, &p_env).await?;

                        let chronicle = match _convert(ch.clone(), &pp_lv, &mut p_env, st).await {
                            Ok(ch) => Some(ch),
                            Err(e) => {
                                println!("convert error: {}", e);
                                ch
                            }
                        };
                        let lv_expanded = None;

                        let model = ActingModel {
                            lv,
                            lv_om,
                            lv_expanded,
                            runtime_info: Default::default(),
                            chronicle,
                        };

                        self.new_abstract_model(task_id, model);
                    }
                    None => {
                        let st = self.st.clone();
                        let mut ch = Chronicle::new(debug, ChronicleKind::Method, st.clone());
                        let task_name = task_name.drain(..).map(|cst| st.new_cst(cst)).collect();
                        let mut name = vec![];
                        if let LValue::List(list) = &lv {
                            let list: Vec<_> = list.iter().map(|lv| lv.as_cst().unwrap()).collect();
                            for arg in list.as_slice() {
                                let id = st.new_cst(arg.clone());
                                ch.add_var(id);
                                name.push(id);
                            }
                        } else {
                            panic!()
                        };
                        ch.set_task(task_name);
                        ch.set_name(name);

                        let ch = Some(ch);
                        let pp_lv = pre_processing(&lv_om, &p_env).await?;
                        //debug_println!("pre_processing =>\n{}", pp_lv.format(0));

                        chronicle = match _convert(ch.clone(), &pp_lv, &mut p_env, st).await {
                            Ok(ch) => Some(ch),
                            Err(e) => {
                                println!("convert error: {}", e);
                                ch
                            }
                        };

                        lv_expanded = Some(pp_lv);
                    }
                }
            }
        }

        Ok(ActingModel {
            lv: lv.clone(),
            lv_om,
            lv_expanded,
            runtime_info: Default::default(),
            chronicle,
        })
    }

    pub fn new_method(
        &mut self,
        label: RefinementLabel,
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

            if name.len() != args.len() {
                panic!(
                    "no matching between chronicle_name:{:?} and args:{:?}",
                    name, args
                )
            }

            let interval = chronicle.interval;
            let start = self.new_acting_var_with_ref(&interval.get_start(), &am_id);
            let end = self.new_acting_var_with_ref(&interval.get_end(), &am_id);
            let mut new_name = vec![];

            for (cst, var_id) in args.drain(..).zip(name) {
                let exec = self.new_acting_var_with_ref(&var_id, &am_id);
                if let Some(val) = cst {
                    self.set_val(&exec, val, origin);
                }
                new_name.push(exec)
            }
            (start, end, new_name)
        } else {
            let start = self.new_acting_var();
            let end = self.new_acting_var();
            let mut new_name = vec![];
            for cst in args {
                let exec = self.acting_vars.new_acting_var();
                if let Some(val) = cst {
                    self.set_val(&exec, val, origin);
                }
                new_name.push(exec);
            }
            (start, end, new_name)
        };

        let action: &mut ActionProcess = self.processes[*parent].inner.as_mut_action().unwrap();

        action.add_method(label, &id);
        self.processes.push(ActingProcess::new(
            id,
            *parent,
            Some(Label::Refinement(label)),
            origin,
            am_id,
            Some(debug),
            start,
            end,
            RefinementProcess::new(label, args),
        ));
        id
    }

    pub fn new_arbitrary(
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
                    .acting_process_models
                    .get_process_model(label)
                    .unwrap()
                    .as_arbitrary()
                    .unwrap();
                let timepoint = binding.timepoint;
                let var_id = binding.var_id;

                let start: ActingVarRef<Timepoint> =
                    self.new_acting_var_with_ref(&timepoint, &om_id);
                let end = start.clone();
                let var = self.new_acting_var_with_ref(&var_id, &om_id);
                (start, end, var)
            }
            None => (
                self.new_acting_var::<Timepoint>(),
                self.new_acting_var::<Timepoint>(),
                self.new_acting_var::<LValue>(),
            ),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            Some(label),
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

    pub fn new_acquire(
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
                let binding = chronicle
                    .acting_process_models
                    .get_process_model(label)
                    .unwrap()
                    .as_acquire()
                    .unwrap()
                    .clone();

                let start = self.new_acting_var_with_ref(&binding.request, &om_id);
                let s_acq = self.new_acting_var_with_ref(&binding.acquisition.get_start(), &om_id);
                let end = self.new_acting_var_with_ref(&binding.acquisition.get_end(), &om_id);
                let resource = self.new_acting_var_with_ref(&binding.resource, &om_id);
                let quantity = self.new_acting_var_with_ref(&binding.quantity, &om_id);
                (start, end, s_acq, resource, quantity)
            }
            None => (
                self.new_acting_var(),
                self.new_acting_var(),
                self.new_acting_var(),
                self.new_acting_var(),
                self.new_acting_var(),
            ),
        };

        self.processes.push(ActingProcess::new(
            id,
            *parent,
            Some(label),
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
        match refinements.last() {
            Some(refinement) => refinement.get_suggested().cloned(),
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
            Some(refinement) => refinement.get_executed().cloned(),
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
        let mut list = vec![];
        for arg in &self.processes[*id].inner.as_method().unwrap().args {
            list.push(self.get_acting_var_acting_val(&arg.id).as_cst().unwrap())
        }
        list.into()
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
            .filter_map(|refinement| {
                refinement
                    .get_executed()
                    .map(|id| self.processes[*id].am_id())
                /*if self.processes[*refinement.get_chosen()].origin.is_exec() {
                    Some(self.processes[*refinement.get_chosen()].am_id())
                } else {
                    None
                }*/
            })
            .map(|id| self.models[id].lv.clone())
            .collect()
    }

    pub fn remove_process_from_model(
        &mut self,
        id: &ActingProcessId,
        label: Option<ActingProcessModelLabel>,
    ) {
        let am_id = self.processes[*id].am_id();
        let label = if let Some(label) = label {
            label
        } else {
            self.processes[*id].label().unwrap().into()
        };
        self.models[am_id].runtime_info.add_to_remove(label);
    }

    pub fn set_s_acq(&mut self, id: &ActingProcessId, instant: Option<Timepoint>) {
        let instant = instant.unwrap_or(self.clock_manager.now());
        let Label::ResourceAcquisition(acquire_id) = self.processes[*id].label().unwrap() else {
            todo!()
        };
        let var = self.processes[*id]
            .inner
            .as_acquire()
            .unwrap()
            .s_acq
            .clone();
        self.remove_process_from_model(id, Some(ActingProcessModelLabel::Acquire(acquire_id)));
        self.set_execution_val(&var, instant)
    }

    pub fn set_arbitrary_value(
        &mut self,
        id: &ActingProcessId,
        set: Vec<LValue>,
        greedy: LValue,
    ) -> LValue {
        let acting_var_ref = self.processes[*id]
            .inner
            .as_arbitrary()
            .unwrap()
            .var
            .clone();
        let value = match self.get_acting_var_acting_val(&acting_var_ref.id) {
            ActingVal::Planned(val) => {
                let value: LValue = val.clone().into();
                if set.contains(&value) {
                    //println!("{}.arb = {}", id, value);
                    value
                } else {
                    greedy
                }
            }
            ActingVal::None => greedy,
            _ => unreachable!(),
        };

        let arbitrary = &mut self.processes[*id];
        let arbitrary = arbitrary.inner.as_mut_arbitrary().unwrap();
        arbitrary.set_set(set);
        let var_ref = arbitrary.var.clone();
        let now = self.clock_manager.now();
        self.set_start(id, Some(now));
        self.set_execution_val(&var_ref, value.clone());
        self.executed(id);
        self.set_end(id, Some(now), ProcessStatus::Success);

        value
    }

    pub async fn acquire(
        &mut self,
        id: &ActingProcessId,
        resource: String,
        quantity: Quantity,
        priority: WaiterPriority,
    ) -> Result<WaitAcquire, LRuntimeError> {
        self.executed(id);

        let acquire = self.processes[*id].inner.as_acquire().unwrap();
        let ref_r = acquire.resource.clone();
        let ref_q = acquire.quantity.clone();

        let reservation = self.processes[*id]
            .inner
            .as_mut_acquire()
            .unwrap()
            .move_reservation();

        let r = if let Some(reservation) = reservation {
            //println!("{} reserved", resource);
            if self
                .resource_manager
                .acquire_reservation(
                    &resource,
                    &quantity,
                    &reservation.get_resource_id(),
                    &reservation.get_client_id(),
                )
                .await
                .is_ok()
            {
                Some(reservation)
            } else {
                self.resource_manager.remove_waiter(reservation).await;
                None
            }
        } else {
            None
        };

        let waiter = match r {
            Some(r) => r,
            None => {
                let waiter: WaitAcquire = self
                    .resource_manager
                    .acquire(&resource, quantity, priority)
                    .await?;

                self.processes[*id]
                    .inner
                    .as_mut_acquire()
                    .unwrap()
                    .set_acquire_id(&waiter);
                waiter
            }
        };

        let capacity = self
            .resource_manager
            .get_client_quantity(&waiter.get_resource_id(), &waiter.get_client_id())
            .await;

        self.set_execution_val(&ref_r, resource.to_string());
        self.set_execution_val(&ref_q, capacity);
        self.set_status(id, ProcessStatus::Running(None));

        Ok(waiter)
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
        let process = &mut self.processes[id];
        let status = process.status.clone();
        let acquire = process.inner.as_mut_acquire().unwrap();

        match status {
            ProcessStatus::Pending => {
                if let Some(wa) = acquire.move_reservation() {
                    self.resource_manager.remove_waiter(wa).await
                }
                acquire.set_reservation(
                    self.resource_manager
                        .reserve(&resource, quantity, priority)
                        .await?,
                );
            }
            ProcessStatus::Running(_) => {
                self.resource_manager
                    .update_priority(
                        &acquire.get_resource_id().unwrap(),
                        &acquire.get_client_id().unwrap(),
                        priority,
                    )
                    .await
            }
            _ => {}
        }

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

        let mut add_chronicle =
            |exec_chronicles: &mut Vec<ExecChronicle>,
             action_id: ActingProcessId,
             origin: ChronicleOrigin,
             chronicle: Chronicle,
             refinement_label: RefinementLabel| {
                let instance_id = chronicles.len();

                for (task_id, subtask) in chronicle.get_subtasks().iter().enumerate() {
                    if let Some(id) = subtask.label.as_ref().and_then(|label| {
                        self.get_id(ProcessRef::Relative(action_id, vec![*label]))
                    }) {
                        exec_chronicles.insert(
                            0,
                            ExecChronicle {
                                id,
                                origin: ChronicleOrigin::Refinement {
                                    refined: vec![TaskId {
                                        instance_id,
                                        task_id,
                                    }],
                                    template_id: 0,
                                },
                            },
                        );
                    }
                }

                chronicles.push(ChronicleInstance {
                    instantiated_chronicle: chronicle.clone(),
                    generated: false,
                    origin,
                    am: ActingModel {
                        lv: LValue::Nil,
                        lv_om: LValue::Nil,
                        lv_expanded: Some(LValue::Nil),
                        runtime_info: Default::default(),
                        chronicle: Some(chronicle),
                    },
                    pr: ProcessRef::Id(action_id),
                    refinement_label,
                });
            };

        'main: while let Some(ExecChronicle { mut id, origin }) = exec_chronicles.pop() {
            let refinement_label: RefinementLabel = RefinementLabel {
                refinement_id: 0,
                method_label: MethodLabel::Possibility(0),
            };
            let process = &self.processes[id];
            match &process.inner {
                ActingProcessInner::RootTask(_) => {
                    let chronicle = self.models[process.am_id()]
                        .get_instantiated_chronicle()
                        .unwrap();

                    add_chronicle(
                        &mut exec_chronicles,
                        id,
                        origin,
                        chronicle,
                        refinement_label,
                    );
                }
                ActingProcessInner::Action(a) => {
                    //Verify if we take the model of the action or the refinement
                    if let Some(abstract_model) = &a.abstract_model {
                        //An abstract model is used, meaning that no subtask is present.
                        id = *abstract_model;

                        let abstract_process = &self.processes[*abstract_model];
                        let chronicle = self.models[abstract_process.get_am_id()]
                            .get_instantiated_chronicle()
                            .unwrap();

                        add_chronicle(
                            &mut exec_chronicles,
                            id,
                            origin,
                            chronicle,
                            refinement_label,
                        );
                    }
                    //Otherwise we check if there is a refinement
                    else if let Some(refinement) = a.refinements.last() {
                        let methods = if let Some(chosen) = refinement.get_executed() {
                            vec![chosen]
                        } else {
                            refinement.get_possibilities()
                        };
                        for refinement_id in methods {
                            let method_process = &self.processes[*refinement_id];
                            let chronicle = self.models[method_process.get_am_id()]
                                .get_instantiated_chronicle()
                                .unwrap();
                            let refinement_label = method_process.inner.as_method().unwrap().label;
                            add_chronicle(
                                &mut exec_chronicles,
                                *refinement_id,
                                origin.clone(),
                                chronicle,
                                refinement_label,
                            )
                        }
                    } else {
                        continue 'main;
                    }
                }
                _ => panic!(),
            };
        }
        //Add all chronicles of the problem

        chronicles
    }

    pub fn export_trace_dot_graph(&self) -> Dot {
        let mut dot: Dot = "digraph {\n".to_string();
        let mut queue = vec![(0, COLOR_EXECUTION)];

        while let Some((id, color)) = queue.pop() {
            let ap = &self.processes[id];
            let label = format_acting_process(&self.acting_vars, ap);

            writeln!(dot, "P{id} [label = \"{label}\", color = {color}];").unwrap();
            match &ap.inner {
                ActingProcessInner::RootTask(rt) => {
                    for st in &rt.tasks {
                        writeln!(dot, "P{id} -> P{st};").unwrap();
                        queue.push((*st, color))
                    }
                }
                ActingProcessInner::Action(t) => {
                    for (_i, r) in t.refinements.iter().enumerate() {
                        if let Some(executed) = r.get_executed() {
                            writeln!(dot, "P{id} -> P{};", executed).unwrap();
                            queue.push((*executed, color))
                        } else {
                            let mut possibilites = r.get_possibilities();
                            if let Some(s) = r.get_suggested() {
                                possibilites.retain(|&p| p != s);
                                writeln!(dot, "P{id} -> P{};", s).unwrap();
                                queue.push((*s, COLOR_PLANNING_SUGGESTED));
                            }

                            for p in possibilites {
                                writeln!(dot, "P{id} -> P{};", p).unwrap();
                                queue.push((*p, COLOR_PLANNING))
                            }
                        }
                    }
                }
                ActingProcessInner::Method(m) | ActingProcessInner::AbstractModel(m) => {
                    for sub in m.childs.values() {
                        writeln!(dot, "P{id} -> P{sub};").unwrap();
                        queue.push((*sub, color))
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

    pub async fn set_planner_manager_interface(&mut self, pmi: PlannerManagerInterface) {
        self.planner_manager_interface = Some(pmi);
    }

    pub fn is_planner_launched(&self) -> bool {
        self.planner_manager_interface.is_some()
    }

    pub fn notify_planner_tree_update(&mut self) {
        if let Some(planner) = &self.planner_manager_interface {
            planner.notify_update_tree();
        }
    }

    pub async fn subscribe_on_planner_tree_update(&self) -> Option<broadcast::Receiver<bool>> {
        self.planner_manager_interface
            .as_ref()
            .map(|pmi| pmi.subscribe_on_update())
    }

    pub(crate) async fn update_acting_tree(&mut self, update: ActingTreeUpdate) {
        let ActingTreeUpdate {
            acting_models,
            choices,
        } = update;
        self.add_processes_from_chronicles(acting_models);
        self.absorb_choices(choices).await;
        //locked.dump_trace(None)
        self.notify_planner_tree_update();
    }

    pub(crate) fn add_processes_from_chronicles(&mut self, instances: Vec<ChronicleInstance>) {
        for instance in instances {
            if instance.generated {
                let chronicle = instance.am.chronicle.as_ref().unwrap();
                let bindings = chronicle.acting_process_models.clone();
                let st = chronicle.st.clone();
                let mut pr = instance.pr.clone();
                let label = pr.pop().unwrap();
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
                                let pr = self.new_high_level_task(
                                    debug,
                                    args.drain(..).map(|c| c.unwrap()).collect(),
                                );
                                let id = self.get_id(pr).unwrap();
                                self.processes[id].origin = ProcessOrigin::Planner;
                                id
                            }
                            _ => {
                                self.new_action(label, &parent, args, debug, ProcessOrigin::Planner)
                            }
                        }
                    }
                };

                let debug = chronicle.get_name().format(&st, true);

                match chronicle.meta_data.kind {
                    ChronicleKind::Command | ChronicleKind::Task => {
                        self.new_abstract_model(&parent, instance.am);
                    }
                    ChronicleKind::Method => {
                        let args: Vec<Option<Cst>> = chronicle
                            .get_name()
                            .iter()
                            .map(|var_id| st.var_as_cst(var_id))
                            .collect();
                        let Label::Refinement(refinement_label) = label else {
                            todo!()
                        };

                        let action = self.processes[parent].inner.as_mut_action().unwrap();
                        if action.refinements.len() == refinement_label.refinement_id {
                            action.new_refinement();
                        }

                        let method = self.new_method(
                            refinement_label,
                            &parent,
                            debug,
                            args,
                            instance.am,
                            ProcessOrigin::Planner,
                        );

                        for (label, binding) in bindings.inner {
                            let ActingProcessModelLabel::Label(label) = label else {
                                todo!()
                            };
                            match binding {
                                ActingProcessModel::Arbitrary(_arbitrary) => {
                                    let _id =
                                        self.new_arbitrary(label, &method, ProcessOrigin::Planner);
                                }
                                ActingProcessModel::Action(action_binding) => {
                                    let action = &action_binding.task;
                                    let name = &action.name;
                                    let debug = name.format(&st, true);
                                    let args: Vec<_> =
                                        name.iter().map(|var_id| st.var_as_cst(var_id)).collect();
                                    let _id = self.new_action(
                                        label,
                                        &method,
                                        args,
                                        debug,
                                        ProcessOrigin::Planner,
                                    );
                                }
                                ActingProcessModel::Resource(_acq) => {
                                    let _id =
                                        self.new_acquire(label, &method, ProcessOrigin::Planner);
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

        let mut update = |plan_var_id: ActingVarId, val: Cst| {
            updates.push(ActingValUpdate {
                acting_var_id: plan_var_id,
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
                    update(arb.var.id, a.val);
                }
                ChoiceInner::Acquire(a) => {
                    reservations.push(Reservation {
                        id,
                        resource: a.resource.to_string(),
                        quantity: Quantity::Some(a.quantity.as_int().unwrap() as usize),
                        priority: a.priority,
                    });
                    let acq = process.inner.as_acquire().unwrap();
                    update(acq.resource.id, a.resource);
                    update(acq.quantity.id, a.quantity);
                    update(acq.s_acq.id, a.s_acq);
                    update(process.start.id, a.request);
                    update(process.end.id, a.e_acq);
                }
                ChoiceInner::SubTask(s) => {
                    let action = process.inner.as_action().unwrap();
                    for (arg, cst) in action.args.iter().zip(s.name) {
                        update(arg.id, cst)
                    }
                    update(process.start.id, s.start);
                    update(process.end.id, s.end);
                }
                ChoiceInner::Refinement(r) => {
                    update(process.start.id, r.start);
                    update(process.end.id, r.end);
                    if let Some(method) = process.inner.as_method() {
                        for (arg, cst) in method.args.iter().zip(r.name) {
                            update(arg.id, cst)
                        }
                        let parent = process.parent();
                        self.processes[parent]
                            .inner
                            .as_mut_action()
                            .unwrap()
                            .set_suggested(r.refinement_label.refinement_id, &id);
                    }
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
                        let start = self.get_acting_var_val(&process.start).unwrap();
                        let end = self.get_acting_var_val(&process.end);
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
        planner_manager.format_acting_var(&acting_process.start),
        planner_manager.format_acting_var(&acting_process.end),
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
            write!(f, "arb({})", planner_manager.format_acting_var(&arb.var)).unwrap();
        }
        ActingProcessInner::Acquire(acq) => {
            write!(
                f,
                "{}: acq({},{})",
                planner_manager.format_acting_var(&acq.s_acq),
                planner_manager.format_acting_var(&acq.resource),
                planner_manager.format_acting_var(&acq.quantity)
            )
            .unwrap();
        }
        ActingProcessInner::AbstractModel(_) => {
            write!(f, "{}", debug).unwrap();
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
