use crate::planning::condition::Condition;
use crate::planning::constraint::{equal, Constraint};
use crate::planning::effect::Effect;
use crate::planning::expression::Expression;
use crate::planning::expression_chronicle::ExpressionChronicle;
use crate::planning::interval::Interval;
use crate::planning::lit::Lit;
use crate::planning::partial_chronicle::PartialChronicle;
use crate::planning::symbol_table::{AtomId, SymTable};
use crate::planning::traits::{Absorb, FormatWithParent, FormatWithSymTable, GetVariables};
use crate::planning::type_table::PlanningAtomType;
use crate::planning::{ConversionCollection, END, PREZ, RESULT, START};
use aries_planning::chronicles::ChronicleKind;
use im::HashSet;
use sompas_structs::lvalue::LValue;
use std::fmt::Display;

impl FormatWithSymTable for Vec<AtomId> {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut str = "(".to_string();
        let mut first = true;
        for e in self {
            if first {
                first = false
            } else {
                str.push(' ');
            }
            str.push_str(e.format(st, sym_version).as_str());
        }
        str.push(')');
        str
    }
}

#[derive(Clone)]
pub struct ChronicleTemplate {
    pub chronicle_kind: ChronicleKind,
    label: String,
    pub name: Vec<AtomId>,
    pub task: Vec<AtomId>,
    pub pc: PartialChronicle,
    debug: Option<LValue>,
}

impl ChronicleTemplate {
    pub fn new(
        ch: &mut ConversionCollection,
        label: impl Display,
        chronicle_kind: ChronicleKind,
    ) -> Self {
        let interval = Interval::new(
            &ch.sym_table.declare_new_parameter(
                START,
                //format!("{}_start", label),
                true,
                Some(PlanningAtomType::Timepoint),
            ),
            &ch.sym_table.declare_new_parameter(
                END,
                //format!("{}_end", label),
                true,
                Some(PlanningAtomType::Timepoint),
            ),
        );

        let presence = ch.sym_table.declare_new_parameter(
            PREZ,
            //format!("{}_prez", label),
            true,
            Some(PlanningAtomType::Presence),
        );

        let result = ch.sym_table.declare_new_parameter(
            RESULT, //format!("{}_result", label),
            true, None,
        );

        let init_var = vec![presence, result, *interval.start(), *interval.end()];

        let pc: PartialChronicle = PartialChronicle {
            presence,
            interval,
            result: result.into(),
            variables: Default::default(),
            constraints: vec![],
            conditions: vec![],
            effects: vec![],
            subtasks: vec![],
        };

        let mut chronicle = Self {
            chronicle_kind,
            label: label.to_string(),
            name: Default::default(),
            task: Default::default(),
            pc,
            debug: None,
        };
        for v in init_var {
            chronicle.add_var(&v);
        }

        chronicle
    }
}

impl FormatWithSymTable for ChronicleTemplate {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut s = String::new();
        //name
        s.push_str(self.label.as_str());
        s.push('\n');
        s.push_str(format!("- name: {}\n", self.name.format(st, sym_version)).as_str());
        //task
        s.push_str(format!("- task: {}\n", self.task.format(st, sym_version)).as_str());
        s.push_str(self.pc.format(st, sym_version).as_str());

        //Debug
        if let Some(exp) = &self.debug {
            s.push_str(format!("debug: {}", exp.format("debug: ".len())).as_str());
        }

        s
    }
}

impl FormatWithParent for ChronicleTemplate {
    fn format_with_parent(&mut self, st: &SymTable) {
        self.name.format_with_parent(st);
        self.task.format_with_parent(st);
        self.pc.format_with_parent(st);
    }
}

impl ChronicleTemplate {
    pub fn set_debug(&mut self, debug: Option<LValue>) {
        self.debug = debug;
    }
}

impl ChronicleTemplate {
    pub fn absorb_expression_chronicle(&mut self, ec: ExpressionChronicle) {
        self.add_constraint(Constraint::Eq(
            self.get_result().into(),
            ec.get_result_as_lit(),
        ));

        self.add_constraint(equal(self.get_interval(), ec.get_interval()));
        self.add_constraint(Constraint::Eq(
            self.get_presence().into(),
            ec.get_presence().into(),
        ));

        self.pc.absorb(ec.pc);

        //add new subtask
    }
}
/*
ADDERS
 */
impl ChronicleTemplate {
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.pc.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.pc.add_interval(interval);
    }

    pub fn add_variables(&mut self, variables: HashSet<AtomId>) {
        self.pc.variables = self.pc.variables.clone().union(variables);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.pc.add_constraint(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.pc.add_condition(cond)
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.pc.add_effect(effect)
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.pc.add_subtask(sub_task)
    }
}
/*
SETTERS
 */
impl ChronicleTemplate {
    pub fn set_name(&mut self, name: Vec<AtomId>) {
        self.name = name;
    }

    pub fn set_task(&mut self, task: Vec<AtomId>) {
        self.task = task;
    }
}

/*
REMOVERS
 */

impl ChronicleTemplate {
    pub fn rm_var(&mut self, sym_id: &AtomId) {
        self.pc.rm_var(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<AtomId>) {
        self.pc.rm_set_var(ids)
    }

    pub fn rm_constraint(&mut self, index: usize) {
        self.pc.rm_constraint(index);
    }

    pub fn rm_set_constraint(&mut self, indexes: Vec<usize>) {
        self.pc.rm_set_constraint(indexes)
    }

    pub fn rm_condition(&mut self, index: usize) {
        self.pc.rm_condition(index);
    }
}

/*
GETTERS
 */
impl ChronicleTemplate {
    pub fn get_presence(&self) -> &AtomId {
        &self.pc.presence
    }

    pub fn get_start(&self) -> &AtomId {
        self.get_interval().start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.get_interval().end()
    }

    pub fn get_result(&self) -> &AtomId {
        self.pc.result.get_id()
    }

    pub fn get_interval(&self) -> &Interval {
        &self.pc.interval
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        self.pc.get_constraints()
    }

    pub fn get_conditions(&self) -> &Vec<Condition> {
        &self.pc.conditions
    }

    pub fn get_effects(&self) -> &Vec<Effect> {
        &self.pc.effects
    }

    pub fn get_subtasks(&self) -> &Vec<Expression> {
        &self.pc.subtasks
    }

    fn build_hashset<T: GetVariables>(vec: &[T]) -> im::HashSet<AtomId> {
        let mut hashset: HashSet<AtomId> = Default::default();
        for e in vec {
            hashset = hashset.union(e.get_variables());
        }

        hashset
    }

    pub fn get_variables_in_set(&self, set: ChronicleSet) -> im::HashSet<AtomId> {
        match set {
            ChronicleSet::Effect => Self::build_hashset(&self.pc.effects),
            ChronicleSet::Constraint => Self::build_hashset(&self.pc.constraints),
            ChronicleSet::Condition => Self::build_hashset(&self.pc.conditions),
            ChronicleSet::SubTask => Self::build_hashset(&self.pc.subtasks),
        }
    }

    pub fn get_variables_in_sets(&self, sets: Vec<ChronicleSet>) -> im::HashSet<AtomId> {
        let mut hashset = HashSet::default();
        for set in sets {
            hashset = hashset.union(self.get_variables_in_set(set))
        }
        hashset
    }

    pub fn get_all_variables_in_sets(&self) -> im::HashSet<AtomId> {
        self.get_variables_in_sets(vec![
            ChronicleSet::Effect,
            ChronicleSet::Constraint,
            ChronicleSet::Condition,
            ChronicleSet::SubTask,
        ])
    }
}

impl GetVariables for ChronicleTemplate {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.pc.get_variables()
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.pc.get_variables_of_type(sym_table, atom_type)
    }
}

/*
GETTERs
 */

#[derive(Clone, Default)]
pub struct ChronicleResult {
    id: AtomId,
    pure: Option<Lit>,
}

impl FormatWithSymTable for ChronicleResult {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        match &self.pure {
            Some(lit) => lit.format(st, sym_version),
            None => self.id.format(st, sym_version),
        }
    }
}

impl From<AtomId> for ChronicleResult {
    fn from(a: AtomId) -> Self {
        Self { id: a, pure: None }
    }
}

impl ChronicleResult {
    pub fn new(id: AtomId, pure: Option<Lit>) -> Self {
        Self { id, pure }
    }
}

impl ChronicleResult {
    pub fn get_id(&self) -> &AtomId {
        &self.id
    }

    pub fn set_pure(&mut self, lit: Lit) {
        self.pure = Some(lit)
    }

    pub fn is_pure(&self) -> bool {
        self.pure.is_some()
    }

    pub fn get_pure(&self) -> &Option<Lit> {
        &self.pure
    }
}

impl From<&ChronicleResult> for Lit {
    fn from(ecr: &ChronicleResult) -> Self {
        match &ecr.pure {
            Some(lit) => lit.clone(),
            None => ecr.id.into(),
        }
    }
}

impl From<ChronicleResult> for Lit {
    fn from(ecr: ChronicleResult) -> Self {
        (&ecr).into()
    }
}

pub enum ChronicleSet {
    Effect,
    Constraint,
    Condition,
    SubTask,
}
