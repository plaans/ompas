use crate::planning::conversion::post_processing::bind_variables;
use crate::planning::structs::condition::Condition;
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::effect::Effect;
use crate::planning::structs::expression::Expression;
use crate::planning::structs::interval::Interval;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::{Absorb, FormatWithSymTable, GetVariables};
use crate::planning::structs::type_table::PlanningAtomType;
use crate::planning::structs::ChronicleHierarchy;
use im::HashSet;
use ompas_lisp::core::structs::lvalue::LValue;
use std::fmt::Display;
use std::ops::Deref;

#[derive(Clone)]
pub struct Chronicle {
    pub name: Lit,
    pub task: Lit,
    pub result: AtomId,
    pub interval: Interval,
    pub prez: AtomId,
    pub partial_chronicle: PartialChronicle,
    debug: Option<LValue>,
}

impl Chronicle {
    pub fn new(ch: &mut ChronicleHierarchy, label: impl Display) -> Self {
        let interval = Interval::new(
            &ch.sym_table.declare_new_variable(
                format!("{}_start", label),
                true,
                Some(PlanningAtomType::Timepoint),
            ),
            &ch.sym_table.declare_new_variable(
                format!("{}_end", label),
                true,
                Some(PlanningAtomType::Timepoint),
            ),
        );

        let prez = ch.sym_table.declare_new_variable(
            format!("{}_prez", label),
            true,
            Some(PlanningAtomType::Bool),
        );

        let result = ch
            .sym_table
            .declare_new_variable(format!("{}_result", label), true, None);

        let init_var = vec![
            prez.clone(),
            result.clone(),
            interval.start().clone(),
            interval.end().clone(),
        ];

        let mut chronicle = Self {
            name: Default::default(),
            task: Default::default(),
            result,
            interval,
            prez,
            partial_chronicle: Default::default(),
            debug: None,
        };
        for v in init_var {
            chronicle.add_var(&v);
        }

        chronicle
    }
}

impl FormatWithSymTable for Chronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let mut s = String::new();
        //name
        s.push_str(
            format!(
                "{} {}\n",
                self.interval.format_with_sym_table(st),
                self.name.format_with_sym_table(st)
            )
            .as_str(),
        );
        //task
        s.push_str(format!("-task: {}\n", self.task.format_with_sym_table(st)).as_str());
        s.push_str(self.partial_chronicle.format_with_sym_table(st).as_str());

        //Debug
        if let Some(exp) = &self.debug {
            s.push_str(format!("debug: {}", exp.format("debug: ".len())).as_str());
            //s.push_str(format!("debug: {:?}", exp).as_str());
        }

        s
    }
}

impl Chronicle {
    pub fn set_debug(&mut self, debug: Option<LValue>) {
        self.debug = debug;
    }
}

impl Chronicle {
    pub fn absorb_expression_chronicle(
        &mut self,
        ec: ExpressionChronicle,
        sym_table: &mut SymTable,
    ) {
        self.add_constraint(Constraint::Eq(self.get_result().into(), ec.get_result()));
        //bind_variables(self.get_result_id(), ec.get_result_id(), sym_table);
        bind_variables(&self.interval.start(), &ec.interval.start(), sym_table);
        bind_variables(&self.interval.end(), &ec.interval.end(), sym_table);
        sym_table.flat_bindings();
        //add result
        //add interval
        self.add_var(&sym_table.get_parent(ec.get_result_id()));
        self.add_var(&sym_table.get_parent(&ec.interval.start()));
        self.add_var(&sym_table.get_parent(&ec.interval.end()));
        self.partial_chronicle.absorb(ec.partial_chronicle);

        //add new subtask
    }
}

impl Chronicle {
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.partial_chronicle.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.partial_chronicle.add_interval(interval);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.partial_chronicle.add_constraint(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.partial_chronicle.add_condition(cond)
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.partial_chronicle.add_effect(effect)
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.partial_chronicle.add_subtask(sub_task)
    }

    pub fn set_name(&mut self, name: Lit) {
        self.name = name;
    }

    pub fn set_task(&mut self, task: Lit) {
        self.task = task;
    }
}
impl Chronicle {
    pub fn get_prez(&self) -> &AtomId {
        &self.prez
    }

    pub fn get_start(&self) -> &AtomId {
        self.get_interval().start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.get_interval().end()
    }

    pub fn get_result(&self) -> &AtomId {
        &self.result
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }
}

impl GetVariables for Chronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.partial_chronicle.get_variables()
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.partial_chronicle
            .get_variables_of_type(sym_table, atom_type)
    }
}

#[derive(Clone, Default)]
pub struct PartialChronicle {
    variables: HashSet<AtomId>,
    constraints: Vec<Constraint>,
    conditions: Vec<Condition>,
    effects: Vec<Effect>,
    subtasks: Vec<Expression>,
}

impl FormatWithSymTable for PartialChronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let mut s = String::new();
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| st.get_sym(id).to_string())
            .collect::<Vec<String>>();
        variables.sort();

        for (i, sym) in variables.iter().enumerate() {
            if i != 0 {
                s.push(',');
            }
            s.push_str(sym);
        }
        s.push_str("}\n");

        s.push_str("-constraint(s): {\n");
        for c in &self.constraints {
            s.push('\t');
            s.push_str(c.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {\n");
        for e in &self.conditions {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {\n");
        for e in &self.effects {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {\n");
        for e in &self.subtasks {
            s.push('\t');
            s.push_str(e.format_with_sym_table(st).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }
}

impl Absorb for PartialChronicle {
    fn absorb(&mut self, mut other: Self) {
        self.variables = self.variables.clone().union(other.variables);
        self.constraints.append(&mut other.constraints);
        self.conditions.append(&mut other.conditions);
        self.effects.append(&mut other.effects);
        self.subtasks.append(&mut other.subtasks);
    }
}

impl PartialChronicle {
    pub fn rm_var(&mut self, sym_id: &AtomId) {
        self.variables.remove(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<AtomId>) {
        for id in ids {
            self.rm_var(&id);
        }
    }

    pub fn rm_constraint(&mut self, index: usize) {
        self.constraints.remove(index);
    }

    pub fn rm_set_constraint(&mut self, mut indexes: Vec<usize>) {
        indexes.reverse();
        for index in indexes {
            self.rm_constraint(index);
        }
    }
}

impl PartialChronicle {
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.variables.insert(*sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.variables.insert(*interval.start());
        self.variables.insert(*interval.end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        if let Constraint::And(Lit::Constraint(a), Lit::Constraint(b)) = constraint {
            self.constraints.push(a.deref().clone());
            self.constraints.push(b.deref().clone());
        } else {
            self.constraints.push(constraint);
        }
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.conditions.push(cond);
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.effects.push(effect);
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.subtasks.push(sub_task);
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        &self.constraints
    }
}

impl GetVariables for PartialChronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.variables.clone()
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.variables
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}

#[derive(Clone, Default)]
pub struct ChronicleResult {
    id: AtomId,
    pure: Option<Lit>,
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

impl From<ChronicleResult> for Lit {
    fn from(ecr: ChronicleResult) -> Self {
        match ecr.pure {
            Some(lit) => lit,
            None => ecr.id.into(),
        }
    }
}

#[derive(Clone)]
pub struct ExpressionChronicle {
    interval: Interval,
    result: ChronicleResult,
    partial_chronicle: PartialChronicle,
    debug: LValue,
}

impl ExpressionChronicle {
    pub fn rm_var(&mut self, sym_id: &AtomId) {
        self.partial_chronicle.rm_var(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<AtomId>) {
        self.partial_chronicle.rm_set_var(ids)
    }

    pub fn rm_constraint(&mut self, index: usize) {
        self.partial_chronicle.rm_constraint(index);
    }

    pub fn rm_set_constraint(&mut self, indexes: Vec<usize>) {
        self.partial_chronicle.rm_set_constraint(indexes)
    }

    pub fn set_pure_result(&mut self, result: Lit) {
        self.result.set_pure(result)
    }
}

impl ExpressionChronicle {
    pub fn add_variables(&mut self, variables: HashSet<AtomId>) {
        self.partial_chronicle.variables =
            self.partial_chronicle.variables.clone().union(variables);
    }
}

impl ExpressionChronicle {
    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_result(&self) -> Lit {
        self.result.clone().into()
    }

    pub fn is_result_pure(&self) -> bool {
        self.result.is_pure()
    }

    pub fn get_result_id(&self) -> &AtomId {
        self.result.get_id()
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        self.partial_chronicle.get_constraints()
    }
}

pub enum ChronicleSet {
    Effect,
    Constraint,
    Condition,
    SubTask,
}

impl ExpressionChronicle {
    fn build_hashset<T: GetVariables>(vec: &[T]) -> im::HashSet<AtomId> {
        let mut hashset: HashSet<AtomId> = Default::default();
        for e in vec {
            hashset = hashset.union(e.get_variables());
        }

        hashset
    }

    pub fn get_variables_in_set(&self, set: ChronicleSet) -> im::HashSet<AtomId> {
        match set {
            ChronicleSet::Effect => Self::build_hashset(&self.partial_chronicle.effects),
            ChronicleSet::Constraint => Self::build_hashset(&self.partial_chronicle.constraints),
            ChronicleSet::Condition => Self::build_hashset(&self.partial_chronicle.conditions),
            ChronicleSet::SubTask => Self::build_hashset(&self.partial_chronicle.subtasks),
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
    pub fn get_debug(&self) -> &LValue {
        &self.debug
    }
}

impl GetVariables for ExpressionChronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashset = self.partial_chronicle.get_variables();
        hashset.insert(*self.result.get_id());
        hashset.union(self.interval.get_variables())
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<AtomId> {
        self.get_variables()
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}

impl ExpressionChronicle {
    /*fn get_local_variables(&self, sym_table: &SymTable) -> HashSet<AtomId> {
        todo!()
    }*/

    pub fn get_symbol_variables(&self, sym_table: &SymTable) -> HashSet<AtomId> {
        let variables = self.get_variables();
        variables
            .iter()
            .filter(|a| sym_table.get_type_of(a).unwrap().a_type == Some(PlanningAtomType::Symbol))
            .cloned()
            .collect()
    }
}

//Creates a new expression chronicle, declaring an interval and a result variable in the symbol table.
//The LValue is used for debug
impl ExpressionChronicle {
    pub fn new(lv: LValue, st: &mut SymTable) -> Self {
        let interval = st.declare_new_interval();
        let result = st.declare_new_result(None);
        let mut ec = Self {
            interval,
            result,
            partial_chronicle: Default::default(),
            debug: lv,
        };

        ec.add_constraint(Constraint::LEq(
            interval.start().into(),
            interval.end().into(),
        ));
        ec
    }

    pub fn make_instantaneous(&mut self) {
        self.add_constraint(Constraint::Eq(
            self.interval.start().into(),
            self.interval.end().into(),
        ));
    }
}

impl ExpressionChronicle {
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.partial_chronicle.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.partial_chronicle.add_interval(interval);
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.partial_chronicle.add_constraint(constraint);
    }

    pub fn add_condition(&mut self, cond: Condition) {
        self.partial_chronicle.add_condition(cond)
    }

    pub fn add_effect(&mut self, effect: Effect) {
        self.partial_chronicle.add_effect(effect)
    }

    pub fn add_subtask(&mut self, sub_task: Expression) {
        self.partial_chronicle.add_subtask(sub_task)
    }
}

impl Absorb for ExpressionChronicle {
    fn absorb(&mut self, other: Self) {
        self.partial_chronicle.absorb(other.partial_chronicle);
        self.add_interval(&other.interval);
        self.add_var(other.result.get_id());
    }
}

impl FormatWithSymTable for ExpressionChronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let mut s = if self.result.is_pure() {
            format!(
                "{} {}\n",
                self.interval.format_with_sym_table(st),
                self.result
                    .get_pure()
                    .clone()
                    .unwrap()
                    .format_with_sym_table(st),
            )
        } else {
            format!(
                "{} {} <- {}\n",
                self.interval.format_with_sym_table(st),
                st.get_sym(self.result.get_id()),
                self.debug
            )
        };
        s.push_str(
            format!(
                "subchronicle: \n{}",
                self.partial_chronicle.format_with_sym_table(st)
            )
            .as_str(),
        );

        s
    }
}
