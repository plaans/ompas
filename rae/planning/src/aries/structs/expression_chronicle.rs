use crate::aries::structs::chronicle::{ChronicleResult, ChronicleSet};
use crate::aries::structs::condition::Condition;
use crate::aries::structs::constraint::Constraint;
use crate::aries::structs::effect::Effect;
use crate::aries::structs::expression::Expression;
use crate::aries::structs::interval::Interval;
use crate::aries::structs::lit::Lit;
use crate::aries::structs::partial_chronicle::PartialChronicle;
use crate::aries::structs::symbol_table::{AtomId, SymTable};
use crate::aries::structs::traits::{Absorb, FormatWithSymTable, GetVariables};
use crate::aries::structs::type_table::PlanningAtomType;
use im::HashSet;
use sompas_structs::lvalue::LValue;

#[derive(Clone)]
pub struct ExpressionChronicle {
    pub(crate) pc: PartialChronicle,
    debug: LValue,
}

impl ExpressionChronicle {
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

    pub fn set_pure_result(&mut self, result: Lit) {
        self.pc.result.set_pure(result)
    }
}

impl ExpressionChronicle {
    pub fn add_variables(&mut self, variables: HashSet<AtomId>) {
        self.pc.variables = self.pc.variables.clone().union(variables);
    }
}

impl ExpressionChronicle {
    pub fn get_presence(&self) -> &AtomId {
        &self.pc.presence
    }

    pub fn get_interval(&self) -> &Interval {
        self.pc.get_interval()
    }

    pub fn get_start(&self) -> &AtomId {
        self.pc.interval.start()
    }

    pub fn get_end(&self) -> &AtomId {
        self.pc.interval.end()
    }

    pub fn get_result_as_lit(&self) -> Lit {
        self.pc.get_result_as_lit()
    }

    pub fn get_result(&self) -> &ChronicleResult {
        self.pc.get_result()
    }

    pub fn get_result_id(&self) -> &AtomId {
        self.pc.get_result_id()
    }

    pub fn is_result_pure(&self) -> bool {
        self.pc.get_result().is_pure()
    }

    pub fn get_constraints(&self) -> &Vec<Constraint> {
        self.pc.get_constraints()
    }
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
    pub fn get_debug(&self) -> &LValue {
        &self.debug
    }
}

impl GetVariables for ExpressionChronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashset = self.pc.get_variables();
        hashset.insert(*self.pc.result.get_id());
        hashset.union(self.pc.interval.get_variables())
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
        Self {
            pc: PartialChronicle::new(st),
            debug: lv,
        }
    }

    pub fn make_instantaneous(&mut self) {
        self.add_constraint(Constraint::Eq(
            self.pc.interval.start().into(),
            self.pc.interval.end().into(),
        ));
    }
}

impl ExpressionChronicle {
    pub fn add_var(&mut self, sym_id: &AtomId) {
        self.pc.add_var(sym_id);
    }
    pub fn add_interval(&mut self, interval: &Interval) {
        self.pc.add_interval(interval);
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

impl Absorb for ExpressionChronicle {
    fn absorb(&mut self, other: Self) {
        self.pc.absorb(other.pc);
    }
}

impl FormatWithSymTable for ExpressionChronicle {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut s = if self.pc.result.is_pure() {
            format!(
                "{} {}\n",
                self.pc.interval.format(st, sym_version),
                self.pc
                    .result
                    .get_pure()
                    .clone()
                    .unwrap()
                    .format(st, sym_version),
            )
        } else {
            format!(
                "{} {} <- {}\n",
                self.pc.interval.format(st, sym_version),
                self.pc.result.get_id().format(st, sym_version),
                self.debug
            )
        };
        s.push_str(format!("subchronicle: \n{}", self.pc.format(st, sym_version)).as_str());

        s
    }
}
