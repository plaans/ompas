use crate::aries::structs::chronicle::ChronicleResult;
use crate::aries::structs::condition::Condition;
use crate::aries::structs::constraint::Constraint;
use crate::aries::structs::effect::Effect;
use crate::aries::structs::expression::Expression;
use crate::aries::structs::interval::Interval;
use crate::aries::structs::lit::Lit;
use crate::aries::structs::symbol_table::{SymTable, VarId};
use crate::aries::structs::traits::{Absorb, FormatWithParent, FormatWithSymTable, GetVariables};
use crate::aries::structs::type_table::PlanningAtomType;
use im::{hashset, HashSet};
use std::borrow::Borrow;
use std::ops::Deref;

#[derive(Clone)]
pub struct PartialChronicle {
    pub presence: VarId,
    pub interval: Interval,
    pub result: ChronicleResult,
    pub variables: HashSet<VarId>,
    pub constraints: Vec<Constraint>,
    pub conditions: Vec<Condition>,
    pub effects: Vec<Effect>,
    pub subtasks: Vec<Expression>,
}

impl PartialChronicle {
    pub fn new(st: &mut SymTable) -> Self {
        let interval = st.declare_new_interval();
        let result = st.declare_new_result(None);
        let presence = st.declare_new_presence();
        let variables = hashset![*interval.start(), *interval.end(), result, presence];

        let constraints = vec![Constraint::Leq(
            interval.start().into(),
            interval.end().into(),
        )];

        Self {
            presence,
            interval,
            result: result.into(),
            variables,
            constraints,
            conditions: vec![],
            effects: vec![],
            subtasks: vec![],
        }
    }
}

impl PartialChronicle {
    pub fn get_presence(&self) -> &VarId {
        &self.presence
    }

    pub fn get_interval(&self) -> &Interval {
        &self.interval
    }

    pub fn get_result(&self) -> &ChronicleResult {
        &self.result
    }

    pub fn get_result_id(&self) -> &VarId {
        self.result.get_id()
    }

    pub fn get_result_as_lit(&self) -> Lit {
        self.result.borrow().into()
    }
}

impl PartialChronicle {
    pub fn rm_var(&mut self, sym_id: &VarId) {
        self.variables.remove(sym_id);
    }

    pub fn rm_set_var(&mut self, ids: Vec<VarId>) {
        for id in ids {
            self.rm_var(&id);
        }
    }

    pub fn rm_constraint(&mut self, index: usize) {
        self.constraints.remove(index);
    }

    pub fn rm_condition(&mut self, index: usize) {
        self.conditions.remove(index);
    }

    pub fn rm_set_constraint(&mut self, mut indexes: Vec<usize>) {
        indexes.sort_unstable();
        indexes.reverse();
        for index in indexes {
            self.rm_constraint(index);
        }
    }
}

impl PartialChronicle {
    pub fn add_var(&mut self, sym_id: &VarId) {
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

impl FormatWithSymTable for PartialChronicle {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut s = String::new();
        s.push_str(
            format!(
                "{}: {} {}\n",
                self.presence.format(st, sym_version),
                self.interval.format(st, sym_version),
                self.result.format(st, sym_version)
            )
            .as_str(),
        );
        s.push_str("-variable(s): {");

        let mut variables = self
            .variables
            .iter()
            .map(|id| {
                /*format!(
                    "({}){}({})",
                    id,
                    id.format_with_sym_table(st, sym_version),
                    st.get_type_of(id)
                        .unwrap()
                        .a_type
                        .format_with_sym_table(st, sym_version)
                )*/

                format!(
                    "{}({})",
                    id.format(st, sym_version),
                    st.get_type_of(id).unwrap().format(st, sym_version)
                )
            })
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
            s.push_str(c.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");

        //conditions
        s.push_str("-conditon(s): {\n");
        for e in &self.conditions {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //effects
        s.push_str("-effect(s): {\n");
        for e in &self.effects {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        //substasks
        s.push_str("-subtask(s): {\n");
        for e in &self.subtasks {
            s.push('\t');
            s.push_str(e.format(st, sym_version).as_str());
            s.push('\n');
        }
        s.push_str("}\n");
        s
    }
}

impl FormatWithParent for PartialChronicle {
    fn format_with_parent(&mut self, st: &SymTable) {
        let old_variables = self.variables.clone();
        let mut new_variables: HashSet<VarId> = Default::default();
        for v in &old_variables {
            let mut v = *v;
            v.format_with_parent(st);
            new_variables.insert(v);
        }

        self.variables = new_variables;
        self.interval.format_with_parent(st);
        self.presence.format_with_parent(st);
        self.constraints.format_with_parent(st);
        self.conditions.format_with_parent(st);
        self.effects.format_with_parent(st);
        self.subtasks.format_with_parent(st);
    }
}

impl Absorb for PartialChronicle {
    fn absorb(&mut self, mut other: Self) {
        self.add_constraint(Constraint::Eq(self.presence.into(), other.presence.into()));
        self.variables = self.variables.clone().union(other.variables);
        self.constraints.append(&mut other.constraints);
        self.conditions.append(&mut other.conditions);
        self.effects.append(&mut other.effects);
        self.subtasks.append(&mut other.subtasks);
    }
}

impl GetVariables for PartialChronicle {
    fn get_variables(&self) -> HashSet<VarId> {
        self.variables.clone()
    }

    fn get_variables_of_type(
        &self,
        sym_table: &SymTable,
        atom_type: &Option<PlanningAtomType>,
    ) -> HashSet<VarId> {
        self.variables
            .iter()
            .filter(|v| sym_table.get_type_of(v).unwrap().a_type == *atom_type)
            .cloned()
            .collect()
    }
}
