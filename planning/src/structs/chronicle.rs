use crate::structs::condition::Condition;
use crate::structs::constraint::Constraint;
use crate::structs::effect::Effect;
use crate::structs::expression::Expression;
use crate::structs::interval::Interval;
use crate::structs::lit::Lit;
use crate::structs::symbol_table::{AtomId, SymTable};
use crate::structs::traits::{Absorb, FormatWithSymTable, GetVariables};
use im::HashSet;
use ompas_lisp::core::structs::lvalue::LValue;

#[derive(Clone, Default)]
pub struct Chronicle {
    name: Lit,
    task: Lit,
    partial_chronicle: PartialChronicle,
    debug: Option<LValue>,
}

impl FormatWithSymTable for Chronicle {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        let mut s = String::new();
        //name
        s.push_str(format!("-name: {}\n", self.name.format_with_sym_table(st)).as_str());
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
    pub fn absorb_expression_chronicle(&mut self, ec: ExpressionChronicle) {
        self.partial_chronicle.absorb(ec.partial_chronicle);
        //add result
        self.add_var(&ec.result.get_id());

        //add interval
        self.add_interval(&ec.interval);

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

impl GetVariables for Chronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        self.partial_chronicle.get_variables().clone()
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
        self.variables.insert(interval.start());
        self.variables.insert(interval.end());
    }

    pub fn add_constraint(&mut self, constraint: Constraint) {
        self.constraints.push(constraint);
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
}

#[derive(Clone, Default)]
pub struct ExpressionChronicleResult {
    id: AtomId,
    pure: Option<Lit>,
}

impl ExpressionChronicleResult {
    pub fn new(id: AtomId, pure: Option<Lit>) -> Self {
        Self { id, pure }
    }
}

impl ExpressionChronicleResult {
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

impl From<ExpressionChronicleResult> for Lit {
    fn from(ecr: ExpressionChronicleResult) -> Self {
        match ecr.pure {
            Some(lit) => lit.clone(),
            None => ecr.id.into(),
        }
    }
}

pub struct ExpressionChronicle {
    interval: Interval,
    result: ExpressionChronicleResult,
    partial_chronicle: PartialChronicle,
    value: Lit,
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
        &self.result.get_id()
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
    All,
}

impl ExpressionChronicle {
    pub fn get_variables_in_set(&self, set: ChronicleSet) -> im::HashSet<AtomId> {
        fn build_hashset<T: GetVariables>(vec: &Vec<T>) -> im::HashSet<AtomId> {
            let mut hashset: HashSet<AtomId> = Default::default();
            for e in vec {
                hashset = hashset.union(e.get_variables());
            }

            hashset
        }

        match set {
            ChronicleSet::Effect => build_hashset(&self.partial_chronicle.effects),
            ChronicleSet::Constraint => build_hashset(&self.partial_chronicle.constraints),
            ChronicleSet::Condition => build_hashset(&self.partial_chronicle.conditions),
            ChronicleSet::SubTask => build_hashset(&self.partial_chronicle.subtasks),
            ChronicleSet::All => build_hashset(&self.partial_chronicle.effects).union(
                build_hashset(&self.partial_chronicle.constraints)
                    .union(build_hashset(&self.partial_chronicle.conditions))
                    .union(build_hashset(&self.partial_chronicle.subtasks)),
            ),
        }
    }
}

impl GetVariables for ExpressionChronicle {
    fn get_variables(&self) -> HashSet<AtomId> {
        let mut hashset = self.partial_chronicle.get_variables();
        hashset.insert(*self.result.get_id());
        hashset.union(self.interval.get_variables())
    }
}

//Creates a new expression chronicle, declaring an interval and a result variable in the symbol table.
//The LValue is used for debug
impl ExpressionChronicle {
    pub fn new(lv: LValue, st: &mut SymTable) -> Self {
        let interval = st.declare_new_interval();
        let result = st.declare_new_result();
        let mut ec = Self {
            interval,
            result,
            partial_chronicle: Default::default(),
            value: Lit::Exp(vec![]),
            debug: lv,
        };

        ec.add_constraint(Constraint::LEq(
            interval.start().into(),
            interval.end().into(),
        ));
        ec
    }

    pub fn set_lit(&mut self, lit: Lit) {
        self.value = lit;
    }

    pub fn get_lit(&self) -> Lit {
        self.value.clone()
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
        self.add_var(&other.result.get_id());
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
