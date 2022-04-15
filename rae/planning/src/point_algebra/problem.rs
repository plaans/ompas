use crate::point_algebra::relation_type::RelationType::Tautology;
use crate::point_algebra::relation_type::{RelationType, RelationTypeBit};
use cli_table::{print_stdout, Cell, Table};
use ompas_rae_structs::planning::constraint::Constraint;
use ompas_rae_structs::planning::lit::Lit;
use ompas_rae_structs::planning::symbol_table::{AtomId, SymTable};
use ompas_rae_structs::planning::traits::FormatWithSymTable;
use ompas_rae_structs::planning::type_table::PlanningAtomType;
use sompas_structs::lerror;
use sompas_structs::lerror::LError;
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

pub struct Relation<T> {
    i: T,
    j: T,
    relation_type: RelationType,
}

pub fn try_into_pa_relation(
    constraint: &Constraint,
    sym_table: &SymTable,
) -> lerror::Result<Relation<AtomId>> {
    let relation_type = match constraint {
        Constraint::Eq(_, _) => RelationType::Eq,
        Constraint::Leq(_, _) => RelationType::LEq,
        Constraint::Lt(_, _) => RelationType::LT,
        Constraint::Neq(_, _) => RelationType::Neq,
        _ => return Err(LError::default()),
    };

    if let Ok(i) = constraint.get_left().try_into() {
        let p_i = sym_table.get_parent(&i);
        if let Ok(j) = constraint.get_right().try_into() {
            let p_j = sym_table.get_parent(&j);
            if sym_table.get_type_of(p_i).unwrap().a_type == Some(PlanningAtomType::Timepoint) {
                if sym_table.get_type_of(p_j).unwrap().a_type == Some(PlanningAtomType::Timepoint) {
                    Ok(Relation::new(*p_i, *p_j, relation_type))
                } else {
                    Err(Default::default())
                }
            } else {
                Err(Default::default())
            }
        } else {
            Err(Default::default())
        }
    } else {
        Err(Default::default())
    }
}

impl<T> Relation<T> {
    pub fn new(i: T, j: T, relation_type: RelationType) -> Self {
        Self {
            i,
            j,
            relation_type,
        }
    }
}

impl From<&Relation<AtomId>> for Constraint {
    fn from(r: &Relation<AtomId>) -> Self {
        match &r.relation_type {
            RelationType::Eq => Constraint::Eq(r.i.into(), r.j.into()),
            RelationType::GT => Constraint::Lt(r.j.into(), r.i.into()),
            RelationType::LT => Constraint::Lt(r.i.into(), r.j.into()),
            RelationType::GEq => Constraint::Leq(r.j.into(), r.i.into()),
            RelationType::LEq => Constraint::Leq(r.i.into(), r.j.into()),
            RelationType::Neq => Constraint::Not(Lit::Constraint(Box::new(Constraint::Eq(
                r.i.into(),
                r.j.into(),
            )))),
            RelationType::Contradiction => panic!("contradiction in the networkd"),
            RelationType::Tautology => panic!("should not have been extracted"),
        }
    }
}

impl<T: Debug> Debug for Relation<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {:?}", self.i, self.relation_type, self.j,)
    }
}

impl<T: FormatWithSymTable> FormatWithSymTable for Relation<T> {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        format!(
            "{} {} {}",
            self.i.format(st, sym_version),
            self.relation_type,
            self.j.format(st, sym_version),
        )
    }
}

pub struct Problem<T> {
    variables: Vec<T>,
    optional: Vec<bool>,
    relations: Vec<Relation<T>>,
}

impl<T> Problem<T> {
    pub fn new(mut vars: Vec<(T, bool)>, relations: Vec<Relation<T>>) -> Self {
        let mut variables = vec![];
        let mut optional = vec![];

        while let Some((val, opt)) = vars.pop() {
            variables.push(val);
            optional.push(opt);
        }

        Self {
            variables,
            optional,
            relations,
        }
    }

    pub fn get_relations(&self) -> &Vec<Relation<T>> {
        &self.relations
    }
}

impl<T: Debug> Debug for Problem<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "problem:\n".to_string();
        str.push_str("variables: {");
        str.push_str(format!("variables: {:#?}\n", self.variables).as_str());
        str.push_str(format!("relations: {:#?}\n", self.relations).as_str());
        write!(f, "{}", str)
    }
}

impl<T: Display + FormatWithSymTable> FormatWithSymTable for Problem<T> {
    fn format(&self, st: &SymTable, sym_version: bool) -> String {
        let mut str = "problem:\n".to_string();
        str.push_str("variables: {");
        let mut first = true;
        for variable in &self.variables {
            if !first {
                str.push(',')
            }
            str.push_str(
                format!(
                    "{}({})",
                    variable.format(st, sym_version).as_str(),
                    variable
                )
                .as_str(),
            );
            first = false;
        }

        str.push_str("}\n");

        str.push_str("relations:\n");

        for relation in &self.relations {
            str.push_str(format!("-{}\n", relation.format(st, sym_version)).as_str());
        }
        str
    }
}

impl<T: Clone + Hash + Eq + Debug> From<&Problem<T>> for Graph<T> {
    fn from(problem: &Problem<T>) -> Self {
        let mut var_key: im::HashMap<T, usize> = Default::default();
        let mut reverse: Vec<T> = Default::default();

        for (i, variable) in problem.variables.iter().enumerate() {
            var_key.insert(variable.clone(), i);
            reverse.push(variable.clone());
        }

        let mut inner: Vec<Vec<RelationTypeBit>> =
            vec![vec![Tautology.into(); problem.variables.len()]; problem.variables.len()];
        for relation in &problem.relations {
            let index_1 = var_key.get(&relation.i).unwrap_or_else(|| {
                panic!("{:?} is not defined in {:?}", relation.i, problem.variables)
            });
            let index_2 = var_key.get(&relation.j).unwrap_or_else(|| {
                panic!("{:?} is not defined in {:?}", relation.j, problem.variables)
            });
            let relation_type_bit: RelationTypeBit = relation.relation_type.into();

            if index_1 == index_2 {
                if relation_type_bit.intersect(&RelationType::Eq.into())
                    == RelationType::Contradiction.into()
                {
                    panic!(
                        "relation on a single timepoint as it to be different from himself, gneeee"
                    );
                }
            } else {
                let (a, b, relation_type_bit) = if index_1 < index_2 {
                    (index_1, index_2, relation_type_bit)
                } else {
                    (index_2, index_1, relation_type_bit.converse())
                };

                inner[*a][*b] = inner[*a][*b] & relation_type_bit;
                inner[*b][*a] = inner[*a][*b].converse()
            }
        }

        Self {
            reverse,
            optional: problem.optional.clone(),
            inner,
        }
    }
}

impl<T: Clone + Eq + Hash> From<Graph<T>> for Problem<T> {
    fn from(g: Graph<T>) -> Self {
        let variables = g.reverse.clone();
        let mut relations = vec![];

        for (i, i_relations) in g.inner.iter().enumerate() {
            for (j, r) in i_relations[..i].iter().enumerate() {
                if r != &Tautology.into() {
                    relations.push(Relation {
                        i: g.reverse[i].clone(),
                        j: g.reverse[j].clone(),
                        relation_type: r.into(),
                    })
                }
            }
        }
        Self {
            variables,
            optional: g.optional,
            relations,
        }
    }
}

pub type Timepoint = usize;

pub struct Graph<T> {
    pub reverse: Vec<T>,
    pub optional: Vec<bool>,
    pub inner: Vec<Vec<RelationTypeBit>>,
}

impl<T> Graph<T> {
    pub fn get_number_timepoints(&self) -> usize {
        self.reverse.len()
    }
}

impl<T> Index<Timepoint> for Graph<T> {
    type Output = Vec<RelationTypeBit>;

    fn index(&self, index: usize) -> &Self::Output {
        &self.inner[index]
    }
}

impl<T> IndexMut<usize> for Graph<T> {
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        &mut self.inner[index]
    }
}

impl<T> Index<(Timepoint, Timepoint)> for Graph<T> {
    type Output = RelationTypeBit;

    fn index(&self, index: (Timepoint, Timepoint)) -> &Self::Output {
        &self.inner[index.0][index.1]
    }
}

impl<T> IndexMut<(Timepoint, Timepoint)> for Graph<T> {
    fn index_mut(&mut self, index: (Timepoint, Timepoint)) -> &mut Self::Output {
        &mut self.inner[index.0][index.1]
    }
}

impl<T> Graph<T> {
    pub fn remove_var(&mut self, index: usize) {
        self.reverse.remove(index);
        self.inner.remove(index);
        self.optional.remove(index);
        for e in &mut self.inner {
            e.remove(index);
        }
    }
}

impl<T: Display> Graph<T> {
    pub fn print(&self) {
        let mut table = vec![];

        let mut first_vec = vec!["".cell()];
        first_vec.append(&mut self.reverse.iter().map(|t| t.cell()).collect());
        table.push(first_vec);
        for (l, t) in self.reverse.iter().enumerate() {
            let mut vec = vec![t.cell()];
            let line: &Vec<RelationTypeBit> = &self[l];
            vec.append(
                &mut line
                    .clone()
                    .iter()
                    .map(|r| {
                        if *r == Tautology.into() {
                            "".cell()
                        } else {
                            RelationType::from(r).cell()
                        }
                    })
                    .collect(),
            );
            table.push(vec);
        }

        print_stdout(table.table()).expect("error printing graph as table");
    }
}
