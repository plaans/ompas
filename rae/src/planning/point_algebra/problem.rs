use crate::planning::point_algebra::relation_type::RelationType::Tautology;
use crate::planning::point_algebra::relation_type::{RelationType, RelationTypeBit};
use crate::planning::structs::symbol_table::SymTable;
use crate::planning::structs::traits::FormatWithSymTable;
use cli_table::{print_stdout, Cell, Table};
use std::fmt::{Debug, Display, Formatter};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

pub struct Relation<T> {
    i: T,
    j: T,
    relation_type: RelationType,
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

impl<T: Debug> Debug for Relation<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?} {} {:?}", self.i, self.relation_type, self.j,)
    }
}

impl<T: FormatWithSymTable> FormatWithSymTable for Relation<T> {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        format!(
            "{} {} {}",
            self.i.format_with_sym_table(st),
            self.relation_type,
            self.j.format_with_sym_table(st),
        )
    }
}

pub struct Problem<T> {
    variables: Vec<T>,
    relations: Vec<Relation<T>>,
}

impl<T> Problem<T> {
    pub fn new(variables: Vec<T>, relations: Vec<Relation<T>>) -> Self {
        Self {
            variables,
            relations,
        }
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
    fn format_with_sym_table(&self, st: &SymTable) -> String {
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
                    variable.format_with_sym_table(st).as_str(),
                    variable
                )
                .as_str(),
            );
            first = false;
        }

        str.push_str("}\n");

        str.push_str("relations:\n");

        for relation in &self.relations {
            str.push_str(format!("-{}\n", relation.format_with_sym_table(st)).as_str());
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

            let (a, b, relation_type_bit) = if index_1 < index_2 {
                (index_1, index_2, relation_type_bit)
            } else {
                (index_2, index_1, relation_type_bit.converse())
            };

            inner[*a][*b] = inner[*a][*b] & relation_type_bit;
            inner[*b][*a] = inner[*a][*b].converse()
        }

        Self { reverse, inner }
    }
}

impl<T: Clone + Eq + Hash> From<Graph<T>> for Problem<T> {
    fn from(g: Graph<T>) -> Self {
        let variables = g.reverse.clone();
        let mut relations = vec![];

        for (i, i_relations) in g.inner.iter().enumerate() {
            for (j, r) in i_relations[i + 1..].iter().enumerate() {
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
            relations,
        }
    }
}

pub type Timepoint = usize;

pub struct Graph<T> {
    reverse: Vec<T>,
    inner: Vec<Vec<RelationTypeBit>>,
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
