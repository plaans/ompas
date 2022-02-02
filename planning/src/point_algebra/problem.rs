use crate::point_algebra::relation_type::{RelationType, RelationTypeBit};
use std::hash::Hash;
use std::ops::{Index, IndexMut};

pub struct Relation<T> {
    i: T,
    j: T,
    relation_type: RelationType,
}

pub struct Problem<T> {
    variables: Vec<T>,
    relations: Vec<Relation<T>>,
}

impl<T: Clone + Hash + Eq> From<&Problem<T>> for Graph<T> {
    fn from(problem: &Problem<T>) -> Self {
        let mut var_key: im::HashMap<T, usize> = Default::default();
        let mut reverse: Vec<T> = Default::default();

        for (i, variable) in problem.variables.iter().enumerate() {
            var_key.insert(variable.clone(), i);
            reverse.push(variable.clone());
        }

        let mut inner = vec![vec![None; problem.variables.len()]; problem.variables.len()];
        for relation in &problem.relations {
            let index_1 = var_key.get(&relation.i).unwrap();
            let index_2 = var_key.get(&relation.j).unwrap();
            let relation_type_bit: RelationTypeBit = relation.relation_type.into();

            let (a, b, relation_type_bit) = if index_1 < index_2 {
                (index_1, index_2, relation_type_bit)
            } else {
                (index_2, index_1, !relation_type_bit)
            };

            if let Some(r) = inner[*a][*b] {
                inner[*a][*b] = Some(r & relation_type_bit);
            } else {
                inner[*a][*b] = Some(relation_type_bit)
            }
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
                if let Some(r) = r {
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
    inner: Vec<Vec<Option<RelationTypeBit>>>,
}

impl<T> Graph<T> {
    pub fn get_number_timepoints(&self) -> usize {
        self.reverse.len()
    }
}

impl<T> Index<Timepoint> for Graph<T> {
    type Output = Vec<Option<RelationTypeBit>>;

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
    type Output = Option<RelationTypeBit>;

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
