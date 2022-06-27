use crate::point_algebra::problem::{Graph, Timepoint};
use crate::point_algebra::relation_type::RelationType;
use crate::point_algebra::relation_type::RelationType::Tautology;
use sompas_structs::lruntimeerror;
use std::collections::VecDeque;

pub mod problem;
pub mod relation_type;

pub type Fail = ();

// Algorithme from paper A. Gerevini / Artificial Intelligence 166 (2005) 37–80
// 1. Q := {(i,j ) | i<j }
// 2. while Q = ∅ do
// 3.      select and delete an element from Q;
// 4.      for k = i, k = j do
// 5.          if REVISE(i, j, k) then
// 6.              if M[i, k]=⊥ then return fail
// 7.              else add (i, k) to the end of Q;
// 8.          if REVISE(k, i, j ) then
// 9.              if M[k,j ]=⊥ then return fail
// 10.             else add (k, j ) to the end of Q;
// 11. return m.
//
// Subroutine: REVISE(i, k, j )
// 1. if M[i, k] or M[k,j ] is the universal relation then return false;
// 2. S := M[i, k] ◦ M[k,j ];
// 3. if M[i, j ] ⊆ S then return false
// 4. M[i,j ] := M[i, j] intersect S
// 5. M[j,i ] := converse(M[i, j])
pub fn path_consistency<T>(mut m: Graph<T>) -> lruntimeerror::Result<Graph<T>> {
    // Q := {(i,j ) | i<j }
    let mut q: VecDeque<(Timepoint, Timepoint)> = Default::default();
    let n = m.get_number_timepoints();
    for i in 0..n {
        for j in i + 1..n {
            q.push_front((i, j));
        }
    }

    while let Some((i, j)) = q.pop_front() {
        for k in (0..n).filter(|k| *k != i && *k != j) {
            if revise(&mut m, i, j, k) {
                if m[(i, j)] == RelationType::Contradiction.into() {
                    return Err(Default::default());
                } else {
                    q.push_back((i, k))
                }
            }
            if revise(&mut m, k, i, j) {
                if m[(k, j)] == RelationType::Contradiction.into() {
                    return Err(Default::default());
                } else {
                    q.push_back((k, j))
                }
            }
        }
    }

    Ok(m)
}

pub fn revise<T>(m: &mut Graph<T>, i: Timepoint, j: Timepoint, k: Timepoint) -> bool {
    let r1 = &m[(i, k)];
    let r2 = &m[(k, j)];
    if *r1 == Tautology.into() || *r2 == Tautology.into() {
        return false;
    }

    let s = r1.compose(r2);

    if m[(i, j)].included_in(&s) {
        return false;
    }
    m[(i, j)] = m[(i, j)].intersect(&s);
    m[(j, i)] = m[(i, j)].converse();
    true
}

pub fn remove_useless_timepoints<T>(mut m: Graph<T>) -> lruntimeerror::Result<Graph<T>> {
    let mut last_index = 0;
    let mut done = false;

    while !done {
        done = true;
        for (i, t) in m.optional[last_index..].iter().enumerate() {
            if *t {
                let mut linked_timepoints = vec![];

                for (i, r) in m.inner[i].iter().enumerate() {
                    if *r != Tautology.into() {
                        linked_timepoints.push(i);
                    }
                }

                match linked_timepoints.len() {
                    0..=1 => {
                        m.remove_var(i);
                        last_index = i;
                        done = false;
                        break;
                    }
                    2 => {
                        let first = linked_timepoints[0];
                        let second = linked_timepoints[1];
                        m[first][second] = m[first][i].compose(&m[i][second]);
                        m[second][first] = m[first][second].converse();
                        m.remove_var(i);
                        last_index = i;
                        done = false;
                        break;
                    }
                    _ => {}
                }
            }
        }
    }

    Ok(m)
}
