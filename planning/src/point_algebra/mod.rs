use crate::point_algebra::problem::{Graph, Timepoint};
use crate::point_algebra::relation_type::RelationType;
use std::collections::VecDeque;

mod problem;
mod relation_type;

pub type Fail = ();

/// Algorithme from paper A. Gerevini / Artificial Intelligence 166 (2005) 37–80
/// 1. Q := {(i,j ) | i<j }
/// 2. while Q = ∅ do
/// 3.      select and delete an element from Q;
/// 4.      for k = i, k = j do
/// 5.          if REVISE(i, j, k) then
/// 6.              if M[i, k]=⊥ then return fail
/// 7.              else add (i, k) to the end of Q;
/// 8.          if REVISE(k, i, j ) then
/// 9.              if M[k,j ]=⊥ then return fail
/// 10.             else add (k, j ) to the end of Q;
/// 11. return m.
/// Subroutine: REVISE(i, k, j )
/// 1. if M[i, k] or M[k,j ] is the universal relation then return false;
/// 2. S := M[i, k] ◦ M[k,j ];
/// 3. if M[i, j ] ⊆ S then return false
/// 4. M[i, j ] := M[i, j]
pub fn path_consistency<T>(mut m: Graph<T>) -> Result<Graph<T>, Fail> {
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
                if m[(i, j)] == Some(RelationType::Contradiction.into()) {
                    return Err(());
                } else {
                    q.push_back((i, k))
                }
            }
            if revise(&mut m, k, i, j) {
                if m[(k, j)] == Some(RelationType::Contradiction.into()) {
                    return Err(());
                } else {
                    q.push_back((k, j))
                }
            }
        }
    }

    return Ok(m);
}

pub fn revise<T>(m: &mut Graph<T>, i: Timepoint, j: Timepoint, k: Timepoint) -> bool {
    let r1 = &m[(i, k)];
    let r2 = &m[(k, j)];
    if *r1 == None || *r2 == None {
        return false;
    }

    let s = r1.unwrap().compose(&r2.unwrap());

    if m[(i, j)].unwrap().included_in(&s) {
        return false;
    }
    m[(i, j)] = Some(m[(i, j)].unwrap().intersect(&s));
    m[(j, i)] = Some(!m[(i, j)].unwrap());
    return true;
}
