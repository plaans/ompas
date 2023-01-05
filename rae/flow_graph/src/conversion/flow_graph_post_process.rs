use crate::structs::chronicle::FlatBindings;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::{False, True};
use crate::structs::domain::Domain;
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::{BlockId, FlowGraph};
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::AtomId;
use log::Level::Debug;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::VecDeque;

const FLOW_GRAPH_POST_PROCESS: &str = "flow_graph_post_process";
const INVALID_FLOWS: &str = "invalid_flows";

pub fn flow_graph_post_processing(graph: &mut FlowGraph) -> Result<(), LRuntimeError> {
    let result_graph = graph.get_flow_result(&graph.flow);
    let domain_result = graph.sym_table.get_domain(&result_graph, false).unwrap();
    let new_domain = graph
        .sym_table
        .substract_domain(&domain_result, &RootType::Err.into());
    if !graph.sym_table.set_domain(&result_graph, new_domain) {
        return Err(LRuntimeError::new(
            FLOW_GRAPH_POST_PROCESS,
            format!(
                "{} has an empty domain",
                graph.sym_table.get_debug(&result_graph)
            ),
        ));
    }
    binding(graph).map_err(|e| e.chain(FLOW_GRAPH_POST_PROCESS))
}

pub enum PostProcessConstraint {
    Binding(AtomId, AtomId),
    Invalidation(BlockId),
    UnionBinding(AtomId, (AtomId, AtomId)),
}

pub fn binding(graph: &mut FlowGraph) -> Result<(), LRuntimeError> {
    let sym_table = graph.sym_table.clone();
    let mut flows_queue: VecDeque<FlowId> = Default::default();
    flows_queue.push_back(graph.flow);

    while let Some(flow_id) = flows_queue.pop_front() {
        let flow = graph.flows[flow_id].clone();
        match flow.kind {
            FlowKind::Vertice(v) => {
                let vertice = graph.vertices[v].clone();
                if let Lit::Atom(a) = &vertice.lit {
                    if graph.sym_table.try_union_atom(&vertice.result, &a) {
                        graph.remove_flow(&flow_id);
                    } else {
                        invalid_flows(graph, &vertice.result)?;
                    }
                }
            }
            FlowKind::Seq(s) => {
                for f in s {
                    flows_queue.push_back(f)
                }
            }
            FlowKind::Branching(b) => {
                flows_queue.push_back(b.cond_flow);
                flows_queue.push_back(b.true_flow);
                flows_queue.push_back(b.false_flow);
                flows_queue.push_back(b.result);
            }
        }
    }

    graph.flat_bindings(&sym_table);
    Ok(())
}

pub fn invalid_flows(graph: &mut FlowGraph, invalid_atom: &AtomId) -> Result<(), LRuntimeError> {
    let vertices = graph.map_atom_id_flow_id.get(invalid_atom).unwrap();
    let mut flows: Vec<FlowId> = vertices
        .iter()
        .map(|id| graph.map_vertice_id_flow_id.get(id).unwrap())
        .cloned()
        .collect();

    let mut sym_table = graph.sym_table.clone();

    while let Some(flow_id) = flows.pop() {
        let flow = &mut graph.flows[flow_id];
        flow.valid = false;

        if let Some(parent) = flow.parent {
            match &graph.flows[parent].kind {
                FlowKind::Vertice(_) => unreachable!(),
                FlowKind::Seq(_) => {
                    flows.push(parent);
                }
                FlowKind::Branching(branching) => {
                    if flow_id == branching.cond_flow || flow_id == branching.result {
                        flows.push(parent)
                    } else if flow_id == branching.true_flow {
                        let cond_result = &graph.get_flow_result(&branching.cond_flow);
                        let domain = sym_table.meet_domain(
                            &sym_table.get_domain(cond_result, true).unwrap(),
                            &False.into(),
                        );
                        if !sym_table.set_domain(cond_result, domain) {
                            invalid_flows(graph, cond_result)?;
                        } else {
                            graph.flows[parent].kind = FlowKind::Seq(vec![
                                branching.cond_flow,
                                branching.false_flow,
                                branching.result,
                            ]);
                        }
                    } else if flow_id == branching.false_flow {
                        let cond_result = &graph.get_flow_result(&branching.cond_flow);
                        let domain = sym_table.meet_domain(
                            &sym_table.get_domain(cond_result, true).unwrap(),
                            &True.into(),
                        );
                        if !sym_table.set_domain(cond_result, domain) {
                            invalid_flows(graph, cond_result)?;
                        } else {
                            graph.flows[parent].kind = FlowKind::Seq(vec![
                                branching.cond_flow,
                                branching.true_flow,
                                branching.result,
                            ]);
                        }
                    } else {
                        panic!("flow is not part of one of the branch");
                    }
                }
            }
        }
    }

    Ok(())
}
