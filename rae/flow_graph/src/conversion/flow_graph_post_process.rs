use crate::structs::chronicle::FlatBindings;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::False;
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::{AtomId, EmptyDomains};
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::VecDeque;

const FLOW_GRAPH_POST_PROCESS: &str = "flow_graph_post_process";
const INVALID_FLOWS: &str = "invalid_flows";

pub fn flow_graph_post_processing(graph: &mut FlowGraph) -> Result<(), LRuntimeError> {
    let result_graph = graph.get_flow_result(&graph.flow);
    /*let domain_result = graph.sym_table.get_domain(&result_graph, false).unwrap();
    let new_domain = graph
        .sym_table
        .substract_domain(&domain_result, &RootType::Err.into());*/

    let emptys = graph
        .sym_table
        .substract_to_domain(&result_graph, RootType::Err);

    if let EmptyDomains::Some(empty) = emptys {
        return Err(LRuntimeError::new(
            FLOW_GRAPH_POST_PROCESS,
            format!(
                "{:?} have an empty domain",
                empty
                    .iter()
                    .map(|id| graph.sym_table.format_variable(id))
                    .collect::<Vec<String>>()
            ),
        ));
    }
    binding(graph).map_err(|e| e.chain(FLOW_GRAPH_POST_PROCESS))
}

pub enum PostProcess {
    Binding(AtomId, AtomId),
    Invalidation(FlowId),
    RemoveFlow(FlowId),
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
                    if let EmptyDomains::Some(emptys) =
                        graph.sym_table.try_union_atom(&vertice.result, &a)
                    {
                        println!("invalid flow(s)");
                        for e in &emptys {
                            invalid_flows(graph, e)?;
                        }
                    } else {
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
            } //FlowKind::Result(_, _) => {}
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
                        let emptys = sym_table.meet_to_domain(cond_result, False);

                        if let EmptyDomains::Some(vec) = emptys {
                            for e in vec {
                                invalid_flows(graph, &e)?;
                            }
                        } else {
                            graph.flows[parent].kind = FlowKind::Seq(vec![
                                branching.cond_flow,
                                branching.false_flow,
                                branching.result,
                            ]);
                        }
                    } else if flow_id == branching.false_flow {
                        let cond_result = &graph.get_flow_result(&branching.cond_flow);
                        let emptys = sym_table.meet_to_domain(cond_result, False);

                        if let EmptyDomains::Some(vec) = emptys {
                            for e in vec {
                                invalid_flows(graph, &e)?;
                            }
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
                } //FlowKind::Result(_, _) => {}
            }
        }
    }

    Ok(())
}
