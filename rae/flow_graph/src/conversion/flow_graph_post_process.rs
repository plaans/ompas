use crate::structs::chronicle::FlatBindings;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::{False, True};
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
        match &flow.kind {
            FlowKind::Assignment(ass) => {
                if let Lit::Atom(a) = &ass.lit {
                    if let EmptyDomains::Some(emptys) =
                        graph.sym_table.try_union_atom(&ass.result, &a)
                    {
                        println!("invalid flow(s)");
                        for e in &emptys {
                            invalid_flows(graph, e)?;
                        }
                    } else {
                        let parent_flow = &mut graph.flows[flow.parent.unwrap()];
                        if let FlowKind::Seq(s, _) = &mut parent_flow.kind {
                            s.retain(|f| *f != flow_id)
                        }
                    }
                }
            }
            FlowKind::Seq(s, _) => {
                for f in s {
                    flows_queue.push_back(*f)
                }
            }
            FlowKind::Branching(b) => {
                flows_queue.push_back(b.cond_flow);
                flows_queue.push_back(b.true_flow);
                flows_queue.push_back(b.false_flow);
                flows_queue.push_back(b.result);
            }
            FlowKind::FlowResult(_) => {}
        }
    }

    graph.flat_bindings(&sym_table);
    Ok(())
}

pub fn invalid_flows(graph: &mut FlowGraph, invalid_atom: &AtomId) -> Result<(), LRuntimeError> {
    let mut flows = graph
        .map_atom_id_flow_id
        .get(invalid_atom)
        .unwrap_or(&vec![])
        .clone();

    let mut sym_table = graph.sym_table.clone();

    while let Some(flow_id) = flows.pop() {
        let flow = &mut graph.flows[flow_id];
        flow.valid = false;

        if let Some(parent) = flow.parent {
            let flow = graph.flows[parent].clone();
            match &flow.kind {
                FlowKind::Assignment(_) => unreachable!(),
                FlowKind::Seq(_, _) => {
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
                            let new_flow = graph.merge_flows(vec![
                                branching.cond_flow,
                                branching.false_flow,
                                branching.result,
                            ]);
                            let grand_parent = &mut graph.flows[flow.parent.unwrap()];

                            //Awful code
                            if let FlowKind::Seq(vec, _) = &mut grand_parent.kind {
                                for f in vec.iter_mut() {
                                    if *f == parent {
                                        *f = new_flow;
                                        break;
                                    }
                                }
                            } else {
                                unreachable!()
                            }
                        }
                    } else if flow_id == branching.false_flow {
                        let cond_result = &graph.get_flow_result(&branching.cond_flow);
                        let emptys = sym_table.meet_to_domain(cond_result, True);

                        if let EmptyDomains::Some(vec) = emptys {
                            for e in vec {
                                invalid_flows(graph, &e)?;
                            }
                        } else {
                            let new_flow = graph.merge_flows(vec![
                                branching.cond_flow,
                                branching.true_flow,
                                branching.result,
                            ]);
                            let grand_parent = &mut graph.flows[flow.parent.unwrap()];

                            if let FlowKind::Seq(vec, _) = &mut grand_parent.kind {
                                for f in vec.iter_mut() {
                                    if *f == parent {
                                        *f = new_flow;
                                        break;
                                    }
                                }
                            } else {
                                unreachable!()
                            }
                        }
                    } else {
                        panic!("flow is not part of one of the branch");
                    }
                }
                FlowKind::FlowResult(_) => {}
            }
        }
    }

    Ok(())
}
