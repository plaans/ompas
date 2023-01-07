use crate::conversion::flow_graph_post_process::PostProcess::*;
use crate::structs::chronicle::FlatBindings;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::{False, True};
use crate::structs::domain::Domain;
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

    let mut post_process = binding_constraints(graph);
    post_process.push_front(Subtract(result_graph, RootType::Err.into()));
    propagate(graph, post_process).map_err(|e| e.chain(FLOW_GRAPH_POST_PROCESS))
}

pub enum PostProcess {
    Subtract(AtomId, Domain),
    Meet(AtomId, Domain),
    Bind(AtomId, AtomId),
    Invalid(FlowId),
}

pub fn propagate(
    graph: &mut FlowGraph,
    mut queue: VecDeque<PostProcess>,
) -> Result<(), LRuntimeError> {
    while let Some(post_process) = queue.pop_front() {
        match post_process {
            Subtract(id, d) => {
                let emptys = graph.sym_table.substract_to_domain(&id, d);
                if let EmptyDomains::Some(emptys) = emptys {
                    println!("[Subtract] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Meet(id, d) => {
                let emptys = graph.sym_table.meet_to_domain(&id, d);
                if let EmptyDomains::Some(emptys) = emptys {
                    println!("[Meet] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Bind(id1, id2) => {
                if let EmptyDomains::Some(emptys) = graph.sym_table.try_union_atom(&id1, &id2) {
                    println!("[Bind] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Invalid(ref id) => {
                if graph.is_valid(id) {
                    graph.invalidate(id);
                    let kind = graph.get_kind(id).clone();
                    match kind {
                        FlowKind::Branching(branching) => {
                            if !graph.is_valid(&branching.true_flow)
                                && graph.is_valid(&branching.false_flow)
                            {
                                let cond_result = &graph.get_flow_result(&branching.cond_flow);
                                queue.push_back(Meet(*cond_result, False.into()));
                                let new_flow = graph
                                    .merge_flows(vec![branching.cond_flow, branching.false_flow]);

                                graph.set_kind(id, new_flow);
                                //graph.update_flow(&flow.parent.unwrap());
                            } else if graph.is_valid(&branching.true_flow)
                                && !graph.is_valid(&branching.false_flow)
                            {
                                let cond_result = &graph.get_flow_result(&branching.cond_flow);
                                queue.push_back(Meet(*cond_result, True.into()));
                                let new_flow = graph
                                    .merge_flows(vec![branching.cond_flow, branching.true_flow]);
                                graph.set_kind(id, new_flow);
                            } else {
                                queue.push_back(Invalid(graph.get_parent(id).unwrap()));
                            }
                        }
                        _ => {
                            if let Some(parent) = graph.get_parent(id) {
                                queue.push_back(Invalid(*parent))
                            }
                        }
                    }
                }
            }
        }
    }
    graph.flat_bindings();

    Ok(())
}

pub fn binding_constraints(graph: &mut FlowGraph) -> VecDeque<PostProcess> {
    let mut flows_queue: VecDeque<FlowId> = Default::default();
    flows_queue.push_back(graph.flow);
    for handle in graph.handles.inner().values() {
        flows_queue.push_back(handle.flow)
    }

    let mut post_process = VecDeque::new();

    while let Some(flow_id) = flows_queue.pop_front() {
        let flow = graph.flows[flow_id].clone();
        match &flow.kind {
            FlowKind::Assignment(ass) => {
                if let Lit::Atom(a) = &ass.lit {
                    post_process.push_back(Bind(ass.result, *a));
                    let parent_flow = &mut graph.flows[flow.parent.unwrap()];
                    if let FlowKind::Seq(s, _) = &mut parent_flow.kind {
                        s.retain(|f| *f != flow_id)
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
    post_process
}

/*
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
                            let new_flow =
                                graph.merge_flows(vec![branching.cond_flow, branching.false_flow]);

                            graph.flows[parent].kind = new_flow;

                            graph.update_flow(&flow.parent.unwrap());
                        }
                    } else if flow_id == branching.false_flow {
                        let cond_result = &graph.get_flow_result(&branching.cond_flow);
                        let emptys = sym_table.meet_to_domain(cond_result, True);

                        if let EmptyDomains::Some(vec) = emptys {
                            for e in vec {
                                invalid_flows(graph, &e)?;
                            }
                        } else {
                            let new_flow =
                                graph.merge_flows(vec![branching.cond_flow, branching.true_flow]);

                            graph.flows[parent].kind = new_flow;

                            graph.update_flow(&flow.parent.unwrap());
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
*/
