use crate::conversion::flow::post_processing::PostProcess::{Bind, Invalid, Meet, Subtract};
use ompas_structs::conversion::flow_graph::flow::{FlowId, FlowKind};
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::sym_table::domain::basic_type::BasicType;
use ompas_structs::sym_table::domain::Domain;
use ompas_structs::sym_table::lit::Lit;
use ompas_structs::sym_table::{EmptyDomains, VarId};
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::VecDeque;

const FLOW_GRAPH_POST_PROCESS: &str = "flow_graph_post_process";

pub fn flow_graph_post_processing(graph: &mut FlowGraph) -> Result<(), LRuntimeError> {
    let result_graph = graph.get_flow_result(&graph.flow);

    let mut post_process = binding_constraints(graph);
    post_process.push_front(Subtract(result_graph, BasicType::Err.into()));
    propagate(graph, post_process).map_err(|e| e.chain(FLOW_GRAPH_POST_PROCESS))?;
    graph.flat_bindings();
    Ok(())
}

pub enum PostProcess {
    Subtract(VarId, Domain),
    Meet(VarId, Domain),
    Bind(VarId, VarId),
    Invalid(FlowId),
}

pub fn propagate(
    graph: &mut FlowGraph,
    mut queue: VecDeque<PostProcess>,
) -> Result<(), LRuntimeError> {
    while let Some(post_process) = queue.pop_front() {
        match post_process {
            Subtract(id, d) => {
                let id = graph.st.get_var_parent(&id);
                let domain_id = graph.st.get_domain_id(&id);
                //println!("Subtract({id}, {})", graph.sym_table.format_domain(&d));
                let emptys = graph.st.substract_to_domain(&domain_id, d);
                if let EmptyDomains::Some(emptys) = emptys {
                    //println!("[Subtract] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Meet(id, d) => {
                let id = graph.st.get_var_parent(&id);
                let domain_id = graph.st.get_domain_id(&id);
                //println!("Meet({id}, {})", graph.sym_table.format_domain(&d));

                let emptys = graph.st.meet_to_domain(&domain_id, d);
                if let EmptyDomains::Some(emptys) = emptys {
                    //println!("[Meet] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Bind(id1, id2) => {
                let id1 = graph.st.get_var_parent(&id1);
                let id2 = graph.st.get_var_parent(&id2);
                //println!("Bind({id1}, {id2})");
                if let EmptyDomains::Some(emptys) = graph.st.union_var(&id1, &id2) {
                    //println!("[Bind] Domains of {:?} are empty.", emptys);
                    for e in &emptys {
                        for f in graph.map_atom_id_flow_id.get(e).unwrap() {
                            queue.push_back(Invalid(*f));
                        }
                    }
                }
            }
            Invalid(ref id) => {
                //println!("Invalid({id})");
                if graph.is_valid(id) {
                    if let FlowKind::Branching(br) = graph.get_kind(id) {
                        if !graph.is_valid(&br.false_flow) && !graph.is_valid(&br.true_flow) {
                            graph.invalidate(id);
                        }
                    } else {
                        graph.invalidate(id);
                    }
                    if !graph.is_valid(id) {
                        if let Some(parent) = graph.get_parent(id) {
                            queue.push_back(Invalid(*parent))
                        }
                    }
                }
            }
        }
        //println!("{}", graph.sym_table);
    }
    Ok(())
}

pub fn binding_constraints(fl: &mut FlowGraph) -> VecDeque<PostProcess> {
    let mut flows_queue: VecDeque<FlowId> = Default::default();
    flows_queue.push_back(fl.flow);
    for handle in fl.handles.values() {
        flows_queue.push_back(*handle)
    }

    let mut post_process = VecDeque::new();

    while let Some(flow_id) = flows_queue.pop_front() {
        let flow = fl.flows[flow_id].clone();
        match &flow.kind {
            FlowKind::Lit(lit) => {
                if let Lit::Atom(a) = &lit {
                    post_process.push_back(Bind(flow.result, *a));
                    let parent_flow = &mut fl.flows[flow.parent.unwrap()];
                    if let FlowKind::Seq(s) = &mut parent_flow.kind {
                        s.retain(|f| *f != flow_id)
                    }
                }
            }
            FlowKind::Seq(seq) => {
                let mut previous_end: Option<VarId> = None;

                for f in seq {
                    if let Some(prev) = previous_end {
                        post_process.push_back(Bind(prev, fl.get_flow_start(f)))
                    }

                    previous_end = Some(fl.get_flow_end(&f))
                }

                for f in seq {
                    flows_queue.push_back(*f)
                }
            }
            FlowKind::Branching(b) => {
                flows_queue.push_back(b.cond_flow);
                flows_queue.push_back(b.true_flow);
                flows_queue.push_back(b.false_flow);
            }
            FlowKind::FlowHandle(f) => flows_queue.push_back(*f),
            FlowKind::FlowResourceHandle(f) => flows_queue.push_back(*f),
            _ => {}
        }
    }
    post_process
}
