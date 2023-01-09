use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::template::{ChronicleKind, ChronicleTemplate};
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::AtomId;
use sompas_structs::lruntimeerror::LRuntimeError;

pub mod post_processing;

pub fn convert_method(
    graph: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    convert_into_chronicle(graph, flow)
    //let partial = convert_into_chronicle(graph, flow)?;

    //post_processing(partial)
}

pub fn convert_into_chronicle(
    graph: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut sym_table = graph.sym_table.clone();

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    //Bind the flow start timepoint with the chronicle start timepoint
    sym_table.union_atom(
        ch.get_interval().get_start(),
        graph.get_flow_interval(flow).get_start(),
    );

    //Bind the flow end timepoint with the chronicle end timepoint
    sym_table.union_atom(
        ch.get_interval().get_end(),
        graph.get_flow_interval(flow).get_end(),
    );

    sym_table.union_atom(&ch.get_result(), &graph.get_flow_result(flow));

    let mut queue = vec![*flow];

    while let Some(flow) = queue.pop() {
        let flow = graph.flows[flow].clone();

        match flow.kind {
            FlowKind::Assignment(ass) => match &ass.lit {
                Lit::Exp(_) => {}
                Lit::Atom(_) => {}
                Lit::Await(_) => {}
                Lit::Constraint(_) => {}
                Lit::Apply(app) => {}
                Lit::Read(read) => {
                    let condition = Condition {
                        interval: ass.interval,
                        sv: read.clone(),
                        value: ass.result,
                    };

                    ch.add_condition(condition);
                }
                Lit::Write(write) => {
                    let sv = write[0..write.len() - 1].to_vec();
                    let value = *write.last().unwrap();
                    let effect = Effect {
                        interval: ass.interval,
                        sv,
                        value,
                    };

                    ch.add_effect(effect);

                    let result = ch.sym_table.new_nil();
                    sym_table.union_atom(&result, &ass.result);
                }
                Lit::Exec(exec) => {
                    let subtask = SubTask {
                        interval: ass.interval,
                        lit: exec.into(),
                        result: ass.result,
                    };

                    ch.add_subtask(subtask);
                    let result = ch.sym_table.new_nil();
                    sym_table.union_atom(&result, &ass.result);
                }
            },
            FlowKind::Seq(mut seq, _) => {
                let mut previous_end: Option<AtomId> = None;

                for f in &seq {
                    if let Some(prev) = previous_end {
                        sym_table.union_atom(&prev, graph.get_flow_interval(&f).get_start());
                    }

                    previous_end = Some(*graph.get_flow_interval(&f).get_end())
                }

                queue.append(&mut seq)
            }
            FlowKind::Branching(_) => {}
            FlowKind::FlowResult(_) => {
                unreachable!()
            }
        }
    }

    Ok(ch)
}
