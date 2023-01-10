use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::template::{ChronicleKind, ChronicleTemplate};
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::lit::Lit;
use itertools::Itertools;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::ops::Deref;

pub mod post_processing;

pub fn convert_method(
    fl: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    convert_into_chronicle(fl, flow)
    //let partial = convert_into_chronicle(graph, flow)?;

    //post_processing(partial)
}

pub fn convert_into_chronicle(
    fl: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut st = fl.sym_table.clone();

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, fl.sym_table.clone());

    ch.add_constraint(Constraint::leq(
        ch.get_interval().get_start(),
        ch.get_interval().get_end(),
    ));

    //Bind the flow start timepoint with the chronicle start timepoint
    st.union_atom(
        ch.get_interval().get_start(),
        fl.get_flow_interval(flow).get_start(),
    );

    //Bind the flow end timepoint with the chronicle end timepoint
    st.union_atom(
        ch.get_interval().get_end(),
        fl.get_flow_interval(flow).get_end(),
    );

    st.union_atom(&ch.get_result(), &fl.get_flow_result(flow));

    let mut queue = vec![*flow];

    while let Some(flow_id) = queue.pop() {
        let flow = fl.flows[flow_id].clone();
        let interval = fl.get_flow_interval(&flow_id);
        let result = fl.get_flow_result(&flow_id);

        match flow.kind {
            FlowKind::Assignment(ass) => match &ass.lit {
                Lit::Exp(_) => {}
                Lit::Atom(_) => {}
                Lit::Await(a) => {
                    let handle = fl.get_handle(a).unwrap();

                    ch.add_constraint(Constraint::leq(
                        fl.get_flow_end(&handle),
                        interval.get_end(),
                    ));
                    //ch.add_constraint(Constraint::eq(fl.get_flow_result(handle), result));
                    st.union_atom(&fl.get_flow_result(&handle), &result);
                }
                Lit::Constraint(c) => ch.add_constraint(Constraint::eq(result, c.deref())),
                Lit::Apply(app) => {}
                Lit::Read(read) => {
                    let condition = Condition {
                        interval,
                        sv: read.clone(),
                        value: result,
                    };

                    ch.add_condition(condition);
                }
                Lit::Write(write) => {
                    let sv = write[0..write.len() - 1].to_vec();
                    let value = *write.last().unwrap();
                    let effect = Effect {
                        interval,
                        sv,
                        value,
                    };

                    ch.add_effect(effect);

                    let result = ch.sym_table.new_nil();
                    st.union_atom(&result, &result);
                }
                Lit::Exec(exec) => {
                    let subtask = SubTask {
                        interval,
                        lit: exec.into(),
                        result,
                    };

                    ch.add_subtask(subtask);
                    /*let subtask_result = ch.sym_table.new_nil();
                    st.union_atom(&subtask_result, &result);*/
                }
            },
            FlowKind::Seq(mut seq, _) => queue.append(&mut seq),
            FlowKind::Branching(_) => {}
            FlowKind::FlowResult(_) => {
                unreachable!()
            }
            FlowKind::FlowAsync(fa) => {
                let h = fa.flow;
                let result = fa.result;
                queue.push(h);
                ch.add_constraint(Constraint::leq(fa.timepoint, fl.get_flow_start(&h)));
                //sym_table.union_atom(&ass.get_start(), &fl.get_flow_start(&h));
                let drops: Vec<Lit> = st
                    .get_drops(&result)
                    .drain(..)
                    .map(|a| st.get_parent(&a).into())
                    .unique()
                    .collect();
                ch.add_constraint(Constraint::leq(fl.get_flow_end(&h), Constraint::Max(drops)))
            }
        }
    }

    fl.flat_bindings();

    ch.flat_bindings();

    Ok(ch)
}
