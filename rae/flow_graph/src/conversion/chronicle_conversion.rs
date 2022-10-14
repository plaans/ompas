use crate::conversion::chronicle_post_processing::{post_processing, unify_equal};
use crate::structs::chronicle::atom::Atom;
use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::AtomId;
use crate::structs::flow_graph::expression::{Block, Expression};
use crate::structs::flow_graph::scope::Scope;
use crate::FlowGraph;
use sompas_structs::lruntimeerror::LRuntimeError;

pub struct Await {
    result: AtomId,
    handle: AtomId,
}

pub fn convert_into_chronicle(
    graph: &FlowGraph,
    scope: Scope,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut awaits: Vec<Await> = vec![];

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    let mut queue = vec![scope.start];

    //Binds the start timepoint of the first expression with start timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().get_start(),
        graph.get(scope.start()).unwrap().interval.get_start(),
    ));

    //Binds the result of the last expression, with the result of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_result(),
        graph.get_result(scope.get_end()),
    ));

    //Binds the end timepoint of the last expression with end timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().get_end(),
        graph.get(scope.get_end()).unwrap().interval.get_end(),
    ));

    while let Some(id) = queue.pop() {
        let vertice = graph.get(&id).unwrap();
        match vertice.get_computation() {
            Expression::Apply(a) => ch.add_constraint(Constraint::eq(vertice.result, a)),
            Expression::Write(vec) => {
                let sv = vec[0..vec.len() - 1].to_vec();
                let value = *vec.last().unwrap();
                let effect = Effect {
                    interval: vertice.interval,
                    sv,
                    value,
                };

                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.result, result));

                ch.add_effect(effect);
            }
            Expression::Read(vec) => {
                let condition = Condition {
                    interval: vertice.interval,
                    sv: vec.clone(),
                    value: vertice.result,
                };

                ch.add_condition(condition);
            }
            Expression::Cst(lit) => {
                ch.add_constraint(Constraint::Eq(vertice.result.into(), lit.clone()))
            }
            //Expression::Await(_) => {}
            Expression::Exec(vec) => {
                let subtask = SubTask {
                    interval: vertice.interval, //TODO: change
                    lit: vec.into(),
                };

                ch.add_subtask(subtask);
                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.get_result(), result));
                ch.add_constraint(Constraint::leq(
                    *vertice.interval.get_start(),
                    *vertice.interval.get_end(),
                ))
            }
            Expression::Err(_) => {}
            Expression::Block(block) => match block {
                Block::If(if_block) => {
                    let t_if = ch.sym_table.new_if();
                    let method_true = convert_into_chronicle(graph, if_block.true_branch)?;
                    let method_false = convert_into_chronicle(graph, if_block.false_branch)?;
                    let task = TaskTemplate {
                        name: vec![t_if],
                        methods: vec![method_true, method_false],
                    };

                    ch.add_task_template(task);

                    ch.add_subtask(SubTask {
                        interval: vertice.interval,
                        lit: vec![t_if].into(),
                    })
                } /*Block::Handle(handle) => {
                      //Add timepoints constraints on the temporal scope of the handle;
                      let interval_handle = graph.get_scope_interval(&handle.scope_handle);
                      let interval_expression = graph.get_scope_interval(&handle.scope_expression);
                      ch.add_constraint(Constraint::eq(
                          interval_handle.start(),
                          interval_expression.start(),
                      ));
                      ch.add_constraint(Constraint::leq(
                          interval_expression.end(),
                          interval_handle.end(),
                      ));
                      queue.push(handle.scope_expression.start)
                  }*/
            },
            Expression::Handle(h) => {
                let handle = graph.handles.get(h).unwrap();
                queue.push(handle.scope.start);
                ch.add_constraint(Constraint::eq(
                    vertice.interval.get_start(),
                    graph.get_scope_interval(&handle.scope).get_start(),
                ));
                ch.add_constraint(Constraint::eq(vertice.result, h));
            }
            Expression::Await(a) => {
                awaits.push(Await {
                    result: vertice.result,
                    handle: *a,
                });
                //ch.add_constraint(Constraint::eq(vertice.result, Constraint::Await(a.into())))
            }
        }
        if let Some(parent) = vertice.parent {
            ch.add_constraint(Constraint::eq(
                graph.get_interval(&parent).get_end(),
                vertice.interval.get_start(),
            ))
        }
        if let Some(child) = vertice.child {
            queue.push(child)
        }
    }

    /*
    HANDLES
     */

    unify_equal(&mut ch);

    for a in &awaits {
        let handle = ch.sym_table.get_parent(&a.handle);
        if ch.sym_table.get_type_of(&handle) == AtomType::Handle {
            let handle = graph.handles.get(&handle).unwrap();
            ch.add_constraint(Constraint::eq(a.result, handle.result));
            let start = ch.sym_table.get_start(&a.result).unwrap();
            let end_async = graph.get(handle.scope.get_end()).unwrap().get_end();
            ch.add_constraint(Constraint::leq(end_async, start));
        } else {
            Err(LRuntimeError::new(
                "convert_into_chronicle",
                "await arg is not a handle",
            ))?
        }
    }

    ch.debug.flow_graph = graph.clone();

    //post_processing(&mut ch)?;

    Ok(ch)
}
