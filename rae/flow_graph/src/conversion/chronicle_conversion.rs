use crate::conversion::chronicle_post_processing::post_processing;
use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::flow_graph::graph::{Block, Scope};
use crate::{Expression, FlowGraph};
use sompas_structs::lruntimeerror::LRuntimeError;

pub fn convert_into_chronicle(
    graph: &FlowGraph,
    scope: Scope,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    let mut queue = vec![scope.start];

    //Binds the start timepoint of the first expression with start timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().start(),
        graph.get(scope.start()).unwrap().interval.start(),
    ));

    //Binds the result of the last expression, with the result of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_result(),
        graph.get_result(scope.end()),
    ));

    //Binds the end timepoint of the last expression with end timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().end(),
        graph.get(scope.end()).unwrap().interval.end(),
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
            Expression::Await(_) => {}

            Expression::Exec(vec) => {
                let subtask = SubTask {
                    interval: vertice.interval, //TODO: change
                    lit: vec.into(),
                };

                ch.add_subtask(subtask);
                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.get_result(), result));
                ch.add_constraint(Constraint::leq(
                    *vertice.interval.start(),
                    *vertice.interval.end(),
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
                }
                Block::Async(a) => {
                    let t_start = graph.get_interval(a.scope.start()).start();
                    let t_end = graph.get_interval(a.scope.end()).end();
                    ch.add_constraint(Constraint::eq(vertice.get_start(), t_start));
                    ch.add_constraint(Constraint::leq(t_end, ch.get_interval().end()));
                    queue.push(a.scope.start)
                }
            },
        }
        if let Some(parent) = vertice.parent {
            ch.add_constraint(Constraint::eq(
                graph.get_interval(&parent).end(),
                vertice.interval.start(),
            ))
        }
        if let Some(child) = vertice.child {
            queue.push(child)
        }
    }

    ch.debug.flow_graph = graph.clone();

    post_processing(&mut ch)?;

    Ok(ch)
}
