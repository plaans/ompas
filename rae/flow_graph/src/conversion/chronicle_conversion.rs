use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::flow_graph::graph::{Block, Scope};
use crate::{Expression, FlowGraph};

pub fn convert_into_chronicle(graph: &FlowGraph, scope: Scope) -> ChronicleTemplate {
    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    let mut next = Some(scope.start);

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

    while let Some(id) = next {
        let vertice = graph.get(&id).unwrap();
        match vertice.get_computation() {
            Expression::Apply(_) => {}
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
            Expression::Handle(_) => {}

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
                    let method_true = convert_into_chronicle(graph, if_block.true_branch);
                    let method_false = convert_into_chronicle(graph, if_block.false_branch);
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
            },
        }
        if let Some(parent) = vertice.parent {
            ch.add_constraint(Constraint::eq(
                graph.get_interval(&parent).end(),
                vertice.interval.start(),
            ))
        }
        next = vertice.child.clone();
    }

    ch.debug.flow_graph = graph.clone();

    ch
}
