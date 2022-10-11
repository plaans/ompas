use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::subtask::SubTask;
use crate::{Expression, FlowGraph};

pub fn convert_into_chronicle(graph: FlowGraph) -> ChronicleTemplate {
    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    for vertice in graph.vertices() {
        match vertice.get_computation() {
            Expression::Apply(_) => {}
            Expression::Write(_) => {}
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
            Expression::Start => ch.add_constraint(Constraint::eq(
                ch.get_interval().start(),
                vertice.interval.end(),
            )),
            Expression::End(id) => {
                ch.add_constraint(Constraint::eq(ch.get_result(), id));

                ch.add_constraint(Constraint::eq(
                    ch.get_interval().end(),
                    vertice.interval.start(),
                ));
            }
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
        }
    }

    for edge in graph.edges() {
        let from_end = graph.get_interval(edge.from()).end();
        let to_start = graph.get_interval(edge.to()).start();
        ch.add_var(from_end);
        ch.add_var(to_start);

        ch.add_constraint(Constraint::eq(from_end, to_start))
    }

    ch.debug.flow_graph = graph;

    ch
}
