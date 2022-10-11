use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::constraint::Constraint;
use crate::{Expression, FlowGraph};
use sompas_structs::lnumber::LNumber;

pub fn convert_into_chronicle(graph: FlowGraph) -> ChronicleTemplate {
    let mut chronicle_template = ChronicleTemplate::new("template", ChronicleKind::Method);

    chronicle_template.debug.flow_graph = graph.clone();

    for node in graph.inner() {
        match node.get_computation() {
            Expression::Apply(_) => {}
            Expression::Write(_) => {}
            Expression::Read(_) => {}
            Expression::Cst(cst) => {
                /*let val = match cst {
                    CstValue::Result(_) => todo!(),
                    CstValue::Number(LNumber::Float(f)) => {
                        chronicle_template.sym_table.new(*f.into())
                    }
                    CstValue::Number(LNumber::Int(i)) => {
                        chronicle_template.sym_table.new_int(*i.into())
                    }
                    CstValue::Bool(b) => chronicle_template.sym_table.new_bool(*b),
                    CstValue::Symbol(s) => {}
                    CstValue::String(s) => {}
                    CstValue::Expression(e) => {}
                };
                //let r = chronicle_template.sym_table.new_result()

                chronicle_template
                    .chronicle_template
                    .add_constraint(Constraint::eq(r, val))*/
            }
            Expression::Handle(_) => {}
            Expression::Start => {}
            Expression::End => {}
        }
    }

    chronicle_template
}
