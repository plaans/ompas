use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::lvalue_to_lit;
use crate::structs::flow_graph::graph::{Vertice, VerticeId};
use crate::{DefineTable, Expression, FlowGraph};
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result;
use core::result::Result::{Err, Ok};
use ompas_rae_language::{RAE_EXEC_COMMAND, RAE_READ_STATE};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub fn convert_into_flow_graph(
    lv: &LValue,
    fl: &mut FlowGraph,
    parent: Option<VerticeId>,
    define_table: &mut DefineTable,
) -> Result<VerticeId, LRuntimeError> {
    let node_id = match lv {
        LValue::Symbol(s) => convert_symbol(s, fl, parent, define_table),
        LValue::String(s) => convert_string(s, fl, parent),
        LValue::Number(n) => convert_number(n, fl, parent),
        LValue::CoreOperator(co) => convert_core_operator(co, fl, parent),
        LValue::List(l) => convert_list(l, fl, parent, define_table)?,
        LValue::True => convert_bool(true, fl, parent),
        LValue::Nil => convert_bool(false, fl, parent),
        lv => {
            return Err(LRuntimeError::new(
                "convert",
                format!("{} can not be converted", lv.get_kind()),
            ))
        }
    };

    Ok(node_id)
}

fn convert_symbol(
    symbol: &Arc<String>,
    fl: &mut FlowGraph,
    parent: Option<VerticeId>,
    define_table: &DefineTable,
) -> VerticeId {
    match define_table.get(symbol.as_str()) {
        None => {
            let id = fl.sym_table.new_symbol(symbol, None);
            fl.new_vertice(Expression::cst(id.into()), parent)
        }
        Some(r) => fl.new_vertice(Expression::cst(r.into()), parent),
    }
}

fn convert_string(
    string: &Arc<String>,
    fl: &mut FlowGraph,
    parent: Option<VerticeId>,
) -> VerticeId {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph, parent: Option<VerticeId>) -> VerticeId {
    let id = fl.sym_table.new_number(number);
    fl.new_vertice(Expression::cst(id.into()), parent)
    //fl.new_vertice(CstValue::number(*number), parent)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph, parent: Option<VerticeId>) -> VerticeId {
    let id = fl.sym_table.new_bool(bool);
    fl.new_vertice(Expression::cst(id.into()), parent)
}

fn convert_core_operator(
    co: &LCoreOperator,
    fl: &mut FlowGraph,
    parent: Option<VerticeId>,
) -> VerticeId {
    todo!()
}

fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    mut parent: Option<VerticeId>,
    define_table: &mut DefineTable,
) -> Result<VerticeId, LRuntimeError> {
    let proc = &list[0];

    match proc {
        LValue::Symbol(s) => convert_apply(s.to_string(), &list[1..], fl, parent, define_table),
        LValue::CoreOperator(co) => match co {
            LCoreOperator::Define => {
                let var = &list[1];
                let val = &list[2];
                let id = convert_into_flow_graph(val, fl, parent, define_table)?;
                let parent = Some(id);
                define_table.insert(var.to_string(), *fl.get_result(&id));
                let id = fl.sym_table.new_bool(false);
                Ok(fl.new_vertice(Expression::cst(id.into()), parent))
            }
            LCoreOperator::If => {
                let mut define_table = &mut define_table.clone();

                let cond = &list[1];
                let true_branch = &list[2];
                let false_branch = &list[3];
                let cond = convert_into_flow_graph(cond, fl, parent, define_table)?;
                parent = Some(cond);
                let true_branch = convert_into_flow_graph(true_branch, fl, parent, define_table)?;
                let false_branch = convert_into_flow_graph(false_branch, fl, parent, define_table)?;
                let t = fl.sym_table.new_timepoint();
                let r = fl.sym_table.new_result();

                let interval = Interval::new_instantaneous(&t);

                let vertice = Vertice {
                    id: 0,
                    interval,
                    result: r,
                    computation: Expression::Cst(fl.get_result(&true_branch).into()),
                };

                let id_true = fl.push(vertice);

                let vertice = Vertice {
                    id: 0,
                    interval,
                    result: r,
                    computation: Expression::Cst(fl.get_result(&false_branch).into()),
                };

                let id_false = fl.push(vertice);

                //fl.set_child_link_lind(&parent.unwrap(), LinkKind::Branching);
                //let result_id = p1.get_relative();
                /*let p2 = fl.duplicate_result_node(
                    result_id,
                    CstValue::result(false_branch),
                    Some(false_branch),
                );*/

                let id = fl.new_vertice(Expression::cst(r.into()), None);

                fl.add_parent(&id, &id_true);
                fl.add_parent(&id, &id_false);
                //fl.set_parent_link_lind(&id, EdgeKind::Branching);

                Ok(id)
            }
            LCoreOperator::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                Ok(fl.new_vertice(Expression::cst(lit), parent))
            }
            LCoreOperator::Begin => {
                let mut define_table = define_table.clone();
                for e in &list[1..] {
                    let node_id = convert_into_flow_graph(e, fl, parent, &mut define_table)?;
                    parent = Some(node_id);
                }
                Ok(parent.unwrap())
            }
            /*LCoreOperator::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let r_async = convert(e, fl, parent.clone(), define_table)?;
                Ok(fl.new_vertice(Expression::handle(r_async), parent))
            }
            LCoreOperator::Await => {
                let define_table = &mut define_table.clone();
                let h = convert(&list[1], fl, parent.clone(), define_table)?;
                let h_parent = fl.backtrack_result(&h);
                let node: &Vertice = fl.get(&h_parent).unwrap();
                if let Expression::Handle(n_async) = node.get_computation().clone() {
                    let r = fl.new_vertice(CstValue::result(n_async), Some(h));
                    fl.add_parent(&r, &n_async);
                    Ok(r)
                } else {
                    Ok(h)
                }
            }*/
            LCoreOperator::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    }
}

fn convert_apply(
    proc: String,
    args: &[LValue],
    fl: &mut FlowGraph,
    mut parent: Option<VerticeId>,
    define_table: &mut DefineTable,
) -> Result<VerticeId, LRuntimeError> {
    let mut define_table = define_table.clone();
    let mut args_result = vec![];

    for arg in args {
        let node_id = convert_into_flow_graph(arg, fl, parent, &mut define_table)?;
        args_result.push(*fl.get_result(&node_id));
        parent = Some(node_id);
    }

    Ok(match proc.as_str() {
        RAE_EXEC_COMMAND => {
            let start = fl.sym_table.new_timepoint();
            let end = fl.sym_table.new_timepoint();
            let r = fl.sym_table.new_result();

            let vertice = Vertice {
                id: 0,
                interval: Interval::new(&start, &end),
                result: r,
                computation: Expression::exec(args_result),
            };
            let id = fl.push(vertice);
            fl.add_parent(&id, &parent.unwrap());
            id
        }
        RAE_READ_STATE => fl.new_vertice(Expression::read(args_result), parent),
        _ => fl.new_vertice(Expression::apply(args_result), parent),
    })
}
