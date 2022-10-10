use crate::{Computation, CstValue, DefineTable, FlowGraph, LinkKind, Node, NodeId};
use core::option::Option;
use core::option::Option::{None, Some};
use core::result::Result;
use core::result::Result::{Err, Ok};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub type ConvertError = String;

pub fn convert(
    lv: &LValue,
    fl: &mut FlowGraph,
    parent: Option<NodeId>,
    define_table: &mut DefineTable,
) -> Result<NodeId, ConvertError> {
    let node_id = match lv {
        LValue::Symbol(s) => convert_symbol(s, fl, parent, define_table),
        LValue::String(s) => convert_string(s, fl, parent),
        LValue::Number(n) => convert_number(n, fl, parent),
        LValue::CoreOperator(co) => convert_core_operator(co, fl, parent),
        LValue::List(l) => convert_list(l, fl, parent, define_table)?,
        LValue::True => convert_bool(true, fl, parent),
        LValue::Nil => convert_bool(false, fl, parent),
        lv => return Err(format!("{} can not be converted", lv.get_kind())),
    };

    Ok(node_id)
}

fn convert_symbol(
    symbol: &Arc<String>,
    fl: &mut FlowGraph,
    parent: Option<NodeId>,
    define_table: &DefineTable,
) -> NodeId {
    match define_table.get(symbol.as_str()) {
        None => fl.new_node(CstValue::symbol(symbol.to_string()), parent),
        Some(r) => fl.new_node(CstValue::result(*r), parent),
    }
}

fn convert_string(string: &Arc<String>, fl: &mut FlowGraph, parent: Option<NodeId>) -> NodeId {
    fl.new_node(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph, parent: Option<NodeId>) -> NodeId {
    fl.new_node(CstValue::number(*number), parent)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph, parent: Option<NodeId>) -> NodeId {
    fl.new_node(CstValue::bool(bool), parent)
}

fn convert_core_operator(co: &LCoreOperator, fl: &mut FlowGraph, parent: Option<NodeId>) -> NodeId {
    todo!()
}

fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    mut parent: Option<NodeId>,
    define_table: &mut DefineTable,
) -> Result<NodeId, ConvertError> {
    let proc = &list[0];

    match proc {
        LValue::Symbol(_) => {
            let mut define_table = define_table.clone();
            let mut args = vec![];
            for e in list.as_slice() {
                let node_id = convert(e, fl, parent, &mut define_table)?;
                args.push(node_id);
                parent = Some(node_id);
            }
            Ok(fl.new_node(Computation::apply(args), parent))
        }
        LValue::CoreOperator(co) => match co {
            LCoreOperator::Define => {
                let var = &list[1];
                let val = &list[2];
                let parent = Some(convert(val, fl, parent, define_table)?);
                define_table.insert(var.to_string(), parent.unwrap());
                Ok(fl.new_node(CstValue::bool(false), parent))
            }
            LCoreOperator::If => {
                let mut define_table = &mut define_table.clone();

                let cond = &list[1];
                let true_branch = &list[2];
                let false_branch = &list[3];
                let cond = convert(cond, fl, parent, define_table)?;

                parent = Some(cond);

                let true_branch = convert(true_branch, fl, parent, define_table)?;
                let p1 = fl.new_node(CstValue::result(true_branch), Some(true_branch));

                let false_branch = convert(false_branch, fl, parent, define_table)?;
                fl.set_child_link_lind(&parent.unwrap(), LinkKind::Branching);
                let result_id = p1.get_relative();
                let p2 = fl.duplicate_result_node(
                    result_id,
                    CstValue::result(false_branch),
                    Some(false_branch),
                );

                let id = fl.new_node(CstValue::result(p1), None);

                fl.add_parent(&id, &p1);
                fl.add_parent(&id, &p2);
                fl.set_parent_link_lind(&id, LinkKind::Branching);

                Ok(id)
            }
            LCoreOperator::Quote => Ok(fl.new_node(CstValue::Expression(list[1].clone()), parent)),
            LCoreOperator::Begin => {
                let mut define_table = define_table.clone();
                for e in &list[1..] {
                    let node_id = convert(e, fl, parent, &mut define_table)?;
                    parent = Some(node_id);
                }
                Ok(parent.unwrap())
            }
            LCoreOperator::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let r_async = convert(e, fl, parent.clone(), define_table)?;
                Ok(fl.new_node(Computation::handle(r_async), parent))
            }
            LCoreOperator::Await => {
                let define_table = &mut define_table.clone();
                let h = convert(&list[1], fl, parent.clone(), define_table)?;
                let h_parent = fl.backtrack_result(&h);
                let node: &Node = fl.get(&h_parent).unwrap();
                if let Computation::Handle(n_async) = node.get_computation().clone() {
                    let r = fl.new_node(CstValue::result(n_async), Some(h));
                    fl.add_parent(&r, &n_async);
                    Ok(r)
                } else {
                    Ok(h)
                }
            }
            LCoreOperator::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    }
}
