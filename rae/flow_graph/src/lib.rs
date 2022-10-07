use crate::define_table::DefineTable;
use crate::graph::{Computation, CstValue, FlowGraph, NodeId};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub mod config;
pub mod define_table;
pub mod graph;

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
            let mut args = vec![];
            for e in list.as_slice() {
                let node_id = convert(e, fl, parent, define_table)?;
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
                todo!()
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
                todo!()
            }
            LCoreOperator::Await => {
                todo!()
            }
            LCoreOperator::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    }
}
