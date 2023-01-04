use crate::structs::chronicle::lit::lvalue_to_lit;
use crate::structs::flow_graph::expression::Expression;
use crate::structs::flow_graph::flow::{Flow, FlowId};
use crate::structs::flow_graph::handle_table::Handle;
use crate::structs::flow_graph::scope::Scope;
use crate::structs::sym_table::AtomId;
use crate::{DefineTable, FlowGraph};
use core::result::Result;
use core::result::Result::{Err, Ok};
use ompas_rae_language::exec::platform::*;
use ompas_rae_language::exec::refinement::EXEC_TASK;
use ompas_rae_language::exec::state::READ_STATE;
use ompas_rae_language::exec::state::{ASSERT, ASSERT_SHORT};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitives::LPrimitives;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::borrow::Borrow;
use std::sync::Arc;

pub fn convert_into_flow_graph(
    lv: &LValue,
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<FlowId, LRuntimeError> {
    let node_id = match lv {
        LValue::Symbol(s) => convert_symbol(s, fl, define_table),
        LValue::String(s) => convert_string(s, fl),
        LValue::Number(n) => convert_number(n, fl),
        LValue::Primitive(co) => convert_core_operator(co, fl),
        LValue::List(l) => convert_list(l, fl, define_table)?,
        LValue::True => convert_bool(true, fl),
        LValue::Nil => convert_bool(false, fl),
        lv => {
            return Err(LRuntimeError::new(
                "convert",
                format!("{} can not be converted", lv.get_kind()),
            ))
        }
    };

    Ok(node_id)
}

fn convert_symbol(symbol: &Arc<String>, fl: &mut FlowGraph, define_table: &DefineTable) -> FlowId {
    let vertice_id = match define_table.get(symbol.as_str()) {
        None => {
            let id = fl.sym_table.new_symbol(symbol, None);
            fl.new_instantaneous_vertice(Expression::expr(id)).into()
        }
        Some(r) => fl.new_instantaneous_vertice(Expression::expr(*r)).into(),
    };

    fl.new_vertice_flow(vertice_id)
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> FlowId {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_number(number);
    let vertice_id = fl.new_instantaneous_vertice(Expression::expr(id));
    fl.new_vertice_flow(vertice_id)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_bool(bool);
    let vertice_id = fl.new_instantaneous_vertice(Expression::expr(id));
    fl.new_vertice_flow(vertice_id)
}

fn convert_core_operator(_: &LPrimitives, _: &mut FlowGraph) -> FlowId {
    todo!()
}

fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<FlowId, LRuntimeError> {
    let proc = &list[0];
    let mut out_of_scope: Vec<AtomId> = vec![];

    let r = match proc {
        LValue::Symbol(_) => convert_apply(list.as_slice(), fl, define_table),
        LValue::Primitive(co) => match co {
            LPrimitives::Define => {
                let var = &list[1].to_string();
                let val = &list[2];
                let mut flow_val = convert_into_flow_graph(val, fl, define_table)?;
                let id_var = fl.sym_table.new_symbol(var, None);
                let vertice_var = fl.new_instantaneous_vertice(Expression::expr(id_var));
                let flow_var = fl.new_vertice_flow(vertice_var);
                define_table.insert(var.to_string(), fl.get_vertice_result(&vertice_var));

                let id = fl.sym_table.new_bool(false);
                let result = fl.new_instantaneous_vertice(Expression::expr(id));
                let flow_result = fl.new_vertice_flow(result);

                Ok(fl.new_flow(Flow::Seq(vec![flow_val, flow_var, flow_result])))
            }
            LPrimitives::If => {
                /*let define_table = &mut define_table.clone();

                /*
                Different parts of an 'if' expression
                 */
                let cond = &list[1];
                let true_branch = &list[2];
                let false_branch = &list[3];

                /*
                Conversion of each parts
                 */
                let cond = convert_into_flow_graph(cond, fl, define_table)?;
                let true_branch = convert_into_flow_graph(true_branch, fl, define_table)?;
                let false_branch = convert_into_flow_graph(false_branch, fl, define_table)?;

                let if_expression = IfBlock {
                    cond: *fl.get_result(cond.get_end()),
                    true_result: *fl.get_result(true_branch.get_end()),
                    false_result: *fl.get_result(false_branch.get_end()),
                    true_branch,
                    false_branch,
                };

                let if_id =
                    fl.new_instantaneous_vertice(Expression::Block(Block::If(if_expression)));
                fl.set_parent(&if_id, cond.get_end());
                let mut scope = cond;
                scope.end = if_id;
                end_scope = *fl.get_scope_interval(&scope).get_end();
                Ok(scope)*/
                todo!()
            }
            LPrimitives::Quote => {
                todo!()
                /*let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                let vertice = fl.new_instantaneous_vertice(Expression::expr(lit));
                end_scope = *fl.get_interval(&vertice).get_end();
                Ok(vertice.into())*/
            }
            LPrimitives::Begin => {
                let mut define_table = define_table.clone();
                let mut results = vec![];
                let mut seq = vec![];
                for e in &list[1..] {
                    let e_flow = convert_into_flow_graph(e, fl, &mut define_table)?;
                    results.push(fl.get_flow_result(&e_flow));
                    seq.push(e_flow);
                }
                out_of_scope.append(&mut results);

                Ok(fl.new_flow(Flow::Seq(seq)))
            }
            /*LPrimitives::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let scope_expression = convert_into_flow_graph(e, fl, define_table)?;

                let handle_id = fl.sym_table.new_handle();

                let vertice = fl.new_instantaneous_vertice(Expression::Handle(handle_id));

                let handle = Handle {
                    result: *fl.get_scope_result(&scope_expression),
                    scope: scope_expression,
                    ends: vec![],
                };

                fl.handles.insert(&handle_id, handle);

                end_scope = *fl.get_interval(&vertice).get_end();

                Ok(vertice.into())
            }
            LPrimitives::Await => {
                let define_table = &mut define_table.clone();
                let mut h = convert_into_flow_graph(&list[1], fl, define_table)?;
                let a = fl.new_vertice(Expression::Await(*fl.get_scope_result(&h)));
                out_of_scope.push(*fl.get_scope_result(&h));
                end_scope = *fl.get_interval(&a).get_end();

                fl.set_parent(&a, h.get_end());
                h.end = a;
                Ok(h)
            }*/
            LPrimitives::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    };

    /*out_of_scope.append(&mut define_table.inner().values().copied().collect());

    for o in &out_of_scope {
        fl.sym_table.set_end(o, &end_scope);
    }*/
    r
}

fn convert_apply(
    expr: &[LValue],
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<FlowId, LRuntimeError> {
    let mut define_table = define_table.clone();

    let mut seq: Vec<FlowId> = vec![];

    for e in expr {
        seq.push(convert_into_flow_graph(e, fl, &mut define_table)?);
    }

    let mut results: Vec<AtomId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();
    let apply_id = fl.new_vertice(Expression::Apply(results.clone()));
    let end = *fl.get_vertice_interval(&apply_id).get_end();
    seq.push(fl.new_vertice_flow(apply_id));
    for o in results {
        fl.sym_table.set_end(&o, &end);
    }
    Ok(fl.new_flow(Flow::Seq(seq)))

    /*let scope = match proc_symbol.as_str() {
        EXEC_COMMAND | EXEC_TASK => {
            let exec_scope: Scope = fl
                .new_vertice(Expression::exec(args_result.to_vec()))
                .into();
            fl.set_parent(exec_scope.start(), arg_scope.get_end());
            arg_scope.end = exec_scope.end;
            arg_scope
        }
        ASSERT | ASSERT_SHORT => {
            let id = fl.new_instantaneous_vertice(Expression::write(args_result.to_vec()));
            fl.set_parent(&id, arg_scope.get_end());
            arg_scope.end = id;
            arg_scope
        }
        READ_STATE => {
            let id = fl.new_instantaneous_vertice(Expression::read(args_result.to_vec()));
            fl.set_parent(&id, arg_scope.get_end());
            arg_scope.end = id;
            arg_scope
        }
        _ => {
            fl.set_child(proc_scope.get_end(), arg_scope.start());

            results.push(*fl.get_result(proc_scope.get_end()));
            let mut vec = vec![*fl.get_result(proc_scope.get_end())];
            vec.append(&mut args_result);

            let id = fl.new_instantaneous_vertice(Expression::apply(vec));
            fl.set_parent(&id, arg_scope.get_end());
            out_of_scope.push(*fl.get_scope_result(&proc_scope));

            Scope {
                start: proc_scope.start,
                end: id,
            }
        }
    };*/
}
