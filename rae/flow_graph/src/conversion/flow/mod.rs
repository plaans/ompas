use crate::conversion::flow::apply::convert_apply;
use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::Boolean;
use crate::structs::flow_graph::flow::{BranchingFlow, FlowId};
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::lit::{lvalue_to_lit, Lit};
use crate::structs::sym_table::{closure, VarId};
use crate::{DefineTable, FlowGraph};
use core::result::Result;
use core::result::Result::{Err, Ok};
use sompas_language::kind::ERR;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitives::LPrimitives;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub mod apply;
pub mod post_processing;
pub mod pre_processing;

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
        LValue::Nil => convert_nil(fl),
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
    match define_table.get(symbol.as_str()) {
        None => {
            let id = fl.sym_table.new_symbol(symbol);
            fl.new_instantaneous_assignment(Lit::Atom(id)).into()
        }
        Some(r) => fl.new_instantaneous_assignment(Lit::Atom(*r)).into(),
    }
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> FlowId {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_number(number);
    fl.new_instantaneous_assignment(Lit::Atom(id))
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_bool(bool);
    fl.new_instantaneous_assignment(Lit::Atom(id))
}

fn convert_nil(fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_nil();
    fl.new_instantaneous_assignment(Lit::Atom(id))
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
    let mut out_of_scope: Vec<VarId> = vec![];

    let r = match proc {
        LValue::Symbol(s) => convert_apply(s.as_str(), list.as_slice(), fl, define_table)?,
        LValue::Primitive(co) => match co {
            LPrimitives::Define => {
                let var = &list[1].to_string();
                let val = &list[2];
                let flow_val = convert_into_flow_graph(val, fl, define_table)?;
                define_table.insert(var.to_string(), fl.get_flow_result(&flow_val));
                let id = fl.sym_table.new_nil();
                let flow_result = fl.new_instantaneous_assignment(Lit::Atom(id));

                fl.new_seq(vec![flow_val, flow_result])
            }
            LPrimitives::If => {
                let define_table = &mut define_table.clone();

                /*
                Different parts of an 'if' expression
                 */
                let cond = &list[1];
                let true_branch = &list[2];
                let false_branch = &list[3];
                /*
                Conversion of each parts
                 */

                let cond_flow = convert_into_flow_graph(cond, fl, define_table)?;
                let cond_result = fl.get_flow_result(&cond_flow);
                fl.sym_table.meet_to_domain(&cond_result, Boolean);

                let true_flow = convert_into_flow_graph(true_branch, fl, define_table)?;
                let true_flow = fl.new_seq(vec![true_flow]);
                let false_flow = convert_into_flow_graph(false_branch, fl, define_table)?;
                let false_flow = fl.new_seq(vec![false_flow]);
                let true_result = fl.get_flow_result(&true_flow);
                let false_result = fl.get_flow_result(&false_flow);

                let result = fl.sym_table.new_result();

                fl.sym_table.add_update(
                    vec![true_result, false_result],
                    Update::new(
                        result,
                        closure::union_update(result, vec![true_result, false_result]),
                    ),
                );
                fl.sym_table.add_update(
                    vec![result],
                    Update::new(true_result, closure::in_union_update(true_result, result)),
                );
                fl.sym_table.add_update(
                    vec![result],
                    Update::new(false_result, closure::in_union_update(false_result, result)),
                );

                fl.sym_table.add_update(
                    vec![true_result],
                    Update::new(
                        cond_result,
                        closure::result_branch_cond_update(cond_result, true_result, true),
                    ),
                );

                fl.sym_table.add_update(
                    vec![false_result],
                    Update::new(
                        cond_result,
                        closure::result_branch_cond_update(cond_result, false_result, false),
                    ),
                );

                fl.sym_table.add_update(
                    vec![cond_result],
                    Update::new(
                        result,
                        closure::cond_result_branching_update(
                            cond_result,
                            result,
                            true_result,
                            false_result,
                        ),
                    ),
                );

                let result = fl.new_result(result, None);

                let cond = fl.new_result(cond_result, Some(fl.get_flow_end(&cond_flow)));

                let branching = BranchingFlow {
                    cond,
                    true_flow,
                    false_flow,
                    result,
                };
                let flow_branch = fl.new_branching(branching);
                fl.new_seq(vec![cond_flow, flow_branch])
            }
            LPrimitives::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                fl.new_instantaneous_assignment(lit)
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

                fl.new_seq(seq)
            }
            LPrimitives::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let async_flow = convert_into_flow_graph(e, fl, define_table)?;
                let r_async = fl.get_flow_result(&async_flow);

                let handle_flow = fl.new_async(async_flow);

                let handle = fl.get_flow_result(&handle_flow);

                fl.handles.insert(handle, async_flow);

                fl.sym_table.add_update(
                    vec![r_async],
                    Update::new(handle, closure::composed_update(handle, r_async)),
                );

                fl.sym_table.add_update(
                    vec![handle],
                    Update::new(r_async, closure::in_composed_update(r_async, handle)),
                );

                handle_flow
            }
            LPrimitives::Await => {
                let define_table = &mut define_table.clone();
                let h = convert_into_flow_graph(&list[1], fl, define_table)?;
                let result = fl.get_flow_result(&h);

                fl.sym_table.meet_to_domain(&result, RootType::Handle);

                let flow_await = fl.new_assignment(Lit::Await(result));
                out_of_scope.push(fl.get_flow_result(&h));

                fl.new_seq(vec![h, flow_await])
            }
            LPrimitives::Err => {
                let define_table = &mut define_table.clone();
                let arg_err = convert_into_flow_graph(&list[1], fl, define_table)?;
                let arg_err_result = fl.get_flow_result(&arg_err);
                let atom_err = fl.sym_table.new_symbol(ERR);
                let flow = fl.new_instantaneous_assignment(vec![atom_err, arg_err_result]);
                let err_result = fl.get_flow_result(&flow);

                fl.sym_table.set_domain(&err_result, RootType::Err);

                fl.sym_table.add_update(
                    vec![arg_err_result],
                    Update::new(
                        err_result,
                        closure::composed_update(err_result, arg_err_result),
                    ),
                );
                fl.sym_table.add_update(
                    vec![err_result],
                    Update::new(
                        arg_err_result,
                        closure::in_composed_update(arg_err_result, err_result),
                    ),
                );

                fl.new_seq(vec![arg_err, flow])
            }
            LPrimitives::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    };

    out_of_scope.append(&mut define_table.inner().values().copied().collect());

    for o in &out_of_scope {
        fl.sym_table.set_drop(o, &fl.get_flow_end(&r));
    }
    Ok(r)
}
