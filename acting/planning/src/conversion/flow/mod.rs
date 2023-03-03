use crate::conversion::flow::apply::convert_apply;
use core::result::Result;
use core::result::Result::{Err, Ok};
use function_name::named;
use ompas_structs::conversion::flow_graph::define_table::DefineTable;
use ompas_structs::conversion::flow_graph::flow::{BranchingFlow, FlowId};
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::sym_table::closure::Update;
use ompas_structs::sym_table::domain::basic_type::BasicType;
use ompas_structs::sym_table::domain::basic_type::BasicType::Boolean;
use ompas_structs::sym_table::lit::{lvalue_to_lit, Lit};
use ompas_structs::sym_table::{closure, VarId};
use sompas_language::kind::ERR;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitive::LPrimitive;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::sync::Arc;

pub mod annotate;
pub mod apply;
pub mod p_eval;
pub mod post_processing;
pub mod pre_processing;

pub fn convert_lv(
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
    let var_id = match define_table.get(symbol.as_str()) {
        None => {
            let id = fl.st.new_symbol(symbol);
            id
        }
        Some(r) => *r,
    };

    let f_id: FlowId = fl.new_instantaneous_assignment(Lit::Atom(var_id)).into();

    //let r = fl.get_flow_result(&f_id);

    //fl.st.union_var(&r, &var_id);

    f_id
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> FlowId {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> FlowId {
    let id = fl.st.new_number(number);
    let f_id = fl.new_instantaneous_assignment(Lit::Atom(id));
    f_id
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> FlowId {
    let id = fl.st.new_bool(bool);
    let f_id = fl.new_instantaneous_assignment(Lit::Atom(id));
    f_id
}

fn convert_nil(fl: &mut FlowGraph) -> FlowId {
    let id = fl.st.new_nil();
    let f_id = fl.new_instantaneous_assignment(Lit::Atom(id));
    f_id
}

fn convert_core_operator(_: &LPrimitive, _: &mut FlowGraph) -> FlowId {
    todo!()
}

#[named]
fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<FlowId, LRuntimeError> {
    let proc = &list[0];
    let mut out_of_scope: Vec<VarId> = vec![];
    let mut st = fl.st.clone();

    let r = match proc {
        LValue::Symbol(s) => convert_apply(s.as_str(), list.as_slice(), fl, define_table)?,
        LValue::Primitive(co) => match co {
            LPrimitive::Define => {
                let var = &list[1].to_string();
                let val = &list[2];
                let flow_val = convert_lv(val, fl, define_table)?;
                define_table.insert(var.to_string(), fl.get_flow_result(&flow_val));
                let id = st.new_nil();
                let flow_result = fl.new_instantaneous_assignment(Lit::Atom(id));

                fl.new_seq(vec![flow_val, flow_result])
            }
            LPrimitive::If => {
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

                let cond_flow = convert_lv(cond, fl, define_table)?;
                let cond_result = st.get_domain_id(&fl.get_flow_result(&cond_flow));
                st.meet_to_domain(&cond_result, Boolean);
                let br_cond_flow = fl.new_instantaneous_assignment(fl.get_flow_result(&cond_flow));
                let cond_result = st.get_domain_id(&fl.get_flow_result(&cond_flow));

                let true_flow = convert_lv(true_branch, fl, define_table)?;
                let true_flow = fl.new_seq(vec![true_flow]);
                let false_flow = convert_lv(false_branch, fl, define_table)?;
                let false_flow = fl.new_seq(vec![false_flow]);
                let true_result = st.get_domain_id(&fl.get_flow_result(&true_flow));
                let false_result = st.get_domain_id(&fl.get_flow_result(&false_flow));

                let branching = BranchingFlow {
                    cond_flow: br_cond_flow,
                    true_flow,
                    false_flow,
                };
                let flow_branch = fl.new_branching(branching);

                let result = fl.get_flow_result(&flow_branch);
                let result = st.get_domain_id(&result);

                st.add_update(
                    vec![true_result, false_result],
                    Update::new(
                        result,
                        closure::union_update(result, vec![true_result, false_result]),
                    ),
                );
                st.add_update(
                    vec![result],
                    Update::new(true_result, closure::in_union_update(true_result, result)),
                );
                st.add_update(
                    vec![result],
                    Update::new(false_result, closure::in_union_update(false_result, result)),
                );

                st.add_update(
                    vec![true_result],
                    Update::new(
                        cond_result,
                        closure::result_branch_cond_update(cond_result, true_result, true),
                    ),
                );

                st.add_update(
                    vec![false_result],
                    Update::new(
                        cond_result,
                        closure::result_branch_cond_update(cond_result, false_result, false),
                    ),
                );

                st.add_update(
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

                fl.new_seq(vec![cond_flow, flow_branch])
            }
            LPrimitive::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut st)?;
                fl.new_instantaneous_assignment(lit)
            }
            LPrimitive::Begin => {
                let mut define_table = define_table.clone();
                let mut results = vec![];
                let mut seq = vec![];
                for e in &list[1..] {
                    let e_flow = convert_lv(e, fl, &mut define_table)?;
                    results.push(fl.get_flow_result(&e_flow));
                    seq.push(e_flow);
                }
                out_of_scope.append(&mut results);

                fl.new_seq(seq)
            }
            LPrimitive::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let async_flow = convert_lv(e, fl, define_table)?;
                let r_async = st.get_domain_id(&fl.get_flow_result(&async_flow));

                let handle_flow = fl.new_async(async_flow);

                let handle = st.get_domain_id(&fl.get_flow_result(&handle_flow));

                fl.handles.insert(handle, async_flow);

                st.add_update(
                    vec![r_async],
                    Update::new(handle, closure::composed_update(handle, r_async)),
                );

                st.add_update(
                    vec![handle],
                    Update::new(r_async, closure::in_composed_update(r_async, handle)),
                );

                handle_flow
            }
            LPrimitive::Await => {
                let define_table = &mut define_table.clone();
                let h = convert_lv(&list[1], fl, define_table)?;
                let result = fl.get_flow_result(&h);

                st.meet_to_domain(&result, BasicType::Handle);

                let flow_await = fl.new_assignment(Lit::Await(result));
                out_of_scope.push(fl.get_flow_result(&h));

                fl.new_seq(vec![h, flow_await])
            }
            LPrimitive::Err => {
                let define_table = &mut define_table.clone();
                let arg_err = convert_lv(&list[1], fl, define_table)?;
                let arg_err_result = st.get_domain_id(&fl.get_flow_result(&arg_err));
                let atom_err = st.new_symbol(ERR);
                let flow = fl.new_instantaneous_assignment(vec![atom_err, arg_err_result]);
                let err_result = st.get_domain_id(&fl.get_flow_result(&flow));

                st.set_domain(&err_result, BasicType::Err);

                st.add_update(
                    vec![arg_err_result],
                    Update::new(
                        err_result,
                        closure::composed_update(err_result, arg_err_result),
                    ),
                );
                st.add_update(
                    vec![err_result],
                    Update::new(
                        arg_err_result,
                        closure::in_composed_update(arg_err_result, err_result),
                    ),
                );

                fl.new_seq(vec![arg_err, flow])
            }
            LPrimitive::Race => {
                todo!()
            }
            co => Err(LRuntimeError::new(
                function_name!(),
                format!("Conversion of {} not supported.", co),
            ))?,
        },
        lv => Err(LRuntimeError::new(
            function_name!(),
            format!("Conversion of {} not supported", lv.get_kind()),
        ))?,
    };

    out_of_scope.append(&mut define_table.inner().values().copied().collect());

    for o in &out_of_scope {
        st.set_drop(o, &fl.get_flow_end(&r));
    }
    Ok(r)
}
