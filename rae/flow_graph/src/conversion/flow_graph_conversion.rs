use crate::structs::domain::root_type::RootType;
use crate::structs::domain::root_type::RootType::Boolean;
use crate::structs::flow_graph::flow::{BranchingFlow, FlowId, FlowKind};
use crate::structs::flow_graph::handle_table::Handle;
use crate::structs::sym_table::lit::{lvalue_to_lit, Lit};
use crate::structs::sym_table::AtomId;
use crate::{DefineTable, FlowGraph};
use core::result::Result;
use core::result::Result::{Err, Ok};
use ompas_rae_language::exec::state::{ASSERT, ASSERT_SHORT};
use sompas_language::kind::ERR;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lprimitives::LPrimitives;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
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
            let id = fl.sym_table.new_symbol(symbol);
            fl.new_instantaneous_vertice(Lit::Atom(id)).into()
        }
        Some(r) => fl.new_instantaneous_vertice(Lit::Atom(*r)).into(),
    };

    fl.new_vertice_flow(vertice_id)
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> FlowId {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_number(number);
    let vertice_id = fl.new_instantaneous_vertice(Lit::Atom(id));
    fl.new_vertice_flow(vertice_id)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> FlowId {
    let id = fl.sym_table.new_bool(bool);
    let vertice_id = fl.new_instantaneous_vertice(Lit::Atom(id));
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
                define_table.insert(var.to_string(), fl.get_flow_result(&flow_val));
                let id = fl.sym_table.new_bool(false);
                let result = fl.new_instantaneous_vertice(Lit::Atom(id));
                let flow_result = fl.new_vertice_flow(result);

                Ok(fl.new_flow(FlowKind::Seq(vec![flow_val, flow_result])))
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

                /*let domain = fl.sym_table.meet_domain(
                    &fl.sym_table.get_domain(&cond_result, false).unwrap(),
                    &Boolean.into(),
                );
                fl.sym_table.set_domain(&cond_result, domain);*/
                let true_flow = convert_into_flow_graph(true_branch, fl, define_table)?;
                let false_flow = convert_into_flow_graph(false_branch, fl, define_table)?;

                let true_result = fl.get_flow_result(&true_flow);
                let false_result = fl.get_flow_result(&false_flow);

                /*let vertice_result = fl.get_vertice_result(&vertice_id);
                let interval = *fl.get_vertice_interval(&vertice_id);
                let mut vertice = &mut fl.vertices[vertice_id_2];
                vertice.result = vertice_result;
                vertice.interval = interval;

                let true_result_flow = fl.new_flow(vertice_id);
                let true_flow = fl.merge_flow(&true_flow, &true_result_flow);

                let false_result_flow = fl.new_flow(vertice_id_2);
                let false_flow = fl.merge_flow(&false_flow, &false_result_flow);*/

                let result = fl.sym_table.new_result();
                fl.sym_table
                    .add_union_dependency(&result, vec![true_result, false_result]);
                fl.sym_table.add_parent_dependency(&true_result, result);
                fl.sym_table.add_parent_dependency(&false_result, result);

                let vertice_id = fl.new_instantaneous_vertice(result);
                let result = fl.new_flow(vertice_id);

                let branching = BranchingFlow {
                    cond_flow,
                    true_flow,
                    false_flow,
                    result,
                };

                Ok(fl.new_flow(FlowKind::Branching(branching)))
            }
            LPrimitives::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                let vertice = fl.new_instantaneous_vertice(lit);
                Ok(fl.new_flow(vertice))
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

                Ok(fl.new_flow(FlowKind::Seq(seq)))
            }
            LPrimitives::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let async_flow_id = convert_into_flow_graph(e, fl, define_table)?;

                let handle_id = fl.sym_table.new_handle();

                let vertice = fl.new_instantaneous_vertice(Lit::Atom(handle_id));

                let handle = Handle {
                    result: fl.get_flow_result(&async_flow_id),
                    flow: async_flow_id,
                    ends: vec![],
                };

                fl.handles.insert(&handle_id, handle);

                Ok(fl.new_vertice_flow(vertice))
            }
            LPrimitives::Await => {
                let define_table = &mut define_table.clone();
                let mut h = convert_into_flow_graph(&list[1], fl, define_table)?;
                let result = fl.get_flow_result(&h);

                fl.sym_table.meet_to_domain(&result, RootType::Handle);

                /*let domain = fl.sym_table.meet_domain(
                    &fl.sym_table.get_domain(&result, false).unwrap(),
                    &RootType::Handle.into(),
                );
                fl.sym_table.set_domain(&result, domain);*/
                let a = fl.new_vertice(Lit::Await(result));
                let flow_await = fl.new_vertice_flow(a);
                out_of_scope.push(fl.get_flow_result(&h));

                Ok(fl.new_flow(FlowKind::Seq(vec![h, flow_await])))
            }
            LPrimitives::Err => {
                let define_table = &mut define_table.clone();
                let err = convert_into_flow_graph(&list[1], fl, define_table)?;
                let result_err = fl.get_flow_result(&err);
                let atom_err = fl.sym_table.new_symbol(ERR);
                let vertice_id = fl.new_vertice(vec![atom_err, result_err]);
                let result_vertice = fl.get_vertice_result(&vertice_id);
                fl.sym_table.set_domain(&result_vertice, RootType::Err);
                Ok(fl.new_flow(vertice_id))
            }
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
    let apply_id = fl.new_vertice(Lit::Apply(results.clone()));
    let end = *fl.get_vertice_interval(&apply_id).get_end();
    seq.push(fl.new_vertice_flow(apply_id));
    for o in results {
        fl.sym_table.set_end(&o, &end);
    }
    Ok(fl.new_flow(FlowKind::Seq(seq)))

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
