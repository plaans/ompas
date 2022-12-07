use crate::structs::chronicle::lit::lvalue_to_lit;
use crate::structs::chronicle::AtomId;
use crate::structs::flow_graph::expression::{Block, Expression, IfBlock};
use crate::structs::flow_graph::handle_table::Handle;
use crate::structs::flow_graph::scope::Scope;
use crate::{DefineTable, FlowGraph};
use core::result::Result;
use core::result::Result::{Err, Ok};
use ompas_rae_language::{
    RAE_ASSERT, RAE_ASSERT_SHORT, RAE_EXEC_COMMAND, RAE_EXEC_TASK, RAE_READ_STATE,
};
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
) -> Result<Scope, LRuntimeError> {
    let node_id = match lv {
        LValue::Symbol(s) => convert_symbol(s, fl, define_table),
        LValue::String(s) => convert_string(s, fl),
        LValue::Number(n) => convert_number(n, fl),
        LValue::CoreOperator(co) => convert_core_operator(co, fl),
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

fn convert_symbol(symbol: &Arc<String>, fl: &mut FlowGraph, define_table: &DefineTable) -> Scope {
    match define_table.get(symbol.as_str()) {
        None => {
            let id = fl.sym_table.new_symbol(symbol, None);
            fl.new_instantaneous_vertice(Expression::cst(id)).into()
        }
        Some(r) => fl.new_instantaneous_vertice(Expression::cst(r)).into(),
    }
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> Scope {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> Scope {
    let id = fl.sym_table.new_number(number);
    fl.new_instantaneous_vertice(Expression::cst(id)).into()
    //fl.new_vertice(CstValue::number(*number), parent)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> Scope {
    let id = fl.sym_table.new_bool(bool);
    fl.new_instantaneous_vertice(Expression::cst(id)).into()
}

fn convert_core_operator(_: &LPrimitives, _: &mut FlowGraph) -> Scope {
    todo!()
}

fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<Scope, LRuntimeError> {
    let proc = &list[0];
    let end_scope;
    let mut out_of_scope: Vec<AtomId> = vec![];

    let r = match proc {
        LValue::Symbol(_) => {
            let (s, e, r) = convert_apply(list.as_slice(), fl, define_table)?;
            end_scope = e;
            out_of_scope = r;
            Ok(s)
        }
        LValue::CoreOperator(co) => match co {
            LPrimitives::Define => {
                let var = &list[1];
                let val = &list[2];
                let mut scope_val = convert_into_flow_graph(val, fl, define_table)?;
                let val = *fl.get_scope_result(&scope_val);

                let vertice_var = fl.new_instantaneous_vertice(Expression::cst(val));
                define_table.insert(var.to_string(), *fl.get_result(&vertice_var));
                fl.set_parent(&vertice_var, scope_val.get_end());

                let id = fl.sym_table.new_bool(false);
                let r = fl.new_instantaneous_vertice(Expression::cst(id));
                fl.set_parent(&r, &vertice_var);
                scope_val.set_end(r);

                end_scope = *fl.get_scope_interval(&scope_val).get_end();
                out_of_scope.push(val);
                Ok(scope_val)
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
                Ok(scope)
            }
            LPrimitives::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                let vertice = fl.new_instantaneous_vertice(Expression::cst(lit));
                end_scope = *fl.get_interval(&vertice).get_end();
                Ok(vertice.into())
            }
            LPrimitives::Begin => {
                let mut define_table = define_table.clone();
                let mut scope = Scope::default();
                let mut results = vec![];
                let mut first = true;
                for e in &list[1..] {
                    let e_scope = convert_into_flow_graph(e, fl, &mut define_table)?;
                    results.push(*fl.get_scope_result(&e_scope));
                    if first {
                        scope = e_scope;
                        first = false;
                    } else {
                        fl.set_parent(e_scope.start(), scope.get_end());
                        scope.end = e_scope.end;
                    }
                }
                end_scope = *fl.get_scope_interval(&scope).get_end();
                out_of_scope.append(&mut results);
                Ok(scope)
            }
            LPrimitives::Async => {
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
            }
            LPrimitives::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    };

    out_of_scope.append(&mut define_table.inner().values().map(|a| *a).collect());

    for o in &out_of_scope {
        fl.sym_table.set_end(o, &end_scope);
    }
    r
}

fn convert_apply(
    expr: &[LValue],
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<(Scope, AtomId, Vec<AtomId>), LRuntimeError> {
    let mut out_of_scope = vec![];
    let end_scope;

    let mut define_table = define_table.clone();

    let proc_symbol: String = expr[0].borrow().try_into()?;
    let proc_scope = convert_into_flow_graph(&expr[0], fl, &mut define_table)?;

    let mut arg_scope: Scope = Default::default();
    let mut args_result = vec![];
    let mut first = true;
    for arg in &expr[1..] {
        let vertice_scope = convert_into_flow_graph(arg, fl, &mut define_table)?;
        if first {
            first = false;
            arg_scope = vertice_scope
        } else {
            fl.set_parent(vertice_scope.start(), arg_scope.get_end());
            arg_scope.end = vertice_scope.end;
        }

        args_result.push(*fl.get_result(vertice_scope.get_end()));
    }

    let mut results = args_result.clone();
    out_of_scope.append(&mut results.clone());

    let scope = match proc_symbol.as_str() {
        RAE_EXEC_COMMAND | RAE_EXEC_TASK => {
            let exec_scope: Scope = fl
                .new_vertice(Expression::exec(args_result.to_vec()))
                .into();
            fl.set_parent(exec_scope.start(), arg_scope.get_end());
            arg_scope.end = exec_scope.end;
            arg_scope
        }
        RAE_ASSERT | RAE_ASSERT_SHORT => {
            let id = fl.new_instantaneous_vertice(Expression::write(args_result.to_vec()));
            fl.set_parent(&id, arg_scope.get_end());
            arg_scope.end = id;
            arg_scope
        }
        RAE_READ_STATE => {
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

            let scope = Scope {
                start: proc_scope.start,
                end: id,
            };

            scope
        }
    };

    end_scope = *fl.get_scope_interval(&scope).get_end();

    Ok((scope, end_scope, out_of_scope))
}
