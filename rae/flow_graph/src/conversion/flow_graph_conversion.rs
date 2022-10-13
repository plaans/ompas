use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::lit::lvalue_to_lit;
use crate::structs::flow_graph::graph::{AsyncBlock, Block, IfBlock, Scope, Vertice};
use crate::{DefineTable, Expression, FlowGraph};
use core::result::Result;
use core::result::Result::{Err, Ok};
use ompas_rae_language::{
    RAE_ASSERT, RAE_ASSERT_SHORT, RAE_EXEC_COMMAND, RAE_EXEC_TASK, RAE_READ_STATE,
};
use sompas_structs::lcoreoperator::LCoreOperator;
use sompas_structs::lnumber::LNumber;
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
            fl.new_vertice(Expression::cst(id.into())).into()
        }
        Some(r) => fl.new_vertice(Expression::cst(r.into())).into(),
    }
}

fn convert_string(_: &Arc<String>, _: &mut FlowGraph) -> Scope {
    todo!()
    //fl.new_vertice(CstValue::string(string.to_string()), parent)
}

fn convert_number(number: &LNumber, fl: &mut FlowGraph) -> Scope {
    let id = fl.sym_table.new_number(number);
    fl.new_vertice(Expression::cst(id.into())).into()
    //fl.new_vertice(CstValue::number(*number), parent)
}

fn convert_bool(bool: bool, fl: &mut FlowGraph) -> Scope {
    let id = fl.sym_table.new_bool(bool);
    fl.new_vertice(Expression::cst(id.into())).into()
}

fn convert_core_operator(_: &LCoreOperator, _: &mut FlowGraph) -> Scope {
    todo!()
}

fn convert_list(
    list: &Arc<Vec<LValue>>,
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<Scope, LRuntimeError> {
    let proc = &list[0];

    match proc {
        LValue::Symbol(_) => convert_apply(list.as_slice(), fl, define_table),
        LValue::CoreOperator(co) => match co {
            LCoreOperator::Define => {
                let var = &list[1];
                let val = &list[2];
                let mut scope = convert_into_flow_graph(val, fl, define_table)?;
                define_table.insert(var.to_string(), *fl.get_result(scope.end()));
                let id = fl.sym_table.new_bool(false);
                let r = fl.new_vertice(Expression::cst(id.into()));
                fl.set_parent(&r, scope.end());
                scope.set_end(r);
                Ok(scope)
            }
            LCoreOperator::If => {
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
                    cond: *fl.get_result(cond.end()),
                    true_result: *fl.get_result(true_branch.end()),
                    false_result: *fl.get_result(false_branch.end()),
                    true_branch,
                    false_branch,
                };

                let if_id = fl.new_vertice(Expression::Block(Block::If(if_expression)));
                fl.set_parent(&if_id, cond.end());
                let mut scope = cond;
                scope.end = if_id;
                Ok(scope)
            }
            LCoreOperator::Quote => {
                let lit = lvalue_to_lit(&list[1], &mut fl.sym_table)?;
                Ok(fl.new_vertice(Expression::cst(lit)).into())
            }
            LCoreOperator::Begin => {
                let mut define_table = define_table.clone();
                let mut scope = Scope::default();
                let mut first = true;
                for e in &list[1..] {
                    let e_scope = convert_into_flow_graph(e, fl, &mut define_table)?;
                    if first {
                        scope = e_scope;
                        first = false;
                    } else {
                        fl.set_parent(e_scope.start(), scope.end());
                        scope.end = e_scope.end;
                    }
                }
                Ok(scope)
            }
            LCoreOperator::Async => {
                let define_table = &mut define_table.clone();
                let e = &list[1];
                let r_async = convert_into_flow_graph(e, fl, define_table)?;
                let async_block = AsyncBlock {
                    result: *fl.get_result(r_async.end()),
                    scope: r_async,
                };

                Ok(fl
                    .new_vertice(Expression::Block(Block::Async(async_block)))
                    .into())
            }
            LCoreOperator::Await => {
                let define_table = &mut define_table.clone();
                let mut h = convert_into_flow_graph(&list[1], fl, define_table)?;
                let a = fl.new_vertice(Expression::Await(*fl.get_result(h.end())));

                fl.set_parent(&a, h.end());
                h.end = a;
                Ok(h)
            }
            LCoreOperator::Race => {
                todo!()
            }
            co => panic!("Conversion of {} not supported.", co),
        },
        _ => panic!(""),
    }
}

fn convert_apply(
    expr: &[LValue],
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<Scope, LRuntimeError> {
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
            fl.set_parent(vertice_scope.start(), arg_scope.end());
            arg_scope.end = vertice_scope.end;
        }

        args_result.push(*fl.get_result(vertice_scope.end()));
    }

    Ok(match proc_symbol.as_str() {
        RAE_EXEC_COMMAND | RAE_EXEC_TASK => {
            let start = fl.sym_table.new_timepoint();
            let end = fl.sym_table.new_timepoint();
            let r = fl.sym_table.new_result();

            let vertice = Vertice {
                id: 0,
                interval: Interval::new(&start, &end),
                result: r,
                parent: None,
                child: None,
                computation: Expression::exec(args_result.to_vec()),
            };
            let exec_scope: Scope = fl.push(vertice).into();
            fl.set_parent(exec_scope.start(), arg_scope.end());
            arg_scope.end = exec_scope.end;
            arg_scope
        }
        RAE_ASSERT | RAE_ASSERT_SHORT => {
            let id = fl.new_vertice(Expression::write(args_result.to_vec()));
            fl.set_parent(&id, arg_scope.end());
            arg_scope.end = id;
            arg_scope
        }
        RAE_READ_STATE => {
            let id = fl.new_vertice(Expression::read(args_result.to_vec()));
            fl.set_parent(&id, arg_scope.end());
            arg_scope.end = id;
            arg_scope
        }
        _ => {
            fl.set_child(proc_scope.end(), arg_scope.start());

            let mut results = vec![*fl.get_result(proc_scope.end())];
            results.append(&mut args_result);

            let id = fl.new_vertice(Expression::apply(results));
            fl.set_parent(&id, arg_scope.end());

            let scope = Scope {
                start: proc_scope.start,
                end: id,
            };

            scope
        }
    })
}
