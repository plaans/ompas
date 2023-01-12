use crate::conversion::flow::convert_lv;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::domain::root_type::RootType::{Boolean, True};
use crate::structs::domain::Domain;
use crate::structs::flow_graph::define_table::DefineTable;
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::{closure, VarId};
use ompas_rae_language::exec::platform::EXEC_COMMAND;
use ompas_rae_language::exec::refinement::EXEC_TASK;
use ompas_rae_language::exec::resource::RELEASE;
use ompas_rae_language::exec::state::{ASSERT, ASSERT_SHORT, READ_STATE, WAIT_FOR};
use sompas_language::basic_math::{EQ, GEQ, GT, LEQ, LT, NOT, NOT_SHORT};
use sompas_language::error::IS_ERR;
use sompas_language::time::SLEEP;
use sompas_language::utils::{AND, OR};
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::Display;

pub const RESSOURCE_HANDLE: &str = "ressource-handle";

pub type ApplyConversion =
    fn(fl: &mut FlowGraph, seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError>;

pub struct ApplyConversionCollection {
    inner: HashMap<String, ApplyConversion>,
}

impl ApplyConversionCollection {
    pub fn get_conversion(&self, proc: &str) -> Option<&ApplyConversion> {
        self.inner.get(proc)
    }

    pub fn add_conversion(&mut self, symbol: impl Display, conversion: ApplyConversion) {
        self.inner.insert(symbol.to_string(), conversion);
    }
}

impl Default for ApplyConversionCollection {
    fn default() -> Self {
        let mut d = Self {
            inner: Default::default(),
        };

        d.add_conversion(IS_ERR, convert_is_err);
        d.add_conversion(EXEC_COMMAND, convert_exec);
        d.add_conversion(EXEC_TASK, convert_exec);
        d.add_conversion(ASSERT, convert_write);
        d.add_conversion(ASSERT_SHORT, convert_write);
        d.add_conversion(READ_STATE, convert_read_state);
        d.add_conversion(EQ, convert_eq);
        d.add_conversion(LEQ, convert_leq);
        d.add_conversion(LT, convert_lt);
        d.add_conversion(GEQ, convert_geq);
        d.add_conversion(GT, convert_gt);
        d.add_conversion(NOT, convert_not);
        d.add_conversion(NOT_SHORT, convert_not);
        d.add_conversion(AND, convert_and);
        d.add_conversion(OR, convert_or);
        d.add_conversion(WAIT_FOR, convert_wait_for);
        d.add_conversion(SLEEP, convert_sleep);
        d.add_conversion(RESSOURCE_HANDLE, convert_ressource_handle);
        d.add_conversion(RELEASE, convert_release);
        d
    }
}

pub fn convert_apply(
    proc: &str,
    expr: &[LValue],
    fl: &mut FlowGraph,
    define_table: &mut DefineTable,
) -> Result<FlowId, LRuntimeError> {
    let conv_collection = ApplyConversionCollection::default();

    let mut define_table = define_table.clone();

    let mut seq: Vec<FlowId> = vec![];

    for e in expr {
        seq.push(convert_lv(e, fl, &mut define_table)?);
    }

    let results: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply = match conv_collection.get_conversion(&proc) {
        Some(proc) => proc(fl, seq)?,
        None => {
            seq.push(fl.new_assignment(Lit::Apply(results.clone())));
            fl.new_seq(seq)
        }
    };

    let end = fl.get_flow_end(&flow_apply);

    for o in results {
        fl.st.set_drop(&o, &end);
    }
    Ok(flow_apply)
}

fn convert_is_err(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let flow_apply = fl.new_instantaneous_assignment(Lit::Apply(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));
    let result_is_err = fl.get_flow_result(&flow_apply);
    fl.st.set_domain(&result_is_err, Boolean);
    assert_eq!(seq.len(), 2);
    let arg_is_err = fl.get_flow_result(&seq[1]);
    fl.st.add_update(
        vec![result_is_err],
        Update::new(
            arg_is_err,
            closure::arg_is_err_update(arg_is_err, result_is_err),
        ),
    );
    fl.st.add_update(
        vec![arg_is_err],
        Update::new(
            result_is_err,
            closure::result_is_err_update(result_is_err, arg_is_err),
        ),
    );

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_exec(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let flow_apply = fl.new_assignment(Lit::Exec(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));

    let flow_result = fl.get_flow_result(&flow_apply);

    fl.st.set_domain(&flow_result, Domain::nil());

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_read_state(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let flow_apply = fl.new_instantaneous_assignment(Lit::Read(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_write(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let flow_apply = fl.new_instantaneous_assignment(Lit::Write(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_eq(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let left = fl.get_flow_result(&seq[0]);
    let right = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::eq(left, right))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_leq(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let left = fl.get_flow_result(&seq[0]);
    let right = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::leq(left, right))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_lt(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let left = fl.get_flow_result(&seq[0]);
    let right = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::lt(left, right))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_gt(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let left = fl.get_flow_result(&seq[0]);
    let right = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::lt(right, left))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_geq(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let left = fl.get_flow_result(&seq[0]);
    let right = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::leq(right, left))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_not(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let val = fl.get_flow_result(&seq[0]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::not(val))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_and(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let args: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::and(args))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_or(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let args: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::or(args))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

/*fn convert_add(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let args: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::or(args))));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}*/

fn convert_wait_for(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq[0] = fl.new_wait(None);
    let last = fl.get_flow_result(&seq.last().unwrap());
    fl.st.set_domain(&fl.st.get_domain_id(&last), True);
    let nil = fl.st.new_nil();
    let result = fl.new_instantaneous_assignment(nil);
    seq.push(result);

    Ok(fl.new_seq(seq))
}

fn convert_arbitrary(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    todo!()
}

fn convert_sleep(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);
    let wait = fl.new_wait(Some(fl.get_flow_result(&seq.last().unwrap())));
    seq.push(wait);
    let nil = fl.st.new_nil();
    let r = fl.new_instantaneous_assignment(nil);
    seq.push(r);

    Ok(fl.new_seq(seq))
}

fn convert_ressource_handle(
    fl: &mut FlowGraph,
    mut seq: Vec<FlowId>,
) -> Result<FlowId, LRuntimeError> {
    let rh = fl.new_resource_handle(seq[1]);
    fl.handles.insert(fl.get_flow_result(&rh), seq[1]);

    Ok(rh)
}

fn convert_release(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let rh = seq[1];
    let result = fl.get_flow_result(&rh);

    let flow_release = fl.new_assignment(Lit::Release(result));

    Ok(fl.new_seq(vec![rh, flow_release]))
}

/*fn convert_acquire(
    fl: &mut FlowGraph,
    dt: &mut DefineTable,
    r: &LValue,
    q: Option<&LValue>,
) -> Result<FlowId, LRuntimeError> {
    let q = match q {
        Some(q) => q.to_string(),
        None => "(read-state max-q __r__)",
    };

    let def__r__ = format!("(define __r__ {r})");

}*/
