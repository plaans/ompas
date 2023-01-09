use crate::conversion::flow::convert_into_flow_graph;
use crate::structs::domain::root_type::RootType::Boolean;
use crate::structs::domain::Domain;
use crate::structs::flow_graph::define_table::DefineTable;
use crate::structs::flow_graph::flow::FlowId;
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::{closure, AtomId};
use ompas_rae_language::exec::platform::EXEC_COMMAND;
use ompas_rae_language::exec::refinement::EXEC_TASK;
use ompas_rae_language::exec::state::{ASSERT, ASSERT_SHORT, READ_STATE};
use sompas_language::error::IS_ERR;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::HashMap;
use std::fmt::Display;

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
        seq.push(convert_into_flow_graph(e, fl, &mut define_table)?);
    }

    let results: Vec<AtomId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply = match conv_collection.get_conversion(&proc) {
        Some(proc) => proc(fl, seq)?,
        None => {
            seq.push(fl.new_assignment(Lit::Apply(results.clone())));
            fl.new_seq(seq)
        }
    };

    let end = fl.get_flow_end(&flow_apply);

    for o in results {
        fl.sym_table.set_end(&o, &end);
    }
    Ok(flow_apply)
}

fn convert_is_err(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let flow_apply = fl.new_assignment(Lit::Apply(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));
    let result_is_err = fl.get_flow_result(&flow_apply);
    fl.sym_table.set_domain(&result_is_err, Boolean);
    assert_eq!(seq.len(), 2);
    let arg_is_err = fl.get_flow_result(&seq[1]);
    fl.sym_table.add_update(
        vec![result_is_err],
        Update::new(
            arg_is_err,
            closure::arg_is_err_update(arg_is_err, result_is_err),
        ),
    );
    fl.sym_table.add_update(
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

    fl.sym_table.set_domain(&flow_result, Domain::nil());

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
