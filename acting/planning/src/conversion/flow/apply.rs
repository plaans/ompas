use crate::conversion::flow::convert_lv;
use function_name::named;
use ompas_language::exec::acting_context::{
    CTX_ACQUIRE, CTX_ARBITRARY, CTX_EXEC_COMMAND, CTX_EXEC_TASK,
};
use ompas_language::exec::platform::EXEC_COMMAND;
use ompas_language::exec::refinement::EXEC_TASK;
use ompas_language::exec::resource::RELEASE;
use ompas_language::exec::state::{
    ASSERT, ASSERT_SHORT, INSTANCE, INSTANCES, READ_STATE, WAIT_FOR,
};
use ompas_language::exec::ARBITRARY;
use ompas_language::supervisor::ACQUIRE;
use ompas_language::sym_table::{TYPE_OBJECT, TYPE_OBJECT_TYPE, TYPE_RESSOURCE_HANDLE};
use ompas_structs::acting_manager::process::process_ref::Label;
use ompas_structs::conversion::chronicle::constraint::Constraint;
use ompas_structs::conversion::flow_graph::define_table::DefineTable;
use ompas_structs::conversion::flow_graph::flow::FlowId;
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::sym_table::closure::Update;
use ompas_structs::sym_table::computation::Computation;
use ompas_structs::sym_table::domain::basic_type::BasicType::{Boolean, True};
use ompas_structs::sym_table::domain::Domain;
use ompas_structs::sym_table::lit::{AcquireLit, Lit};
use ompas_structs::sym_table::litset::LitSet;
use ompas_structs::sym_table::r#trait::FormatWithSymTable;
use ompas_structs::sym_table::{closure, VarId};
use sompas_language::basic_math::{ADD, EQ, GEQ, GT, LEQ, LT, NEQ, NOT, NOT_SHORT, SUB};
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
        d.add_conversion(ASSERT, convert_assert);
        d.add_conversion(ASSERT_SHORT, convert_assert);
        d.add_conversion(READ_STATE, convert_read_state);
        d.add_conversion(EQ, convert_eq);
        d.add_conversion(LEQ, convert_leq);
        d.add_conversion(LT, convert_lt);
        d.add_conversion(GEQ, convert_geq);
        d.add_conversion(GT, convert_gt);
        d.add_conversion(NOT, convert_not);
        d.add_conversion(NOT_SHORT, convert_not);
        d.add_conversion(NEQ, convert_neq);
        d.add_conversion(AND, convert_and);
        d.add_conversion(OR, convert_or);
        d.add_conversion(WAIT_FOR, convert_wait_for);
        d.add_conversion(SLEEP, convert_sleep);
        d.add_conversion(ACQUIRE, convert_acquire);
        d.add_conversion(RESSOURCE_HANDLE, convert_ressource_handle);
        d.add_conversion(RELEASE, convert_release);
        d.add_conversion(ADD, convert_add);
        d.add_conversion(SUB, convert_sub);
        d.add_conversion(INSTANCE, convert_instance);
        d.add_conversion(INSTANCES, convert_instances);
        d.add_conversion(ARBITRARY, convert_arbitrary);
        d.add_conversion(CTX_ARBITRARY, convert_ctx_arbitrary);
        d.add_conversion(CTX_EXEC_TASK, convert_ctx_exec_task);
        d.add_conversion(CTX_EXEC_COMMAND, convert_ctx_exec_command);
        d.add_conversion(CTX_ACQUIRE, convert_ctx_acquire);
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
    seq.push(fl.new_wait(None));
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

fn convert_assert(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let flow_apply = fl.new_assignment(Lit::Write(
        seq.iter().map(|f| fl.get_flow_result(f)).collect(),
    ));

    fl.st
        .set_domain(&fl.get_flow_result(&flow_apply), Domain::nil());
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

fn convert_neq(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let a = fl.get_flow_result(&seq[0]);
    let b = fl.get_flow_result(&seq[1]);

    let flow_apply =
        fl.new_instantaneous_assignment(Lit::Constraint(Box::new(Constraint::neq(a, b))));

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

fn convert_add(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let args: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply = fl.new_instantaneous_assignment(Lit::computation(Computation::add(args)));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_sub(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let args: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();

    let flow_apply = fl.new_instantaneous_assignment(Lit::computation(Computation::sub(args)));

    seq.push(flow_apply);
    Ok(fl.new_seq(seq))
}

fn convert_wait_for(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq[0] = fl.new_wait(None);
    let last = fl.get_flow_result(&seq.last().unwrap());
    fl.st.set_domain(&fl.st.get_domain_id(&last), True);
    let nil = fl.st.new_nil();
    let result = fl.new_instantaneous_assignment(nil);
    seq.push(result);

    Ok(fl.new_seq(seq))
}

#[named]
fn convert_arbitrary(fl: &mut FlowGraph, seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let lit = fl
        .try_get_flow_lit(&seq[1])
        .ok_or_else(|| LRuntimeError::new(function_name!(), "arg of arbitrary is not a lit"))?;

    match lit {
        Lit::Set(set) => {
            let flow_apply = fl.new_instantaneous_assignment(Lit::Constraint(Box::new(
                Constraint::arbitrary(set),
            )));
            //seq.push(flow_apply);
            Ok(fl.new_seq(vec![flow_apply]))
        }
        Lit::Exp(exp) => {
            let mut set = vec![];
            let mut domain = Domain::any();
            for e in exp {
                if let Lit::Atom(a) = e {
                    set.push(a);
                    let d_var = fl.st.get_domain_of_var(&a).get_type();
                    domain = fl.st.meet(&domain, &d_var);
                    if domain.is_empty() {
                        return Err(LRuntimeError::new(
                            function_name!(),
                            "Variables of set do not have the same type",
                        ));
                    }
                } else {
                    return Err(LRuntimeError::new(
                        function_name!(),
                        format!("{} is not an atom", e.format(&fl.st, true)),
                    ));
                }
            }

            let flow_apply = fl.new_instantaneous_assignment(Lit::Constraint(Box::new(
                Constraint::arbitrary(LitSet::Finite(set)),
            )));
            let r_flow_apply = fl.get_flow_result(&flow_apply);
            fl.st
                .meet_to_domain(&fl.st.get_domain_id(&r_flow_apply), domain);

            Ok(fl.new_seq(vec![flow_apply]))
        }
        _ => Err(LRuntimeError::new(
            function_name!(),
            "arg of arbitrary is not a set",
        )),
    }
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

fn convert_acquire(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    seq.remove(0);

    let release_time = fl.st.new_timepoint();
    let resource = fl.get_flow_result(&seq[0]);
    let capacity = seq.get(1).map(|f| fl.get_flow_result(f));
    let acquire = fl.new_assignment(Lit::Acquire(AcquireLit {
        resource,
        capacity,
        release_time,
    }));

    let acquire_result = fl.get_flow_result(&acquire);
    let acquire_domain = fl.st.get_domain_id(&acquire_result);
    fl.st.set_domain(
        &acquire_domain,
        fl.st.get_type_as_domain(TYPE_RESSOURCE_HANDLE).unwrap(),
    );
    fl.resource_handles
        .insert(fl.get_flow_result(&acquire), release_time);

    seq.push(acquire);
    Ok(fl.new_seq(seq))
}

fn convert_ressource_handle(fl: &mut FlowGraph, seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let rh = fl.new_resource_handle(seq[1]);
    fl.handles.insert(fl.get_flow_result(&rh), seq[1]);

    Ok(rh)
}

fn convert_release(fl: &mut FlowGraph, seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let rh = seq[1];
    let result = fl.get_flow_result(&rh);
    let result_domain = fl.st.get_domain_id(&result);
    fl.st.set_domain(
        &result_domain,
        fl.st.get_type_as_domain(TYPE_RESSOURCE_HANDLE).unwrap(),
    );

    let flow_release = fl.new_instantaneous_assignment(Lit::Release(result));
    fl.st
        .set_domain(&fl.get_flow_result(&flow_release), Domain::nil());

    Ok(fl.new_seq(vec![rh, flow_release]))
}

fn convert_instance(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let results: Vec<VarId> = seq.iter().map(|f| fl.get_flow_result(f)).collect();
    fl.st
        .set_domain(&results[1], fl.st.get_type_as_domain(TYPE_OBJECT).unwrap());
    fl.st.set_domain(
        &results[2],
        fl.st.get_type_as_domain(TYPE_OBJECT_TYPE).unwrap(),
    );
    let args = results[..2].to_vec();
    let flow_read = fl.new_instantaneous_assignment(Lit::Read(args));
    let flow_equal = fl.new_instantaneous_assignment(Lit::constraint(Constraint::eq(
        fl.get_flow_result(&flow_read),
        results[2],
    )));
    seq.push(flow_read);
    seq.push(flow_equal);
    Ok(fl.new_seq(seq))
}

#[named]
fn convert_instances(fl: &mut FlowGraph, seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let d = fl
        .try_get_flow_lit(&seq[1])
        .ok_or_else(|| LRuntimeError::new(function_name!(), ""))?;

    if let Lit::Atom(d) = d {
        let flow_domain = fl.new_instantaneous_assignment(Lit::Set(LitSet::Domain(d)));

        Ok(flow_domain)
    } else {
        Err(LRuntimeError::new(function_name!(), ""))?
    }
}

fn extract_index(fl: &mut FlowGraph, seq: &mut Vec<FlowId>) -> usize {
    let flow_index = seq.remove(1);
    let lit = fl.try_get_flow_lit(&flow_index).unwrap();
    let index = if let Lit::Atom(v) = lit {
        //println!("{}", fl.st.format_variable(&v));
        fl.st
            .get_domain_of_var(&v)
            .as_constant()
            .unwrap()
            .as_int()
            .unwrap()
    } else {
        panic!()
    };
    index as usize
}

//(ctx-arbitrary <id> (list args))
fn convert_ctx_arbitrary(
    fl: &mut FlowGraph,
    mut seq: Vec<FlowId>,
) -> Result<FlowId, LRuntimeError> {
    let index = extract_index(fl, &mut seq);
    let flow_id = convert_arbitrary(fl, seq)?;
    let flow_arbitrary = fl.try_get_last_flow(&flow_id).unwrap();

    fl.set_label(&flow_arbitrary, Label::Arbitrary(index as usize));
    Ok(flow_id)
}

fn convert_ctx_exec_command(
    fl: &mut FlowGraph,
    mut seq: Vec<FlowId>,
) -> Result<FlowId, LRuntimeError> {
    let index = extract_index(fl, &mut seq);
    let flow_id = convert_exec(fl, seq)?;
    let flow_exec = fl.try_get_last_flow(&flow_id).unwrap();
    fl.set_label(&flow_exec, Label::Action(index as usize));
    Ok(flow_id)
}

fn convert_ctx_exec_task(
    fl: &mut FlowGraph,
    mut seq: Vec<FlowId>,
) -> Result<FlowId, LRuntimeError> {
    let index = extract_index(fl, &mut seq);
    let flow_id = convert_exec(fl, seq)?;
    let flow_exec = fl.try_get_last_flow(&flow_id).unwrap();
    fl.set_label(&flow_exec, Label::Action(index as usize));
    Ok(flow_id)
}

fn convert_ctx_acquire(fl: &mut FlowGraph, mut seq: Vec<FlowId>) -> Result<FlowId, LRuntimeError> {
    let index = extract_index(fl, &mut seq);
    let flow_id = convert_acquire(fl, seq)?;
    let flow_acquire = fl.try_get_last_flow(&flow_id).unwrap();
    fl.set_label(&flow_acquire, Label::Acquire(index as usize));
    Ok(flow_id)
}
