use crate::conversion::chronicle_post_processing::{
    rm_useless_var, simplify_timepoints, unify_equal,
};
use crate::structs::chronicle::chronicle::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::lit::Lit;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::chronicle::type_table::AtomType;
use crate::structs::chronicle::{AtomId, GetVariables, Replace, COND};
use crate::structs::flow_graph::expression::{Block, Expression};
use crate::structs::flow_graph::handle_table::HandleTable;
use crate::structs::flow_graph::scope::Scope;
use crate::FlowGraph;
use im::HashMap;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::HashSet;

pub struct Await {
    result: AtomId,
    handle: AtomId,
}

pub struct PartialConversion {
    chronicle: ChronicleTemplate,
    awaits: Vec<Await>,
    handles: HandleTable,
}

pub fn convert_method(graph: &FlowGraph, scope: Scope) -> Result<ChronicleTemplate, LRuntimeError> {
    let partial = convert_into_chronicle(graph, scope)?;
    post_processing(partial)
}

pub fn convert_into_chronicle(
    graph: &FlowGraph,
    scope: Scope,
) -> Result<PartialConversion, LRuntimeError> {
    let mut awaits: Vec<Await> = vec![];

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    let mut queue = vec![scope.start];

    let handles = graph.handles.clone();

    //Binds the start timepoint of the first expression with start timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().get_start(),
        graph.get(scope.start()).unwrap().interval.get_start(),
    ));

    //Binds the result of the last expression, with the result of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_result(),
        graph.get_result(scope.get_end()),
    ));

    //Binds the end timepoint of the last expression with end timepoint of the chronicle
    ch.add_constraint(Constraint::eq(
        ch.get_interval().get_end(),
        graph.get(scope.get_end()).unwrap().interval.get_end(),
    ));

    while let Some(id) = queue.pop() {
        let vertice = graph.get(&id).unwrap();
        if !vertice.interval.is_instantaneous() {
            ch.add_constraint(Constraint::leq(
                *vertice.interval.get_start(),
                *vertice.interval.get_end(),
            ));
        }
        match vertice.get_computation() {
            Expression::Apply(a) => ch.add_constraint(Constraint::eq(vertice.result, a)),
            Expression::Write(vec) => {
                let sv = vec[0..vec.len() - 1].to_vec();
                let value = *vec.last().unwrap();
                let effect = Effect {
                    interval: vertice.interval,
                    sv,
                    value,
                };

                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.result, result));

                ch.add_effect(effect);
            }
            Expression::Read(vec) => {
                let condition = Condition {
                    interval: vertice.interval,
                    sv: vec.clone(),
                    value: vertice.result,
                };

                ch.add_condition(condition);
            }
            Expression::Cst(lit) => {
                ch.add_constraint(Constraint::Eq(vertice.result.into(), lit.clone()))
            }
            //Expression::Await(_) => {}
            Expression::Exec(vec) => {
                let subtask = SubTask {
                    interval: vertice.interval,
                    lit: vec.into(),
                    result: vertice.result,
                };

                ch.add_subtask(subtask);
                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.get_result(), result));
            }
            Expression::Err(_) => {}
            Expression::Block(block) => match block {
                Block::If(if_block) => {
                    let (t_if, m_true, m_false) = ch.sym_table.new_if();

                    /*
                    Partially converts the a branch of 'if', getting by the mean time the variables necessary in computation of the branch that have been defined previously.
                    It also returns the symbol of cond, as it is necessary
                     */
                    let mut partially_convert_branch = |scope: &Scope,
                                                        branch: bool,
                                                        label: AtomId|
                     -> Result<
                        (PartialConversion, HashMap<AtomId, AtomId>),
                        LRuntimeError,
                    > {
                        let mut branch_params: HashMap<AtomId, AtomId> = Default::default();
                        let mut partial_method = convert_into_chronicle(graph, *scope)?;
                        let method = &mut partial_method.chronicle;
                        for v in &method.get_variables() {
                            if let Some(interval) = ch.sym_table.get_scope(v) {
                                //It means the variable has been created before the method, and shall be transformed into a parameter
                                if interval.get_start()
                                    < graph.get_scope_interval(scope).get_start()
                                {
                                    let param = ch.sym_table.new_parameter(
                                        ch.sym_table.get_atom(v, false).unwrap().to_string(),
                                        ch.sym_table.get_type_of(v),
                                    );
                                    method.replace(v, &param);
                                    branch_params.insert(*v, param);
                                }
                            }
                        }
                        let cond = ch.sym_table.new_parameter(COND, AtomType::Bool);
                        method.add_constraint(Constraint::eq(cond, ch.sym_table.new_bool(branch)));
                        method.set_task(vec![t_if, cond]);
                        method.set_name(vec![label, cond]);
                        Ok((partial_method, branch_params))
                    };

                    let (partial_method_true, true_params) =
                        partially_convert_branch(&if_block.true_branch, true, m_true)?;
                    let (partial_method_false, false_params) =
                        partially_convert_branch(&if_block.false_branch, true, m_false)?;

                    let true_params_id: HashSet<AtomId> = true_params.keys().cloned().collect();
                    let false_params_id: HashSet<AtomId> = false_params.keys().cloned().collect();
                    let mut task_params: HashSet<AtomId> =
                        true_params_id.union(&false_params_id).cloned().collect();

                    let mut task_params: Vec<AtomId> = task_params.drain().collect();
                    let cond_if = ch.sym_table.new_parameter(COND, AtomType::Bool);

                    let mut task = vec![t_if, cond_if];
                    for p in &task_params {
                        task.push(ch.sym_table.new_parameter(
                            ch.sym_table.get_atom(p, false).unwrap().to_string(),
                            ch.sym_table.get_type_of(p),
                        ));
                    }

                    let mut modify_and_convert_branch =
                        |mut partial: PartialConversion,
                         method_params: HashMap<AtomId, AtomId>|
                         -> Result<ChronicleTemplate, LRuntimeError> {
                            let method = &mut partial.chronicle;
                            for p in &task_params {
                                let id = match method_params.get(p) {
                                    None => {
                                        let id = ch.sym_table.new_parameter(
                                            ch.sym_table.get_atom(p, false).unwrap().to_string(),
                                            ch.sym_table.get_type_of(p),
                                        );
                                        method.add_var(&id);
                                        id
                                    }
                                    Some(id) => *id,
                                };
                                method.add_task_parameter(&id);
                                method.add_method_parameter(&id);
                            }

                            post_processing(partial)
                        };

                    let method_true = modify_and_convert_branch(partial_method_true, true_params)?;
                    let method_false =
                        modify_and_convert_branch(partial_method_false, false_params)?;

                    let task = TaskTemplate {
                        name: task,
                        methods: vec![method_true, method_false],
                    };
                    ch.add_task_template(task);

                    let mut task = vec![t_if, if_block.cond];
                    task.append(&mut task_params);
                    ch.add_subtask(SubTask {
                        interval: vertice.interval,
                        lit: task.into(),
                        result: vertice.result,
                    })
                }
            },
            Expression::Handle(h) => {
                let handle = graph.handles.get(h).unwrap();
                queue.push(handle.scope.start);
                ch.add_constraint(Constraint::eq(
                    vertice.interval.get_start(),
                    graph.get_scope_interval(&handle.scope).get_start(),
                ));
                ch.add_constraint(Constraint::eq(vertice.result, h));
            }
            Expression::Await(a) => {
                awaits.push(Await {
                    result: vertice.result,
                    handle: *a,
                });
                //ch.add_constraint(Constraint::eq(vertice.result, Constraint::Await(a.into())))
            }
        }
        if let Some(parent) = vertice.parent {
            ch.add_constraint(Constraint::eq(
                graph.get_interval(&parent).get_end(),
                vertice.interval.get_start(),
            ))
        }
        if let Some(child) = vertice.child {
            queue.push(child)
        }
    }

    ch.debug.flow_graph = graph.clone();

    //post_processing(&mut ch)?;

    Ok(PartialConversion {
        chronicle: ch,
        awaits,
        handles,
    })
}

pub fn post_processing(partial: PartialConversion) -> Result<ChronicleTemplate, LRuntimeError> {
    let (mut ch, awaits, mut handles) = (partial.chronicle, partial.awaits, partial.handles);

    /*
    HANDLES
     */

    unify_equal(&mut ch);

    /*
     * GET END OF SCOPE FOR VARIABLES CONTAINING HANDLES
     */

    for v in &ch.variables {
        let p = ch.sym_table.get_parent(v);
        if ch.sym_table.get_type_of(&p) == AtomType::Handle {
            match ch.sym_table.get_end(v) {
                None => {}
                Some(end) => handles.get_mut(&p).unwrap().add_end(&end),
            }
        }
    }

    for a in &awaits {
        let handle = ch.sym_table.get_parent(&a.handle);
        if ch.sym_table.get_type_of(&handle) == AtomType::Handle {
            let handle = handles.get(&handle).unwrap();
            ch.add_constraint(Constraint::eq(a.result, handle.result));
            let start = ch.sym_table.get_start(&a.result).unwrap();
            let end_async = ch
                .debug
                .flow_graph
                .get(handle.scope.get_end())
                .unwrap()
                .get_end();
            ch.add_constraint(Constraint::leq(end_async, start));
        } else {
            Err(LRuntimeError::new(
                "convert_into_chronicle",
                "await arg is not a handle",
            ))?
        }
    }

    for (_, handle) in handles.inner() {
        let vec: Vec<Lit> = handle.ends.iter().map(|e| Lit::from(e)).collect();
        let end_async = ch
            .debug
            .flow_graph
            .get(handle.scope.get_end())
            .unwrap()
            .get_end();
        ch.add_constraint(Constraint::leq(end_async, Constraint::Max(vec.into())));
    }

    /*
    POST PROCESSING
     */

    simplify_timepoints(&mut ch)?;
    rm_useless_var(&mut ch);
    //merge_conditions(c, context, ch)?;
    //simplify_constraints(c, context, ch)?;

    Ok(ch)
}
