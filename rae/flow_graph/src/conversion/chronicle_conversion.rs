use crate::conversion::chronicle_post_processing::{
    post_processing, rm_useless_var, simplify_timepoints, unify_equal,
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
use crate::structs::flow_graph::scope::Scope;
use crate::FlowGraph;
use sompas_structs::lruntimeerror::LRuntimeError;

pub struct Await {
    result: AtomId,
    handle: AtomId,
}

pub fn convert_into_chronicle(
    graph: &FlowGraph,
    scope: Scope,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut awaits: Vec<Await> = vec![];

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, graph.sym_table.clone());

    let mut queue = vec![scope.start];

    let mut handles = graph.handles.clone();

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
                };

                ch.add_subtask(subtask);
                let result = ch.sym_table.new_bool(false);
                ch.add_constraint(Constraint::eq(vertice.get_result(), result));
            }
            Expression::Err(_) => {}
            Expression::Block(block) => match block {
                Block::If(if_block) => {
                    let mut params = vec![];
                    let (t_if, m_true, m_false) = ch.sym_table.new_if();
                    let cond_if = ch.sym_table.new_parameter(COND, AtomType::Bool);

                    let mut method_true = convert_into_chronicle(graph, if_block.true_branch)?;
                    for v in &method_true.get_variables() {
                        if let Some(interval) = ch.sym_table.get_scope(v) {
                            //It means the variable has been created before the method, and shall be transformed into a parameter
                            if interval.get_start() <= method_true.get_interval().get_start() {
                                let param = ch.sym_table.new_parameter(
                                    ch.sym_table.get_atom(v, false).unwrap().to_string(),
                                    ch.sym_table.get_type_of(v),
                                );
                                method_true.replace(v, &param);
                                params.push(*v);
                            }
                        }
                    }
                    let cond_true = ch.sym_table.new_parameter(COND, AtomType::Bool);
                    method_true
                        .add_constraint(Constraint::eq(cond_true, ch.sym_table.new_bool(true)));

                    let mut method_false = convert_into_chronicle(graph, if_block.false_branch)?;
                    let cond_false = ch.sym_table.new_parameter(COND, AtomType::Bool);
                    method_false
                        .add_constraint(Constraint::eq(cond_false, ch.sym_table.new_bool(false)));

                    let vars_method_true = method_true.get_variables();
                    let vars_method_false = method_false.get_variables();

                    let vars_task = vars_method_true.union(vars_method_false);

                    for var in &vars_task {
                        method_true.add_var(var);
                        method_false.add_var(var)
                    }

                    method_true.set_name(vec![m_true, cond_true]);
                    method_true
                        .set_task(vec![t_if, ch.sym_table.new_parameter(COND, AtomType::Bool)]);
                    method_false.set_name(vec![m_false, cond_false]);
                    method_false
                        .set_task(vec![t_if, ch.sym_table.new_parameter(COND, AtomType::Bool)]);

                    let mut task = vec![t_if, cond_if];
                    task.append(&mut params);

                    let task = TaskTemplate {
                        name: task,
                        methods: vec![method_true, method_false],
                    };
                    ch.add_task_template(task);

                    let cond = ch.sym_table.new_parameter(COND, AtomType::Bool);

                    ch.add_subtask(SubTask {
                        interval: vertice.interval,
                        lit: vec![t_if, cond].into(),
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

    /*
    HANDLES
     */

    /*
    POST PROCESSING
     */
    // unify_equal(&mut ch);

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
            let end_async = graph.get(handle.scope.get_end()).unwrap().get_end();
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
        let end_async = graph.get(handle.scope.get_end()).unwrap().get_end();
        ch.add_constraint(Constraint::leq(end_async, Constraint::Max(vec.into())));
    }

    //simplify_timepoints(&mut ch)?;
    //rm_useless_var(&mut ch);
    /*println!(
        "before merge conditions: {}",
        c.format_with_sym_table(&ch.sym_table, true)
    );*/
    //merge_conditions(c, context, ch)?;
    //simplify_constraints(c, context, ch)?;
    //c.format_with_parent(&ch.get_mut_sym_table());

    ch.debug.flow_graph = graph.clone();

    //post_processing(&mut ch)?;

    Ok(ch)
}
