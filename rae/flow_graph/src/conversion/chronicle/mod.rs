use crate::structs::chronicle::condition::Condition;
use crate::structs::chronicle::constraint::Constraint;
use crate::structs::chronicle::effect::Effect;
use crate::structs::chronicle::subtask::SubTask;
use crate::structs::chronicle::task_template::TaskTemplate;
use crate::structs::chronicle::template::{ChronicleKind, ChronicleTemplate};
use crate::structs::chronicle::{GetVariables, Replace};
use crate::structs::domain::root_type::RootType::Boolean;
use crate::structs::flow_graph::flow::{FlowId, FlowKind};
use crate::structs::flow_graph::graph::FlowGraph;
use crate::structs::sym_table::lit::Lit;
use crate::structs::sym_table::{VarId, COND};
use itertools::Itertools;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::{HashMap, HashSet};
use std::ops::Deref;

pub mod post_processing;

pub fn convert_method(
    fl: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    convert_into_chronicle(fl, flow)
    //let partial = convert_into_chronicle(graph, flow)?;

    //post_processing(partial)
}

pub fn convert_into_chronicle(
    fl: &mut FlowGraph,
    flow: &FlowId,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let mut st = fl.sym_table.clone();

    let mut ch = ChronicleTemplate::new("template", ChronicleKind::Method, fl.sym_table.clone());

    ch.add_constraint(Constraint::leq(
        ch.get_interval().get_start(),
        ch.get_interval().get_end(),
    ));

    //Bind the flow start timepoint with the chronicle start timepoint
    st.union_var(
        ch.get_interval().get_start(),
        fl.get_flow_interval(flow).get_start(),
    );

    //Bind the flow end timepoint with the chronicle end timepoint
    st.union_var(
        ch.get_interval().get_end(),
        fl.get_flow_interval(flow).get_end(),
    );

    st.union_var(&ch.get_result(), &fl.get_flow_result(flow));

    let mut queue = vec![*flow];

    while let Some(flow_id) = queue.pop() {
        let flow = fl.flows[flow_id].clone();
        let interval = fl.get_flow_interval(&flow_id);
        let result = fl.get_flow_result(&flow_id);

        match flow.kind {
            FlowKind::Assignment(ass) => match &ass.lit {
                Lit::Exp(_) => {}
                Lit::Atom(_) => {}
                Lit::Await(a) => {
                    let handle = fl.get_handle(a).unwrap();

                    ch.add_constraint(Constraint::leq(
                        fl.get_flow_end(&handle),
                        interval.get_end(),
                    ));
                    //ch.add_constraint(Constraint::eq(fl.get_flow_result(handle), result));
                    st.union_var(&fl.get_flow_result(&handle), &result);
                }
                Lit::Constraint(c) => ch.add_constraint(Constraint::eq(result, c.deref())),
                Lit::Apply(app) => {}
                Lit::Read(read) => {
                    let condition = Condition {
                        interval,
                        sv: read.clone(),
                        value: result,
                    };

                    ch.add_condition(condition);
                }
                Lit::Write(write) => {
                    let sv = write[0..write.len() - 1].to_vec();
                    let value = *write.last().unwrap();
                    let effect = Effect {
                        interval,
                        sv,
                        value,
                    };

                    ch.add_effect(effect);

                    let result = ch.sym_table.new_nil();
                    st.union_var(&result, &result);
                }
                Lit::Exec(exec) => {
                    let subtask = SubTask {
                        interval,
                        lit: exec.into(),
                        result,
                    };

                    ch.add_subtask(subtask);
                    /*let subtask_result = ch.sym_table.new_nil();
                    st.union_atom(&subtask_result, &result);*/
                }
            },
            FlowKind::Seq(mut seq, _) => queue.append(&mut seq),
            FlowKind::Branching(branching) => {
                let cond = fl.get_flow_result(&branching.cond);
                let cond_domain = st.get_domain_of_var(&cond);
                if cond_domain.is_true() {
                    queue.push(branching.true_flow)
                } else if cond_domain.is_false() {
                    queue.push(branching.false_flow)
                } else {
                    let (t_if, m_true, m_false) = ch.sym_table.new_if();

                    /*
                    Partially converts the a branch of 'if', getting by the mean time the variables necessary in computation of the branch that have been defined previously.
                    It also returns the symbol of cond, as it is necessary
                     */
                    let mut convert_branch = |flow: &FlowId,
                                              branch: bool,
                                              label: VarId|
                     -> Result<
                        (ChronicleTemplate, HashMap<VarId, VarId>),
                        LRuntimeError,
                    > {
                        let mut branch_params: HashMap<VarId, VarId> = Default::default();
                        let mut method = convert_into_chronicle(fl, flow)?;
                        for v in &method.get_variables() {
                            if let Some(declaration) = st.get_declaration(v) {
                                //It means the variable has been created before the method, and shall be transformed into a parameter
                                if declaration < fl.get_flow_start(&flow_id) {
                                    let param = st.new_parameter(
                                        st.get_label(v, false),
                                        st.get_domain_of_var(v),
                                    );
                                    st.union_domain(
                                        &st.get_domain_id(&param),
                                        &st.get_domain_id(&v),
                                    );
                                    method.replace(v, &param);
                                    branch_params.insert(*v, param);
                                }
                            }
                        }
                        let cond = ch.sym_table.new_parameter(COND, branch);
                        method.set_task(vec![t_if, cond]);
                        method.set_name(vec![label, cond]);
                        Ok((method, branch_params))
                    };

                    let (method_true, true_params) =
                        convert_branch(&branching.true_flow, true, m_true)?;
                    let (method_false, false_params) =
                        convert_branch(&branching.false_flow, false, m_false)?;

                    let true_params_id: HashSet<VarId> = true_params.keys().cloned().collect();
                    let false_params_id: HashSet<VarId> = false_params.keys().cloned().collect();
                    let mut task_params: HashSet<VarId> =
                        true_params_id.union(&false_params_id).cloned().collect();

                    let mut task_params: Vec<VarId> = task_params.drain().collect();
                    let cond_if = ch.sym_table.new_parameter(COND, Boolean);

                    let mut task = vec![t_if, cond_if];
                    for p in &task_params {
                        let p = st.get_var_parent(p);
                        let id = ch.sym_table.new_parameter(
                            ch.sym_table.get_label(&p, false),
                            ch.sym_table.get_domain_of_var(&p),
                        );
                        task.push(id);
                    }

                    /*
                    CREATE EXPRESSION FOR METHOD:
                    - create a new parameter if the parameter is not present in the method,
                    but needed by the other method of the synthetic task
                     */
                    let mut modify_and_convert_branch =
                        |mut method: ChronicleTemplate,
                         method_params: HashMap<VarId, VarId>|
                         -> Result<ChronicleTemplate, LRuntimeError> {
                            for p in &task_params {
                                let id = match method_params.get(p) {
                                    None => {
                                        let id = ch.sym_table.new_parameter(
                                            ch.sym_table.get_label(&p, false),
                                            ch.sym_table.get_domain_of_var(p),
                                        );
                                        method.add_var(&id);
                                        id
                                    }
                                    Some(id) => *id,
                                };
                                st.union_domain(&st.get_domain_id(&p), &st.get_domain_id(&id));
                                method.add_task_parameter(&id);
                                method.add_method_parameter(&id);
                            }
                            //We enforce that the return types are the same
                            /*let m_result = *method.get_result();
                            st.union_domain(
                                &st.get_domain_id(&m_result),
                                &st.get_domain_id(&vertice.result),
                            );*/
                            /*println!(
                                "union type(id)s {} and {}",
                                ch.sym_table.get_type_id_of(&m_result),
                                ch.sym_table.get_type_id_of(&vertice.result)
                            );*/
                            Ok(method)
                        };

                    let method_true = modify_and_convert_branch(method_true, true_params)?;
                    let method_false = modify_and_convert_branch(method_false, false_params)?;

                    let task = TaskTemplate {
                        name: task,
                        methods: vec![method_true, method_false],
                    };
                    ch.add_task_template(task);

                    let mut task = vec![t_if, cond];
                    task.append(&mut task_params);
                    ch.add_subtask(SubTask {
                        interval,
                        lit: task.into(),
                        result,
                    })
                }
            }
            FlowKind::FlowResult(_) => {
                unreachable!()
            }
            FlowKind::FlowAsync(fa) => {
                let h = fa.flow;
                let result = fa.result;
                queue.push(h);
                ch.add_constraint(Constraint::leq(fa.timepoint, fl.get_flow_start(&h)));
                //sym_table.union_atom(&ass.get_start(), &fl.get_flow_start(&h));
                let domain_id = st.get_domain_id(&result);
                let drops: Vec<Lit> = st
                    .get_domain_vars(&domain_id)
                    .drain(..)
                    .map(|a| st.get_var_parent(&a).into())
                    .unique()
                    .collect();
                ch.add_constraint(Constraint::leq(fl.get_flow_end(&h), Constraint::Max(drops)))
            }
        }
    }

    fl.flat_bindings();

    ch.flat_bindings();

    Ok(ch)
}
