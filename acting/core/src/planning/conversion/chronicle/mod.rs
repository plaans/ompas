use crate::model::chronicle::acting_process_model::{
    AcquireModel, ActionModel, ArbitraryModel, ReleaseModel, ResourceModel,
};
use crate::model::chronicle::computation::Computation;
use crate::model::chronicle::condition::Condition;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::effect::Effect;
use crate::model::chronicle::interval::Interval;
use crate::model::chronicle::lit::{Lit, LitSet};
use crate::model::chronicle::subtask::SubTask;
use crate::model::chronicle::task_template::TaskTemplate;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::sym_domain::basic_type::BasicType::Boolean;
use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#trait::FormatWithSymTable;
use crate::model::sym_table::r#trait::{GetVariables, Replace};
use crate::model::sym_table::VarId;
use crate::planning::conversion::flow_graph::flow::{FlowId, FlowKind};
use crate::planning::conversion::flow_graph::graph::FlowGraph;
use crate::planning::conversion::ConvertParameters;
use function_name::named;
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::sym_table::{COND, EPSILON};
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Deref;

pub mod post_processing;

#[derive(Default, Clone)]
pub struct HandleTable {
    asyncs: HashMap<FlowId, AsyncHandle>,
    resources: HashMap<VarId, ResourceHandle>,
}

#[derive(Default, Clone)]
pub struct AsyncHandle {
    drops: HashSet<VarId>,
    awaits: HashSet<VarId>,
}

#[derive(Default, Clone)]
pub struct ResourceHandle {
    drops: HashSet<VarId>,
    releases: HashSet<VarId>,
}

impl HandleTable {
    pub fn add_handle(&mut self, flow: FlowId) {
        self.asyncs.insert(
            flow,
            AsyncHandle {
                drops: Default::default(),
                awaits: Default::default(),
            },
        );
    }

    pub fn add_resource_handle(&mut self, var_id: VarId) {
        self.resources.insert(
            var_id,
            ResourceHandle {
                drops: Default::default(),
                releases: Default::default(),
            },
        );
    }

    pub fn add_async_drop(&mut self, flow: &FlowId, drop: VarId) {
        if let Some(handle) = self.asyncs.get_mut(flow) {
            handle.drops.insert(drop);
        } else {
            panic!("");
        }
    }

    pub fn add_resource_drop(&mut self, var_id: &VarId, drop: VarId) {
        if let Some(handle) = self.resources.get_mut(var_id) {
            handle.drops.insert(drop);
        } else {
            panic!("");
        }
    }

    pub fn add_await(&mut self, flow: &FlowId, r#await: VarId) {
        if let Some(handle) = self.asyncs.get_mut(flow) {
            handle.awaits.insert(r#await);
        } else {
            panic!("");
        }
    }

    pub fn add_release(&mut self, var_id: &VarId, release: VarId) {
        if let Some(handle) = self.resources.get_mut(var_id) {
            handle.releases.insert(release);
        } else {
            panic!("");
        }
    }
}

pub fn convert_graph(
    ch: Option<Chronicle>,
    fl: &mut FlowGraph,
    flow: &FlowId,
    env: &LEnv,
    cv: &ConvertParameters,
) -> Result<Chronicle, LRuntimeError> {
    let ht = &mut HandleTable::default();

    let mut ch = convert_into_chronicle(ch, ht, fl, flow, env, cv)?;

    for (flow, handle) in &ht.asyncs {
        let drops: Vec<VarId> = handle
            .drops
            .iter()
            .map(|v| ch.st.get_var_parent(v))
            .collect();
        let t_drop = match drops.len() {
            0 => panic!(),
            1 => *drops.first().unwrap(),
            _ => {
                let t_drop = fl.st.new_timepoint();
                ch.add_constraint(Constraint::eq(t_drop, Constraint::max(drops)));
                t_drop
            }
        };
        let mut awaits = handle.awaits.clone();
        awaits.insert(t_drop);
        let awaits: Vec<VarId> = awaits.drain().map(|v| ch.st.get_var_parent(&v)).collect();
        let lit: Option<Lit> = match awaits.len() {
            0 => None,
            1 => Some(awaits.first().unwrap().into()),
            _ => Some(Constraint::min(awaits).into()),
        };

        if let Some(lit) = lit {
            let end = fl.st.get_var_parent(&fl.get_flow_end(flow));
            ch.add_constraint(Constraint::leq(end, lit));
        }
    }

    for (release_time, handle) in &ht.resources {
        let drops: Vec<VarId> = handle
            .drops
            .iter()
            .map(|v| ch.st.get_var_parent(v))
            .collect();
        let t_drop = match drops.len() {
            0 => panic!(),
            1 => *drops.first().unwrap(),
            _ => {
                let t_drop = fl.st.new_timepoint();
                ch.add_constraint(Constraint::eq(t_drop, Constraint::max(drops)));
                t_drop
            }
        };
        let mut releases = handle.releases.clone();
        releases.insert(t_drop);
        let releases: Vec<VarId> = releases.drain().map(|v| ch.st.get_var_parent(&v)).collect();
        let lit: Option<Lit> = match releases.len() {
            0 => None,
            1 => Some(releases.first().unwrap().into()),
            _ => Some(Constraint::min(releases).into()),
        };

        if let Some(lit) = lit {
            ch.add_constraint(Constraint::eq(release_time, lit));
        }
    }

    Ok(ch)
}

#[named]
pub fn convert_into_chronicle(
    ch: Option<Chronicle>,
    ht: &mut HandleTable,
    fl: &mut FlowGraph,
    flow: &FlowId,
    _env: &LEnv,
    cv: &ConvertParameters,
) -> Result<Chronicle, LRuntimeError> {
    let st = fl.st.clone();

    let mut ch = ch.unwrap_or(Chronicle::new(
        "template",
        ChronicleKind::Method,
        fl.st.clone(),
    ));

    ch.add_constraint(Constraint::leq(
        ch.get_interval().get_start(),
        ch.get_interval().get_end(),
    ));

    //Bind the flow start timepoint with the chronicle start timepoint
    st.union_var(
        &ch.get_interval().get_start(),
        &fl.get_flow_interval(flow).get_start(),
    );

    //Bind the flow end timepoint with the chronicle end timepoint
    st.union_var(
        &ch.get_interval().get_end(),
        &fl.get_flow_interval(flow).get_end(),
    );

    st.union_var(ch.get_result(), &fl.get_flow_result(flow));

    let mut queue = VecDeque::new();
    queue.push_back(*flow);

    while let Some(flow_id) = queue.pop_front() {
        let flow = fl.flows[flow_id].clone();
        let interval = fl.get_flow_interval(&flow_id);
        let result = fl.get_flow_result(&flow_id);
        let start = interval.get_start();
        let end = interval.get_end();
        let duration = interval.get_duration();

        if start != end {
            if let Some(duration) = duration {
                /*if OMPAS_CHRONICLE_DEBUG_ON.get() >= ChronicleDebug::On {
                    println!("duration encoded");
                }*/
                ch.add_constraint(Constraint::eq(end, Computation::add(vec![start, duration])));
            } else {
                ch.add_constraint(Constraint::leq(start, end))
            }
        }

        match flow.kind {
            FlowKind::Lit(lit) => {
                match &lit {
                    Lit::Exp(_) => {}
                    Lit::Atom(a) => ch.add_constraint(Constraint::eq(result, a)),
                    Lit::Await(a) => {
                        let handle = fl.get_handle(a).unwrap();
                        ht.add_await(handle, interval.get_end());

                        /*ch.add_constraint(Constraint::leq(
                            fl.get_flow_end(&handle),
                            interval.get_end(),
                        ));*/
                        st.union_var(&fl.get_flow_result(handle), &result);
                    }
                    Lit::Acquire(acq) => {
                        let quantity_symbol = st.new_symbol(QUANTITY);
                        let max_q_symbol = st.new_symbol(MAX_Q);
                        let epsilon = st.new_symbol(EPSILON);
                        let resource = acq.resource;
                        let t_prime = st.new_timepoint();
                        let t_release_prime = st.new_timepoint();
                        let t_release = acq.release_time;

                        //println!("convert_into_chronicle/max_capacity = {}", cv.max_capacity);
                        let quantity_domain: Domain = Domain::IntRange(
                            0,
                            cv.max_capacity, //Bound::Inc(Cst::Int(MAX_QUANTITY_VALUE)),
                        );

                        let max_q_result = st.new_result();
                        st.set_domain(&st.get_domain_id(&max_q_result), quantity_domain.clone());

                        let max_q_result_2 = st.new_result();
                        st.set_domain(&st.get_domain_id(&max_q_result_2), quantity_domain.clone());

                        //let int_domain: Domain = Domain::Simple(TYPE_ID_INT);

                        let new_q_acquire = st.new_result();
                        st.set_domain(&st.get_domain_id(&new_q_acquire), quantity_domain.clone());
                        let new_q_release = st.new_result();
                        st.set_domain(&st.get_domain_id(&new_q_release), quantity_domain.clone());
                        let current_release_quantity = st.new_result();
                        st.set_domain(
                            &st.get_domain_id(&current_release_quantity),
                            quantity_domain.clone(),
                        );
                        let current_quantity = st.new_result();
                        st.set_domain(
                            &st.get_domain_id(&current_quantity),
                            quantity_domain.clone(),
                        );

                        ht.add_resource_handle(t_release);

                        let domain_id = st.get_domain_id(&result);

                        let drops: Vec<VarId> = st
                            .get_domain_vars(&domain_id)
                            .drain(..)
                            .filter_map(|a| st.get_drop(&st.get_var_parent(&a)))
                            .collect();

                        for drop in drops {
                            ht.add_resource_drop(&t_release, drop)
                        }

                        let mut acquire = AcquireModel::default();
                        let mut release = ReleaseModel::default();

                        //Add all conditions
                        // Current quantity

                        acquire.conditions.push(Condition {
                            interval: Interval::new_instantaneous(t_prime),
                            sv: vec![quantity_symbol, resource],
                            value: current_quantity,
                        });

                        acquire.conditions.push(Condition {
                            interval: Interval::new_instantaneous(interval.get_start()),
                            sv: vec![max_q_symbol, resource],
                            value: max_q_result,
                        });

                        let quantity = if let Some(capacity) = acq.capacity {
                            capacity
                        } else {
                            max_q_result
                        };

                        //Add all constraints on acquisition and release of constraint
                        acquire
                            .constraints
                            .push(Constraint::leq(interval.get_start(), t_prime));
                        acquire.constraints.push(Constraint::eq(
                            interval.get_end(),
                            Computation::add(vec![t_prime, epsilon]),
                        ));
                        acquire.constraints.push(Constraint::eq(
                            new_q_acquire,
                            Computation::sub(vec![current_quantity, quantity]),
                        ));
                        acquire
                            .constraints
                            .push(Constraint::leq(st.new_int(0), new_q_acquire));
                        acquire
                            .constraints
                            .push(Constraint::leq(new_q_acquire, max_q_result));

                        acquire.effects.push(Effect {
                            interval: Interval::new(t_prime, interval.get_end()),
                            sv: vec![quantity_symbol, resource],
                            value: new_q_acquire,
                        });

                        release.constraints.push(Constraint::eq(
                            t_release_prime,
                            Computation::add(vec![t_release, epsilon]),
                        ));

                        release.conditions.push(Condition {
                            interval: Interval::new_instantaneous(t_release),
                            sv: vec![quantity_symbol, resource],
                            value: current_release_quantity,
                        });

                        release.conditions.push(Condition {
                            interval: Interval::new_instantaneous(t_release),
                            sv: vec![max_q_symbol, resource],
                            value: max_q_result_2,
                        });

                        release.constraints.push(Constraint::eq(
                            new_q_release,
                            Computation::add(vec![current_release_quantity, quantity]),
                        ));

                        release
                            .constraints
                            .push(Constraint::leq(interval.get_end(), t_release));

                        release
                            .constraints
                            .push(Constraint::leq(st.new_int(0), new_q_release));

                        release
                            .constraints
                            .push(Constraint::leq(new_q_release, max_q_result_2));

                        release.effects.push(Effect {
                            interval: Interval::new(t_release, t_release_prime),
                            sv: vec![quantity_symbol, resource],
                            value: new_q_release,
                        });

                        if let Some(label) = flow.label {
                            ch.acting_process_models.add_binding(
                                label,
                                ResourceModel::new(
                                    resource,
                                    quantity,
                                    start,
                                    Interval::new(end, t_release),
                                    acquire,
                                    release,
                                ),
                            )
                        } else {
                            ch.absorb_acquire_model(acquire);
                            ch.absorb_acquire_model(release);
                        }
                    }
                    Lit::Release(release) => {
                        let handle = fl.get_resource_handle(release).unwrap();
                        ht.add_release(handle, interval.get_end());
                    }
                    Lit::Constraint(c) => match c.deref() {
                        Constraint::Arbitrary(set) => {
                            let constraint = match set {
                                LitSet::Finite(set) => {
                                    let mut constraints = vec![];
                                    for e in set {
                                        constraints.push(Constraint::eq(result, e))
                                    }
                                    if constraints.len() > 1 {
                                        Some(Constraint::or(constraints))
                                    } else {
                                        Some(constraints.remove(0))
                                    }
                                }
                                LitSet::Domain(d) => {
                                    let id = st.new_parameter("_arbitrary_", Domain::any());
                                    let r#type: String = d.format(&st, true);
                                    let domain =
                                        st.get_type_as_domain(&r#type).ok_or_else(|| {
                                            LRuntimeError::new(
                                                function_name!(),
                                                format!("{} is not a defined type", r#type),
                                            )
                                        })?;
                                    st.meet_to_domain(&st.get_domain_id(&result), domain);
                                    st.union_var(&id, &result);
                                    None
                                }
                            };
                            if let Some(label) = flow.label {
                                ch.acting_process_models.add_binding(
                                    label,
                                    ArbitraryModel::new(
                                        flow.interval.get_start(),
                                        flow.result,
                                        constraint,
                                    ),
                                );
                            } else if let Some(constraint) = constraint {
                                ch.add_constraint(constraint)
                            }
                        }
                        _ => ch.add_constraint(Constraint::eq(result, c.deref())),
                    },
                    Lit::Computation(c) => ch.add_constraint(Constraint::eq(result, c.deref())),
                    Lit::Apply(_) => ch.add_constraint(Constraint::eq(result, lit)),
                    Lit::Read(read) => {
                        let condition = Condition {
                            interval,
                            sv: read.clone(),
                            value: result,
                        };

                        let mut args = read[1..].to_vec();
                        args.push(result);

                        let sf = st.get_var_parent(&read[0]);
                        let d = st.get_domain_of_var(&sf);
                        if let Domain::Cst(t, _) = d {
                            if let Domain::Application(_, types, r) = t.deref() {
                                //println!("setting types");
                                let mut types = types.clone();
                                types.push(*r.clone());
                                for (i, (f, t)) in args.iter().zip(types).enumerate() {
                                    let r = fl.st.get_domain_id(f);
                                    let domain_debug = st.get_domain(&r);
                                    if !fl.st.meet_to_domain(&r, t.clone()).is_none() {
                                        panic!("Error checking domain of {} which is not compatible with arg {} of sf {}: expected {}, got {}", f.format(&st, true), i, sf.format(&st, true), st.format_domain(&t), st.format_domain(&domain_debug))
                                    };
                                }
                            }
                        }

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

                        let args = write[1..].to_vec();
                        let sf = st.get_var_parent(&write[0]);
                        let d = st.get_domain_of_var(&sf);
                        if let Domain::Cst(t, _) = d {
                            if let Domain::Application(_, types, r) = t.deref() {
                                //println!("setting types");
                                let mut types = types.clone();
                                types.push(*r.clone());
                                for (i, (f, t)) in args.iter().zip(types).enumerate() {
                                    let r = fl.st.get_domain_id(f);
                                    let domain_debug = st.get_domain(&r);
                                    if !fl.st.meet_to_domain(&r, t.clone()).is_none() {
                                        panic!("Error checking domain of {} which is not compatible with arg {} of sf {}: expected {}, got {}", f.format(&st, true), i, sf.format(&st, true),st.format_domain(&t),st.format_domain(&domain_debug) )
                                    };
                                }
                            }
                        }

                        ch.add_effect(effect);
                        if duration.is_none() {
                            let eps = st.new_symbol(EPSILON);
                            ch.add_constraint(Constraint::eq(
                                interval.get_end(),
                                Computation::add(vec![interval.get_start(), eps]),
                            ));
                        }

                        let result = ch.st.new_nil();
                        st.union_var(&result, &result);
                    }
                    Lit::Exec(exec) => {
                        let subtask = SubTask {
                            interval,
                            name: exec.clone(),
                            result,
                            label: flow.label,
                        };

                        let mut args = exec[1..].to_vec();
                        args.push(result);
                        let task = exec[0];
                        let d = st.get_domain_of_var(&task);
                        if let Domain::Cst(t, _) = d {
                            if let Domain::Application(_, types, r) = t.deref() {
                                //println!("setting types");
                                let mut types = types.clone();
                                types.push(*r.clone());
                                for (f, t) in args.iter().zip(types) {
                                    let r = fl.st.get_domain_id(f);
                                    let t_debug = t.clone();
                                    if !fl.st.meet_to_domain(&r, t).is_none() {
                                        panic!(
                                            "{}: incompatible variable domains: {} not {}",
                                            exec.format(&st, true),
                                            (*f).format(&st, true),
                                            st.format_domain(&t_debug),
                                        )
                                    };
                                }
                            }
                        }
                        if let Some(label) = flow.label {
                            ch.acting_process_models
                                .add_binding(label, ActionModel::new(subtask, vec![]));
                        } else {
                            ch.add_subtask(subtask);
                        }

                        /*let subtask_result = ch.sym_table.new_nil();
                        st.union_atom(&subtask_result, &result);*/
                    }
                    Lit::Set(_) => panic!("set not supported yet"),
                }
            }
            FlowKind::Seq(seq) => {
                let mut precedent = start;
                for f in &seq {
                    let start = fl.get_flow_start(f);
                    if start != precedent {
                        ch.add_constraint(Constraint::leq(precedent, start));
                    }
                    precedent = fl.get_flow_end(f);
                }

                queue.append(&mut VecDeque::from(seq))
            }
            FlowKind::Branching(branching) => {
                let cond = fl.get_flow_result(&branching.cond_flow);
                let cond_domain = st.get_domain_of_var(&cond);
                if cond_domain.is_true() {
                    st.union_var(
                        &fl.get_flow_end(&branching.cond_flow),
                        &fl.get_flow_start(&branching.true_flow),
                    );
                    st.union_var(&fl.get_flow_end(&branching.true_flow), &end);
                    st.union_var(&fl.get_flow_result(&branching.true_flow), &result);
                    queue.push_back(branching.true_flow)
                } else if cond_domain.is_false() {
                    st.union_var(
                        &fl.get_flow_end(&branching.cond_flow),
                        &fl.get_flow_start(&branching.false_flow),
                    );
                    st.union_var(&fl.get_flow_end(&branching.false_flow), &end);
                    st.union_var(&fl.get_flow_result(&branching.false_flow), &result);
                    queue.push_back(branching.false_flow)
                } else {
                    let (t_if, m_true, m_false) = ch.st.new_if();

                    /*
                    Partially converts the a branch of 'if', getting by the mean time the variables necessary in computation of the branch that have been defined previously.
                    It also returns the symbol of cond, as it is necessary
                     */
                    let mut convert_branch = |flow: &FlowId,
                                              branch: bool,
                                              label: VarId|
                     -> Result<
                        (Chronicle, HashMap<VarId, VarId>),
                        LRuntimeError,
                    > {
                        let mut branch_params: HashMap<VarId, VarId> = Default::default();
                        let mut method = convert_into_chronicle(None, ht, fl, flow, _env, cv)?;
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
                                        &st.get_domain_id(v),
                                    );
                                    method.replace(v, &param);
                                    branch_params.insert(*v, param);
                                }
                            }
                        }
                        let cond = ch.st.new_parameter(COND, branch);
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
                    let cond_if = ch.st.new_parameter(COND, Boolean);

                    let mut task = vec![t_if, cond_if];
                    for p in &task_params {
                        let p = st.get_var_parent(p);
                        let id = ch
                            .st
                            .new_parameter(ch.st.get_label(&p, false), ch.st.get_domain_of_var(&p));
                        task.push(id);
                    }

                    /*
                    CREATE EXPRESSION FOR METHOD:
                    - create a new parameter if the parameter is not present in the method,
                    but needed by the other method of the synthetic task
                     */
                    let modify_and_convert_branch =
                        |mut method: Chronicle,
                         method_params: HashMap<VarId, VarId>|
                         -> Result<Chronicle, LRuntimeError> {
                            for p in &task_params {
                                let id = match method_params.get(p) {
                                    None => {
                                        let id = ch.st.new_parameter(
                                            ch.st.get_label(p, false),
                                            ch.st.get_domain_of_var(p),
                                        );
                                        method.add_var(id);
                                        id
                                    }
                                    Some(id) => *id,
                                };
                                st.union_domain(&st.get_domain_id(p), &st.get_domain_id(&id));
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
                        name: task,
                        result,
                        label: None,
                    })
                }
            }
            FlowKind::FlowHandle(h) => {
                queue.push_back(h);
                st.union_var(&start, &fl.get_flow_start(&h));
                ht.add_handle(h);
                //sym_table.union_atom(&ass.get_start(), &fl.get_flow_start(&h));
                let domain_id = st.get_domain_id(&result);
                let drops: Vec<VarId> = st
                    .get_domain_vars(&domain_id)
                    .drain(..)
                    .filter_map(|a| st.get_drop(&st.get_var_parent(&a)))
                    .collect();

                for drop in drops {
                    ht.add_async_drop(&h, drop)
                }

                //ch.add_constraint(Constraint::leq(fl.get_flow_end(&h), Constraint::Max(drops)))
            }
        }
    }

    fl.flat_bindings();

    ch.flat_bindings();

    Ok(ch)
}
