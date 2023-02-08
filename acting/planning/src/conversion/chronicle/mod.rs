use function_name::named;
use ompas_language::sym_table::COND;
use ompas_structs::conversion::chronicle::condition::Condition;
use ompas_structs::conversion::chronicle::constraint::Constraint;
use ompas_structs::conversion::chronicle::effect::Effect;
use ompas_structs::conversion::chronicle::subtask::SubTask;
use ompas_structs::conversion::chronicle::task_template::TaskTemplate;
use ompas_structs::conversion::chronicle::template::{ChronicleKind, ChronicleTemplate};
use ompas_structs::conversion::flow_graph::flow::{FlowId, FlowKind};
use ompas_structs::conversion::flow_graph::graph::FlowGraph;
use ompas_structs::sym_table::computation::Computation;
use ompas_structs::sym_table::domain::basic_type::BasicType::Boolean;
use ompas_structs::sym_table::domain::Domain;
use ompas_structs::sym_table::lit::Lit;
use ompas_structs::sym_table::litset::LitSet;
use ompas_structs::sym_table::r#trait::FormatWithSymTable;
use ompas_structs::sym_table::r#trait::{GetVariables, Replace};
use ompas_structs::sym_table::VarId;
use sompas_structs::lenv::LEnv;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::collections::{HashMap, HashSet, VecDeque};
use std::ops::Deref;

pub mod post_processing;

#[derive(Default, Clone)]
pub struct HandleTable {
    map: HashMap<FlowId, HandleMeta>,
}

impl HandleTable {
    pub fn add_handle(&mut self, flow: FlowId, kind: HandleKind) {
        self.map.insert(
            flow,
            HandleMeta {
                kind,
                drops: Default::default(),
                release: Default::default(),
            },
        );
    }

    pub fn add_drop(&mut self, flow: &FlowId, drop: VarId) {
        if let Some(handle) = self.map.get_mut(&flow) {
            handle.drops.insert(drop);
        } else {
            panic!("");
        }
    }

    pub fn add_release(&mut self, flow: &FlowId, release: VarId) {
        if let Some(handle) = self.map.get_mut(&flow) {
            handle.release.insert(release);
        } else {
            panic!("");
        }
    }
}

#[derive(Clone)]
pub enum HandleKind {
    Async,
    Ressource,
}

#[derive(Clone)]
pub struct HandleMeta {
    kind: HandleKind,
    drops: HashSet<VarId>,
    release: HashSet<VarId>,
}

pub fn convert_method(
    ch: Option<ChronicleTemplate>,
    fl: &mut FlowGraph,
    flow: &FlowId,
    env: &LEnv,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let ht = &mut HandleTable::default();

    let mut ch = convert_into_chronicle(ch, ht, fl, flow, env)?;

    for (flow, handle) in ht.map.iter() {
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
        let mut release = handle.release.clone();
        release.insert(t_drop);
        let release: Vec<VarId> = release.drain().map(|v| ch.st.get_var_parent(&v)).collect();
        let lit: Option<Lit> = match release.len() {
            0 => None,
            1 => Some(release.first().unwrap().into()),
            _ => Some(Constraint::min(release).into()),
        };

        if let Some(lit) = lit {
            let end = fl.st.get_var_parent(&fl.get_flow_end(flow));
            match handle.kind {
                HandleKind::Async => ch.add_constraint(Constraint::leq(end, lit)),
                HandleKind::Ressource => match lit {
                    /*Lit::Atom(a) => {
                        ch.st.union_var(&a, &end);
                    }*/
                    _ => {
                        ch.add_constraint(Constraint::eq(end, lit));
                    }
                },
            }
        }
    }

    Ok(ch)
}

#[named]
pub fn convert_into_chronicle(
    ch: Option<ChronicleTemplate>,
    ht: &mut HandleTable,
    fl: &mut FlowGraph,
    flow: &FlowId,
    env: &LEnv,
) -> Result<ChronicleTemplate, LRuntimeError> {
    let st = fl.st.clone();

    let mut ch = ch.unwrap_or(ChronicleTemplate::new(
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

    st.union_var(&ch.get_result(), &fl.get_flow_result(flow));

    let mut queue = VecDeque::new();
    queue.push_back(*flow);

    while let Some(flow_id) = queue.pop_front() {
        let flow = fl.flows[flow_id].clone();
        let interval = fl.get_flow_interval(&flow_id);
        let result = fl.get_flow_result(&flow_id);
        let start = interval.get_start();
        let end = interval.get_end();

        if start != end {
            ch.add_constraint(Constraint::leq(start, end))
        }

        match flow.kind {
            FlowKind::Lit(lit) => {
                match &lit {
                    Lit::Exp(_) => {}
                    Lit::Atom(_) => {}
                    Lit::Await(a) => {
                        let handle = fl.get_handle(a).unwrap();
                        ht.add_release(handle, interval.get_end());

                        /*ch.add_constraint(Constraint::leq(
                            fl.get_flow_end(&handle),
                            interval.get_end(),
                        ));*/
                        st.union_var(&fl.get_flow_result(&handle), &result);
                    }
                    Lit::Release(release) => {
                        let handle = fl.get_handle(release).unwrap();
                        ht.add_release(handle, interval.get_end());

                        /*ch.add_constraint(Constraint::eq(
                            fl.get_flow_end(&handle),
                            interval.get_end(),
                        ));*/
                    }
                    Lit::Constraint(c) => match c.deref() {
                        Constraint::Arbitrary(set) => match set {
                            LitSet::Finite(set) => {
                                let mut constraints = vec![];
                                for e in set {
                                    constraints.push(Constraint::eq(result, e))
                                }

                                ch.add_constraint(Constraint::or(constraints))
                            }
                            LitSet::Domain(d) => {
                                let id = st.new_parameter("_arbitrary_", Domain::any());
                                let r#type: String = d.format(&st, true);
                                let domain = st.get_type_as_domain(&r#type).ok_or_else(|| {
                                    LRuntimeError::new(
                                        function_name!(),
                                        format!("{} is not a defined type", r#type),
                                    )
                                })?;
                                st.meet_to_domain(&st.get_domain_id(&result), domain);
                                st.union_var(&id, &result);
                            }
                        },
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
                                for (f, t) in args.iter().zip(types) {
                                    let r = fl.st.get_domain_id(&f);
                                    if !fl.st.meet_to_domain(&r, t).is_none() {
                                        panic!("brrruuuuuh")
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
                                for (f, t) in args.iter().zip(types) {
                                    let r = fl.st.get_domain_id(&f);
                                    if !fl.st.meet_to_domain(&r, t).is_none() {
                                        panic!("brrruuuuuh")
                                    };
                                }
                            }
                        }

                        ch.add_effect(effect);

                        let result = ch.st.new_nil();
                        st.union_var(&result, &result);
                    }
                    Lit::Exec(exec) => {
                        let subtask = SubTask {
                            interval,
                            lit: exec.clone(),
                            result,
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
                                    let r = fl.st.get_domain_id(&f);
                                    /*if !fl.st.meet_to_domain(&r, t).is_none() {
                                        panic!("brrruuuuuh")
                                    };*/
                                }
                            }
                        }

                        ch.add_subtask(subtask);
                        /*let subtask_result = ch.sym_table.new_nil();
                        st.union_atom(&subtask_result, &result);*/
                    }
                    Lit::Set(_) => panic!("set not supported yet"),
                }
            }
            FlowKind::Seq(seq) => queue.append(&mut VecDeque::from(seq)),
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
                        (ChronicleTemplate, HashMap<VarId, VarId>),
                        LRuntimeError,
                    > {
                        let mut branch_params: HashMap<VarId, VarId> = Default::default();
                        let mut method = convert_into_chronicle(None, ht, fl, flow, env)?;
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
                        |mut method: ChronicleTemplate,
                         method_params: HashMap<VarId, VarId>|
                         -> Result<ChronicleTemplate, LRuntimeError> {
                            for p in &task_params {
                                let id = match method_params.get(p) {
                                    None => {
                                        let id = ch.st.new_parameter(
                                            ch.st.get_label(&p, false),
                                            ch.st.get_domain_of_var(p),
                                        );
                                        method.add_var(id);
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
            FlowKind::FlowHandle(h) => {
                queue.push_back(h);
                st.union_var(&start, &fl.get_flow_start(&h));
                ht.add_handle(h, HandleKind::Async);
                //sym_table.union_atom(&ass.get_start(), &fl.get_flow_start(&h));
                let domain_id = st.get_domain_id(&result);
                let drops: Vec<VarId> = st
                    .get_domain_vars(&domain_id)
                    .drain(..)
                    .filter_map(|a| st.get_drop(&st.get_var_parent(&a)))
                    .collect();

                for drop in drops {
                    ht.add_drop(&h, drop)
                }

                //ch.add_constraint(Constraint::leq(fl.get_flow_end(&h), Constraint::Max(drops)))
            }
            FlowKind::FlowPause(fw) => {
                if let Some(duration) = fw.duration {
                    ch.add_constraint(Constraint::eq(end, Computation::add(vec![start, duration])));
                }
            }
            FlowKind::FlowResourceHandle(h) => {
                queue.push_back(h);
                let domain_id = st.get_domain_id(&result);
                ht.add_handle(h, HandleKind::Ressource);
                ch.add_constraint(Constraint::leq(start, fl.get_flow_start(&h)));

                let drops: Vec<VarId> = st
                    .get_domain_vars(&domain_id)
                    .drain(..)
                    .filter_map(|a| st.get_drop(&st.get_var_parent(&a)))
                    .collect();

                for drop in drops {
                    ht.add_drop(&h, drop)
                }

                /*let drops: Vec<Lit> = st
                    .get_domain_vars(&domain_id)
                    .drain(..)
                    .filter_map(|a| st.get_drop(&st.get_var_parent(&a)).map(|d| Lit::from(d)))
                    .unique()
                    .collect();
                ch.add_constraint(Constraint::eq(fl.get_flow_end(&h), Constraint::Max(drops)))*/
            }
        }
    }

    fl.flat_bindings();

    ch.flat_bindings();

    Ok(ch)
}
