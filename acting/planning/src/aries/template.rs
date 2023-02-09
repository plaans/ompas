use crate::aries::{useful, BindingAriesAtoms, FLOAT_SCALE};
use anyhow::{anyhow, Context};
use aries_core::{IntCst, Lit as aLit, INT_CST_MAX, INT_CST_MIN};
use aries_model::extensions::Shaped;
use aries_model::lang::{
    Atom as aAtom, Atom, ConversionError, FAtom, FVar, IAtom, SAtom, SVar, Type as aType, Variable,
};
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::constraints::{Constraint as aConstraint, Sum};
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{
    Chronicle as aChronicle, ChronicleKind as aChronicleKind,
    ChronicleTemplate as aChronicleTemplate, Condition, Container, Ctx, Effect,
    Problem as aProblem, StateFun, SubTask, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;
use function_name::named;
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::exec::state::INSTANCE;
use ompas_language::sym_table::{
    EPSILON, TYPE_ABSTRACT_TASK, TYPE_COMMAND, TYPE_METHOD, TYPE_OBJECT, TYPE_OBJECT_TYPE,
    TYPE_PREDICATE, TYPE_PRESENCE, TYPE_STATE_FUNCTION, TYPE_TASK, TYPE_TIMEPOINT,
};
use ompas_structs::conversion::chronicle::constraint::Constraint;
use ompas_structs::conversion::chronicle::template::{ChronicleKind, ChronicleTemplate};
use ompas_structs::planning::problem::PlanningProblem;
use ompas_structs::sym_table::computation::Computation;
use ompas_structs::sym_table::domain::basic_type::{
    TYPE_ID_BOOLEAN, TYPE_ID_EMPTY, TYPE_ID_EMPTY_LIST, TYPE_ID_FALSE, TYPE_ID_FLOAT, TYPE_ID_INT,
    TYPE_ID_NIL, TYPE_ID_NUMBER, TYPE_ID_TRUE,
};
use ompas_structs::sym_table::domain::cst::Cst;
use ompas_structs::sym_table::domain::Domain::Simple;
use ompas_structs::sym_table::domain::{Domain, TypeId};
use ompas_structs::sym_table::lit::Lit;
use ompas_structs::sym_table::r#ref::RefSymTable;
use ompas_structs::sym_table::r#trait::{FormatWithSymTable, GetVariables};
use ompas_structs::sym_table::VarId;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::borrow::Borrow;
use std::ops::Deref;
use std::sync::Arc;

#[named]
pub fn generate_templates(
    problem: &PlanningProblem,
    bindings: &mut BindingAriesAtoms,
) -> anyhow::Result<chronicles::Problem> {
    let st = problem.st.clone();
    let lattice = st.get_lattice();
    let instance = &problem.instance;
    let domain = &problem.domain;

    let mut types: Vec<(Sym, Option<Sym>)> = vec![
        (TYPE_TASK.into(), None),
        (TYPE_ABSTRACT_TASK.into(), Some(TYPE_TASK.into())),
        (TYPE_COMMAND.into(), Some(TYPE_TASK.into())),
        (TYPE_METHOD.into(), None),
        (TYPE_PREDICATE.into(), None),
        (TYPE_STATE_FUNCTION.into(), None),
        (TYPE_OBJECT.into(), None),
        (TYPE_OBJECT_TYPE.into(), None),
    ];
    //let top_type: Sym = OBJECT_TYPE.into();

    let mut symbols: Vec<TypedSymbol> = vec![];
    let id = lattice.get_type_id(TYPE_OBJECT).unwrap();

    let new_types: Vec<TypeId> = lattice
        .get_all_childs(id)
        .drain(..)
        .filter(|t| *t != TYPE_ID_EMPTY && t != id)
        .collect();
    //Add TypeHierarchy
    for t in &new_types {
        let sym = lattice.format_type(&t);
        let parent = lattice.format_type(&lattice.get_parent(&t).first().unwrap());
        types.push((sym.to_string().into(), Some(parent.into())));
        symbols.push(TypedSymbol::new(sym, TYPE_OBJECT_TYPE));
    }

    /*for t in &problem.types {
        types.push((t.into(), Some(OBJECT_TYPE.into())));
        symbols.push(TypedSymbol::new(t, TYPE_TYPE));
    }*/

    let th = TypeHierarchy::new(types).unwrap_or_else(|e| panic!("{e}"));

    for (t, instances) in &instance.state.instance.inner {
        for sym in instances {
            symbols.push(TypedSymbol::new(sym, t));
        }
    }

    //Adding custom state functions
    symbols.push(TypedSymbol::new(INSTANCE, TYPE_STATE_FUNCTION));
    symbols.push(TypedSymbol::new(QUANTITY, TYPE_STATE_FUNCTION));
    symbols.push(TypedSymbol::new(MAX_Q, TYPE_STATE_FUNCTION));
    //symbols.push(TypedSymbol::new(NIL_OBJECT, OBJECT_TYPE));
    for sf in &domain.sf {
        symbols.push(TypedSymbol::new(sf.get_label(), TYPE_STATE_FUNCTION));
    }
    for command in &domain.commands {
        symbols.push(TypedSymbol::new(command, TYPE_COMMAND));
    }
    for task in &domain.tasks {
        symbols.push(TypedSymbol::new(task, TYPE_ABSTRACT_TASK));
    }
    for method in &domain.methods {
        symbols.push(TypedSymbol::new(method, TYPE_METHOD));
    }

    let symbols = symbols
        .drain(..)
        .map(|ts| (ts.symbol, ts.tpe.unwrap_or_else(|| TYPE_OBJECT.into())))
        .collect();
    let symbol_table = SymbolTable::new(th, symbols).unwrap_or_else(|e| panic!("{e}"));

    //We have 4 synthetic state functions.
    let mut state_functions = Vec::with_capacity(domain.sf.len() + 3);
    /*
     * Add state function for type.
     * instance(?<object>) -> type
     */
    {
        let sym = symbol_table
            .id(INSTANCE)
            .ok_or_else(|| anyhow!("{} undefined", INSTANCE))?;
        let mut args = Vec::with_capacity(2);
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT)
                .ok_or_else(|| anyhow!("{} undefined.", TYPE_OBJECT))?,
        ));
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined", TYPE_OBJECT_TYPE))?,
        ));
        state_functions.push(StateFun { sym, tpe: args })
    }

    /*
     * Add state function for type.
     * instances(?<type>) -> List<object>
     */
    /*{
        let sym = symbol_table
            .id(INSTANCES)
            .ok_or_else(|| anyhow!("{} undefined", INSTANCE))?;
        let mut args = Vec::with_capacity(2);
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined.", TYPE_OBJECT))?,
        ));
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined", TYPE_OBJECT_TYPE))?,
        ));
        state_functions.push(StateFun { sym, tpe: args })
    }*/

    /*
     * Add state function for type.
     * quantity(?<resource>) -> float
     */
    {
        let sym = symbol_table
            .id(QUANTITY)
            .ok_or_else(|| anyhow!("{} undefined", QUANTITY))?;
        let mut args = Vec::with_capacity(2);
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined.", TYPE_OBJECT))?,
        ));
        args.push(aType::Int);
        state_functions.push(StateFun { sym, tpe: args })
    }

    /*
     * Add state function for type.
     * quantity(?<resource>) -> float
     */
    {
        let sym = symbol_table
            .id(MAX_Q)
            .ok_or_else(|| anyhow!("{} undefined", QUANTITY))?;
        let mut args = Vec::with_capacity(2);
        args.push(aType::Sym(
            symbol_table
                .types
                .id_of(TYPE_OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined.", TYPE_OBJECT))?,
        ));
        args.push(aType::Int);
        state_functions.push(StateFun { sym, tpe: args })
    }

    for sf in &domain.sf {
        let sym = symbol_table.id(sf.get_label()).ok_or_else(|| {
            lruntimeerror!(
                function_name!(),
                format!("{} Unknown symbol", sf.get_label())
            )
        })?;
        let mut args = Vec::with_capacity(sf.parameters.get_number() + 1);
        for tpe in &sf.parameters.get_type_domain() {
            args.push(useful::get_type(&lattice, &symbol_table, tpe)?);
        }
        args.push(useful::get_type(&lattice, &symbol_table, &sf.result)?);
        state_functions.push(StateFun { sym, tpe: args })
    }

    let mut ctx = Ctx::new(Arc::new(symbol_table), state_functions);

    println!("Definition of symbols: Ok!");

    let mut templates: Vec<aChronicleTemplate> = Vec::new();
    let cont = Container::Template(domain.templates.len());

    for template in &domain.templates {
        let mut template = read_chronicle(&mut ctx, bindings, &template, cont.clone())?;
        Printer::print_chronicle(&template.chronicle, &ctx.model);
        templates.push(template);
    }

    Ok(aProblem {
        context: ctx,
        templates,
        chronicles: vec![],
    })
}

#[allow(dead_code)]
fn convert_constraint(
    c: &Constraint,
    _prez: aLit,
    _container: Container,
    bindings: &BindingAriesAtoms,
    st: &RefSymTable,
    ctx: &mut Ctx,
) -> lruntimeerror::Result<Vec<aConstraint>> {
    let get_atom = |a: &VarId, ctx| -> aAtom { useful::var_id_into_atom(a, st, bindings, ctx) };
    let mut constraints = vec![];
    match c {
        Constraint::Leq(a, b) => {
            let a: VarId = a.try_into().expect("");
            let b: VarId = b.try_into().expect("");
            let a: FAtom = get_atom(&a, ctx).try_into().map_err(|c: ConversionError| {
                LRuntimeError::new("conversion_error", c.to_string())
            })?;
            let b: FAtom = get_atom(&b, ctx).try_into().map_err(|c: ConversionError| {
                LRuntimeError::new("conversion_error", c.to_string())
            })?;

            constraints.push(aConstraint::fleq(a, b));
            /*
            let lt = ctx
                .model
                .new_optional_bvar(prez, container / Reification)
                .into();
            let eq: aLit = ctx
                .model
                .new_optional_bvar(prez, container / Reification)
                .into();
            constraints.push(aConstraint::reified_lt(
                get_atom(&a, ctx),
                get_atom(&b, ctx),
                lt,
            ));
            constraints.push(aConstraint::reified_lt(
                get_atom(&a, ctx),
                get_atom(&b, ctx),
                eq,
            ));*/
            //constraints.push(aConstraint::or(vec![lt, eq]));
        }
        Constraint::Eq(a, b) => match (a, b) {
            (Lit::Atom(a), Lit::Atom(b)) => {
                constraints.push(aConstraint::eq(get_atom(a, ctx), get_atom(b, ctx)))
            }
            (Lit::Atom(_), Lit::Constraint(_)) => {}
            (Lit::Atom(a), Lit::Computation(c)) => match c.deref() {
                Computation::Add(vec) => {
                    let a = get_atom(a, ctx);
                    if vec.len() == 2 && vec[1].format(st, true).as_str() == EPSILON {
                        let mut b = get_atom(&vec[0].clone().try_into().expect(""), ctx);
                        b = (FAtom::try_from(b)
                            .map_err(|c| LRuntimeError::new("conversion_error", c.to_string()))?
                            + FAtom::EPSILON)
                            .into();
                        constraints.push(aConstraint::eq(a, b))
                    } else if vec.len() == 2 {
                        if let Lit::Atom(c) = &vec[1] {
                            let c = get_atom(c, ctx);
                            let mut b = get_atom(&vec[0].clone().try_into().expect(""), ctx);
                            match c {
                                Atom::Int(i) => {
                                    let i: IntCst = i.try_into().context("add").expect("");
                                    let bbis = FAtom::try_from(b).map_err(|c| {
                                        LRuntimeError::new("conversion_error", c.to_string())
                                    })?;
                                    b = (bbis + i).into();
                                }
                                Atom::Fixed(f) => {
                                    let f: IntCst =
                                        IntCst::try_from(f.num).context("add").expect("") / f.denom;

                                    let bbis = FAtom::try_from(b).map_err(|c| {
                                        LRuntimeError::new("conversion_error", c.to_string())
                                    })?;
                                    let r: FAtom = bbis + f;
                                    b = r.into()
                                }
                                _ => todo!(),
                            }
                            b = (FAtom::try_from(b).map_err(|c| {
                                LRuntimeError::new("conversion_error", c.to_string())
                            })? + FAtom::EPSILON)
                                .into();
                            constraints.push(aConstraint::eq(a, b))
                        }
                    } else {
                        let mut signs = vec![true];
                        let mut variables = vec![a];
                        for var in vec {
                            signs.push(false);
                            let a: VarId = var.try_into().expect("");
                            variables.push(get_atom(&a, ctx))
                        }

                        constraints.push(aConstraint::sum(variables, Sum { signs, value: 0 }));
                    }
                }
                Computation::Sub(vec) => {
                    let a = get_atom(a, ctx);
                    let b: VarId = vec[0].borrow().try_into().expect("");
                    let b = get_atom(&b, ctx);
                    let mut signs = vec![true, false];
                    let mut variables = vec![a, b];
                    for var in &vec[1..] {
                        signs.push(true);
                        let a: VarId = var.try_into().expect("");
                        variables.push(get_atom(&a, ctx))
                    }

                    constraints.push(aConstraint::sum(variables, Sum { signs, value: 0 }));
                }
            },
            _ => Err(LRuntimeError::default().chain("constraint::eq"))?,
        },
        Constraint::Not(a) => {
            let a: VarId = a.try_into().expect("");
            constraints.push(aConstraint::eq(get_atom(&a, ctx), aLit::FALSE))
        }
        Constraint::Lt(a, b) => {
            let a: VarId = a.try_into().expect("");
            let b: VarId = b.try_into().expect("");
            constraints.push(aConstraint::lt(get_atom(&a, ctx), get_atom(&b, ctx)))
        }
        Constraint::And(and) => {
            todo!()
        }
        Constraint::Or(or) => {
            todo!()
        }
        Constraint::Type(_, _) => {
            unreachable!()
        }

        Constraint::Arbitrary(_) => {
            unreachable!()
        }
        Constraint::Neq(a, b) => {
            let a: VarId = a.try_into().expect("");
            match b {
                Lit::Atom(b) => {
                    constraints.push(aConstraint::neq(get_atom(&a, ctx), get_atom(b, ctx)));
                }
                Lit::Constraint(_c) => {
                    /*constraints.push(aConstraint::reify(
                        false,
                        aConstraint::reify(
                            get_atom(&a),
                            convert_constraint(c.deref(), prez, container, bindings, st, ctx)?,
                        ),
                    ));*/
                }
                Lit::Exp(_) => Err(LRuntimeError::default().chain("constraint::arbitrary"))?,
                _ => {}
            }
        }
        Constraint::Min(_) => {}
        Constraint::Max(_) => {}
    };
    Ok(constraints)
}

pub fn read_chronicle(
    ctx: &mut Ctx,
    bindings: &mut BindingAriesAtoms,
    ch: &ChronicleTemplate,
    container: Container,
) -> anyhow::Result<aChronicleTemplate> {
    /*println!(
        "reading chronicle: {}",
        chronicle.format_with_sym_table(&ch.sym_table, false)
    );*/

    let st = ch.st.clone();
    let lattice = st.get_lattice();

    let mut params: Vec<Variable> = Vec::new();
    let mut constraints: Vec<aConstraint> = vec![];
    let mut conditions: Vec<Condition> = vec![];
    let mut effects: Vec<Effect> = vec![];
    let mut subtasks: Vec<SubTask> = vec![];
    // Declaration of the presence variable

    //Declaration of the variables
    let prez_var = ctx.model.new_bvar(container / VarType::Presence);
    bindings.add_binding(ch.get_presence(), &prez_var.into());
    let prez = prez_var.true_lit();
    params.push(prez_var.into());

    print!("init params...");
    //TODO: handle case where some parameters are already instantiated.
    for var in &ch.get_variables() {
        let var_domain = st.get_var_domain(var);
        let domain = &var_domain.domain;
        let label = st.get_label(var, false);

        let (t, cst) = &match domain {
            Simple(TYPE_ID_NIL | TYPE_ID_FALSE) => (TYPE_ID_BOOLEAN, Some(Cst::Bool(false))),
            Simple(TYPE_ID_TRUE) => (TYPE_ID_BOOLEAN, Some(Cst::Bool(true))),
            Simple(t) => (*t, None),
            Domain::Cst(t, c) => {
                if let Simple(t) = t.deref() {
                    (*t, Some(c.clone()))
                } else {
                    unreachable!()
                }
            }
            d => {
                if d == &Domain::Union(vec![TYPE_ID_FALSE.into(), TYPE_ID_EMPTY_LIST.into()]) {
                    (TYPE_ID_BOOLEAN, Some(Cst::Bool(false)))
                } else {
                    panic!(
                        "{} is not supported in domaine def",
                        d.format(&ch.st.get_lattice())
                    )
                }
            }
        };

        let param: Variable = if t == lattice.get_type_id(TYPE_TIMEPOINT).unwrap() {
            let fvar = ctx.model.new_optional_fvar(
                0,
                INT_CST_MAX,
                TIME_SCALE,
                prez,
                container / VarType::Parameter(label),
            );

            bindings.add_binding(var, &fvar.into());
            fvar.into()
        } else if t == lattice.get_type_id(TYPE_PRESENCE).unwrap() {
            if let Some(var) = bindings.get_var(var) {
                *var
            } else {
                panic!()
            }
        } else if *t == TYPE_ID_INT {
            let ivar = ctx.model.new_optional_ivar(
                INT_CST_MIN,
                INT_CST_MAX,
                prez,
                container / VarType::Parameter(label),
            );
            bindings.add_binding(var, &ivar.into());
            ivar.into()
        } else if *t == TYPE_ID_FLOAT || *t == TYPE_ID_NUMBER {
            let fvar = ctx.model.new_optional_fvar(
                INT_CST_MIN,
                INT_CST_MAX,
                TIME_SCALE, //Not sure of that
                prez,
                container / VarType::Parameter(label),
            );
            bindings.add_binding(var, &fvar.into());
            fvar.into()
        } else if *t == TYPE_ID_BOOLEAN {
            let bvar = ctx
                .model
                .new_optional_bvar(prez, container / VarType::Parameter(label));
            //context.model.
            bindings.add_binding(var, &bvar.into());
            bvar.into()
        } else if *t == TYPE_ID_NIL {
            unreachable!()
            /*let bvar = context
                .model
                .new_optional_bvar(prez, container / VarType::Parameter(label));
            bindings.add_binding(var, &bvar.into());
            bvar.into()*/
        } else {
            let str = domain.format(&lattice);
            let t = ctx
                .model
                .get_symbol_table()
                .types
                .id_of(&str)
                .unwrap_or_else(|| panic!("{str} should be defined in type hierarchy"));
            //let s = ch.sym_table.get_atom(var, true).unwrap();
            let svar =
                ctx.model
                    .new_optional_sym_var(t, prez, container / VarType::Parameter(label));
            bindings.add_binding(var, &svar.into());
            svar.into()
        };

        if let Some(cst) = cst {
            let cst = useful::atom_from_cst(ctx, &cst);

            constraints.push(aConstraint::eq(param, cst));
        }

        params.push(param);
    }
    println!("ok!");

    //End declaration of the variables

    /*
    CREATION of the name
     */
    //For the moment lacking the fact that we can add any kind of variables
    print!("init name...");
    let mut name: Vec<SAtom> = vec![];
    for (i, p) in ch.get_name().iter().enumerate() {
        if i == 0 {
            name.push(
                ctx.typed_sym(
                    ctx.model
                        .get_symbol_table()
                        .id(&st.format_variable(p).to_string())
                        .unwrap(),
                )
                .into(),
            )
        } else {
            name.push(
                SVar::try_from(*bindings.get_var(p).unwrap())
                    .unwrap_or_else(|e| panic!("{e}"))
                    .into(),
            )
        }
    }
    println!("ok!");

    let get_atom =
        |a: &VarId, context| -> aAtom { useful::var_id_into_atom(a, &st, bindings, context) };

    for x in ch.get_constraints() {
        let mut x = convert_constraint(x, prez, container, bindings, &st, ctx)?;
        constraints.append(&mut x);
    }

    print!("init conditions...");
    for c in ch.get_conditions() {
        let sv =
            c.sv.iter()
                .map(|a| {
                    get_atom(a, ctx)
                        .try_into()
                        .unwrap_or_else(|e| panic!("{}", e))
                })
                .collect();
        let value = get_atom(&c.value, ctx);
        let start: FVar =
            useful::try_variable_into_fvar(bindings.get_var(&c.get_start()).cloned().unwrap())?;
        let start = FAtom::from(start);
        let end: FVar =
            useful::try_variable_into_fvar(bindings.get_var(&c.get_end()).cloned().unwrap())?;
        let end = FAtom::from(end);
        let condition = Condition {
            start,
            state_var: sv,
            value,
            end,
        };
        conditions.push(condition);
    }
    println!("ok!");

    print!("init effects...");
    for e in ch.get_effects() {
        let sv =
            e.sv.iter()
                .map(|a| {
                    get_atom(a, ctx)
                        .try_into()
                        .unwrap_or_else(|e| panic!("{}", e))
                })
                .collect();
        let value = get_atom(&e.value, ctx);
        let start: FVar =
            useful::try_variable_into_fvar(*bindings.get_var(&e.get_start()).unwrap())?;
        let start = FAtom::from(start);
        let end: FVar = useful::try_variable_into_fvar(*bindings.get_var(&e.get_end()).unwrap())?;
        let end = FAtom::from(end);
        let effect = Effect {
            transition_start: start, // + FAtom::EPSILON,
            persistence_start: end,  // + FAtom::EPSILON,
            min_persistence_end: vec![],
            state_var: sv,
            value,
        };
        effects.push(effect);
    }
    println!("ok!");

    print!("init subtasks...");
    for s in ch.get_subtasks() {
        let start: FAtom = get_atom(&s.interval.get_start(), ctx)
            .try_into()
            .unwrap_or_else(|t| panic!("{:?}", t));
        let end: FAtom = get_atom(&s.interval.get_end(), ctx)
            .try_into()
            .unwrap_or_else(|t| panic!("{:?}", t));
        let e: Vec<SAtom> = s
            .task
            .iter()
            .map(|a| get_atom(&a, ctx).try_into().expect(""))
            .collect();
        let st = SubTask {
            id: None,
            start,
            end,
            task_name: e,
        };

        subtasks.push(st);
    }
    println!("ok!");

    let start = useful::try_variable_into_fvar(
        bindings
            .get_var(&ch.get_interval().get_start())
            .unwrap()
            .clone(),
    )?;
    let start = FAtom::from(start);
    let end = useful::try_variable_into_fvar(
        bindings
            .get_var(&ch.get_interval().get_end())
            .unwrap()
            .clone(),
    )?;
    let end = FAtom::from(end);

    print!("init task...");
    let task: Vec<SAtom> = ch
        .get_task()
        .iter()
        .map(|a| get_atom(a, ctx).try_into().expect(""))
        .collect();
    println!("ok!");
    print!("\n\n");

    let template = aChronicle {
        kind: match ch.debug.kind {
            ChronicleKind::Command => aChronicleKind::Action,
            ChronicleKind::Method => aChronicleKind::Method,
            ChronicleKind::Task => aChronicleKind::Action,
        },
        presence: prez,
        start,
        end,
        name,
        task: Some(task),
        conditions,
        effects,
        constraints,
        subtasks,
        cost: None,
    };

    let template = aChronicleTemplate {
        label: None,
        parameters: params,
        chronicle: template,
    };

    Ok(template)
}
