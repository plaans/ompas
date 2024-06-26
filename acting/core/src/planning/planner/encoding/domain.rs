use crate::model::acting_domain::model::ActingModel;
use crate::model::chronicle::computation::Computation;
use crate::model::chronicle::constraint::Constraint;
use crate::model::chronicle::effect::EffectOperationInner;
use crate::model::chronicle::lit::Lit;
use crate::model::chronicle::{Chronicle, ChronicleKind};
use crate::model::sym_domain::basic_type::{
    TYPE_ID_BOOLEAN, TYPE_ID_EMPTY, TYPE_ID_EMPTY_LIST, TYPE_ID_FALSE, TYPE_ID_FLOAT, TYPE_ID_INT,
    TYPE_ID_NIL, TYPE_ID_NUMBER, TYPE_ID_TRUE,
};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_domain::Domain::Simple;
use crate::model::sym_domain::{Domain, TypeId};
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::r#trait::{FormatWithSymTable, GetVariables};
use crate::model::sym_table::VarId;
use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::ompas::manager::state::instance::InstanceCollection;
use crate::planning::planner::encoding::{
    atom_from_cst, get_type, var_id_into_atom, PlannerDomain,
};
use crate::OMPAS_PLAN_ENCODING_OPTIMIZATION;
use aries::core::{IntCst, Lit as aLit, INT_CST_MAX, INT_CST_MIN};
use aries::model::extensions::Shaped;
use aries::model::lang::linear::{LinearSum, LinearTerm};
use aries::model::lang::{Atom as aAtom, Atom, ConversionError, FAtom, IAtom, IVar, Variable};
use aries::model::symbols::SymbolTable;
use aries::model::types::TypeHierarchy;
use aries::utils::input::Sym;
use aries_planning::chronicles::constraints::{Constraint as aConstraint, ConstraintType};
use aries_planning::chronicles::printer::Printer;
use aries_planning::chronicles::{
    Chronicle as aChronicle, ChronicleKind as aChronicleKind,
    ChronicleTemplate as aChronicleTemplate, Condition, Container, Ctx, Effect, EffectOp, Fluent,
    StateVar, SubTask, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use env_param::EnvParam;
use function_name::named;
use ompas_language::exec::state::UNKNOWN;
use ompas_language::sym_table::*;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use std::ops::Deref;
use std::sync::Arc;

static TIMEPOINT_UB: EnvParam<IntCst> = EnvParam::new("OMPAS_TIMEPOINT_UB", "1000000");

#[named]
pub fn encode_ctx(
    st: &RefSymTable,
    domain: &PlannerDomain,
    instance: &InstanceCollection,
) -> anyhow::Result<Ctx> {
    let st = st.clone();
    let lattice = st.get_lattice();

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
        let sym = lattice.format_type(t);
        let parent = lattice.format_type(lattice.get_parent(t).first().unwrap());
        //println!("type: {}, parent: {}", sym, parent);
        types.push((sym.to_string().into(), Some(parent.into())));
        symbols.push(TypedSymbol::new(sym, TYPE_OBJECT_TYPE));
    }

    let th = TypeHierarchy::new(types).unwrap_or_else(|e| panic!("{e}"));

    for (t, instances) in &instance.inner {
        for sym in instances.get_all_elements() {
            symbols.push(TypedSymbol::new(sym, t));
        }
    }

    symbols.push(TypedSymbol::new(UNKNOWN, TYPE_OBJECT));

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
    let mut state_functions = Vec::with_capacity(domain.sf.len());
    /*
     * Add state function for type.
     * instance(?<object>) -> type
     */

    for sf in &domain.sf {
        let sym = symbol_table.id(sf.get_label()).ok_or_else(|| {
            lruntimeerror!(
                function_name!(),
                format!("{} Unknown symbol", sf.get_label())
            )
        })?;
        let mut args = Vec::with_capacity(sf.parameters.get_number() + 1);
        for tpe in &sf.parameters.get_type_domain() {
            args.push(get_type(&lattice, &symbol_table, tpe)?);
        }
        args.push(get_type(&lattice, &symbol_table, &sf.result)?);
        state_functions.push(Fluent {
            sym,
            signature: args,
        })
    }

    Ok(Ctx::new(Arc::new(symbol_table), state_functions))
}

pub fn generate_templates(
    table: &mut ActingVarRefTable,
    ctx: &mut Ctx,
    templates: &Vec<ActingModel>,
) -> anyhow::Result<Vec<aChronicleTemplate>> {
    let mut planner_templates: Vec<aChronicleTemplate> = Vec::new();
    let cont = Container::Template(templates.len());

    for template in templates {
        let template =
            read_chronicle(ctx, table, template.chronicle.as_ref().unwrap(), cont, None)?;
        Printer::print_chronicle(&template.chronicle, &ctx.model);
        planner_templates.push(template);
    }

    Ok(planner_templates)
}

#[allow(dead_code)]
fn convert_constraint(
    c: &Constraint,
    prez: aLit,
    container: Container,
    table: &ActingVarRefTable,
    st: &RefSymTable,
    ctx: &mut Ctx,
    reified: Option<aLit>,
) -> lruntimeerror::Result<Vec<aConstraint>> {
    let get_atom = |a: &VarId, ctx: &Ctx| -> aAtom { var_id_into_atom(a, st, table, ctx) };
    let mut constraints = vec![];

    let mut constraint = match c {
        Constraint::Leq(a, b) => {
            let a: VarId = a.try_into().expect("");
            let b: VarId = b.try_into().expect("");
            let a: FAtom = get_atom(&a, ctx).try_into().map_err(|c: ConversionError| {
                LRuntimeError::new("conversion_error", c.to_string())
            })?;
            let b: FAtom = get_atom(&b, ctx).try_into().map_err(|c: ConversionError| {
                LRuntimeError::new("conversion_error", c.to_string())
            })?;
            aConstraint::leq(a, b)
        }
        Constraint::Eq(a, b) => match (a, b) {
            (Lit::Atom(a), Lit::Atom(b)) => aConstraint::eq(get_atom(a, ctx), get_atom(b, ctx)),
            (Lit::Atom(a), Lit::Constraint(c)) => {
                let value = ctx
                    .model
                    .new_optional_bvar(prez, container / VarType::Reification)
                    .into();
                let mut cs =
                    convert_constraint(c.deref(), prez, container, table, st, ctx, Some(value))?;
                constraints.append(&mut cs);
                aConstraint::eq(get_atom(a, ctx), value)
            }
            (Lit::Atom(value), Lit::Computation(c)) => {
                let value = get_atom(value, ctx);
                let mut variables = vec![(-1, value)];
                match c.deref() {
                    Computation::Add(add) => {
                        for var in add {
                            let var: VarId = var.try_into().expect("");
                            variables.push((1, get_atom(&var, ctx)))
                        }
                    }
                    Computation::Sub(sub) => {
                        for (i, var) in sub.iter().enumerate() {
                            let var: VarId = var.try_into().expect("");
                            variables.push((if i == 0 { 1 } else { -1 }, get_atom(&var, ctx)))
                        }
                    }
                    //Only supported for multiplication by a constant
                    Computation::Mul(mul) => {
                        let mut num = 1;
                        let mut denom = 1;
                        let mut iatom: Option<IAtom> = None;
                        for v in mul {
                            let v: VarId = v.try_into().expect("");
                            let atom = get_atom(&v, ctx);
                            match atom {
                                Atom::Int(i) => {
                                    if i.var == IVar::ZERO {
                                        num *= i.shift;
                                    } else if iatom.is_none() {
                                        iatom = Some(i);
                                    } else {
                                        panic!(
                                            "Multiplication between variables not supported yet."
                                        )
                                    }
                                }
                                Atom::Fixed(f) => {
                                    denom *= f.denom;
                                    if f.num.var == IVar::ZERO {
                                        num *= f.num.shift;
                                    } else if iatom.is_none() {
                                        iatom = Some(f.num)
                                    } else {
                                        panic!(
                                            "Multiplication between variables not supported yet."
                                        )
                                    }
                                }
                                _ => {
                                    todo!()
                                }
                            }
                        }

                        let iatom = iatom.unwrap_or(IAtom::new(IVar::ZERO, 1));

                        let atom = if denom != 1 {
                            Atom::Fixed(FAtom::new(iatom, denom))
                        } else {
                            Atom::Int(iatom)
                        };

                        variables.push((num, atom))
                    }
                    //Only supported for division by a constant
                    Computation::Div(div) => {
                        let mut num = 1;
                        let mut denom = 1;
                        let mut iatom: Option<IAtom> = None;
                        for (i, v) in div.iter().enumerate() {
                            let v: VarId = v.try_into().expect("");
                            let atom = get_atom(&v, ctx);
                            if i == 0 {
                                match atom {
                                    Atom::Int(i) => iatom = Some(i),
                                    Atom::Fixed(f) => {
                                        iatom = Some(f.num);
                                        denom *= f.denom;
                                    }
                                    _ => panic!(),
                                }
                            } else {
                                match atom {
                                    Atom::Int(i) => {
                                        if i.var == IVar::ZERO {
                                            denom *= i.shift;
                                        } else {
                                            panic!("Variables as denominator is not supported.")
                                        }
                                    }
                                    Atom::Fixed(f) => {
                                        num *= f.denom;
                                        if f.num.var == IVar::ZERO {
                                            denom *= f.num.shift;
                                        } else {
                                            panic!("Variables as denominator is not supported.")
                                        }
                                    }
                                    _ => {
                                        panic!()
                                    }
                                }
                            }
                        }

                        let iatom = iatom.unwrap_or(IAtom::new(IVar::ZERO, 1));

                        let atom = if denom != 1 {
                            Atom::Fixed(FAtom::new(iatom, denom))
                        } else {
                            Atom::Int(iatom)
                        };

                        variables.push((num, atom))
                    }
                }

                let mut sum = LinearSum::zero();

                for (sign, var) in variables {
                    match var {
                        Atom::Int(IAtom { var, shift }) => {
                            sum += LinearTerm::int(sign, var, prez);
                            sum += LinearTerm::constant_int(sign * shift, prez);
                        }
                        Atom::Fixed(FAtom {
                            num: IAtom { var, shift },
                            denom,
                        }) => {
                            sum += LinearTerm::rational(sign, var, denom, prez);
                            sum += LinearTerm::constant_rational(sign * shift, denom, prez);
                        }
                        _ => unreachable!(),
                    };
                }

                let sum = sum.simplify();
                //println!("INFO! sum = {:?}", sum);
                aConstraint::linear_eq_zero(sum)
            }
            _ => Err(LRuntimeError::new(
                "constraint::eq",
                format!("cannot encode constraint {}", c.format(st, false)),
            ))?,
        },
        Constraint::Not(a) => {
            let a: VarId = a.try_into().expect("");
            aConstraint::eq(get_atom(&a, ctx), aLit::FALSE)
        }
        Constraint::Lt(a, b) => {
            let a: VarId = a.try_into().expect("");
            let b: VarId = b.try_into().expect("");
            aConstraint::lt(get_atom(&a, ctx), get_atom(&b, ctx))
        }
        Constraint::And(args) => {
            let mut or_args: Vec<aAtom> = vec![];
            for arg in args {
                match arg {
                    Lit::Atom(id) => {
                        let atom = get_atom(id, ctx);
                        or_args.push(atom)
                    }
                    Lit::Constraint(c) => {
                        let value: aLit = ctx
                            .model
                            .new_optional_bvar(prez, container / VarType::Reification)
                            .into();
                        let mut cs =
                            convert_constraint(c, prez, container, table, st, ctx, Some(value))?;

                        constraints.append(&mut cs);
                        or_args.push(value.into())
                    }
                    _ => panic!(),
                }
            }
            aConstraint {
                variables: or_args
                    .drain(..)
                    .map(|a| aLit::try_from(a).unwrap().not().into())
                    .collect(),
                tpe: ConstraintType::Or,
                value: None,
            }
        }
        Constraint::Or(args) => {
            let mut or_args: Vec<aAtom> = vec![];
            for arg in args {
                match arg {
                    Lit::Atom(id) => {
                        let atom = get_atom(id, ctx);
                        or_args.push(atom)
                    }
                    Lit::Constraint(c) => {
                        let value: aLit = ctx
                            .model
                            .new_optional_bvar(prez, container / VarType::Reification)
                            .into();
                        let mut cs =
                            convert_constraint(c, prez, container, table, st, ctx, Some(value))?;

                        constraints.append(&mut cs);
                        or_args.push(value.into())
                    }
                    _ => panic!(),
                }
            }
            aConstraint {
                variables: or_args,
                tpe: ConstraintType::Or,
                value: None,
            }
        }
        Constraint::Neq(a, b) => {
            let a: VarId = a.try_into().expect("");
            match b {
                Lit::Atom(b) => aConstraint::neq(get_atom(&a, ctx), get_atom(b, ctx)),
                Lit::Constraint(_c) => {
                    let value = ctx
                        .model
                        .new_optional_bvar(prez, container / VarType::Reification)
                        .into();
                    let mut cs =
                        convert_constraint(c, prez, container, table, st, ctx, Some(value))?;
                    constraints.append(&mut cs);
                    aConstraint::neq(get_atom(&a, ctx), value)
                }
                Lit::Exp(_) => Err(LRuntimeError::default().chain("constraint::arbitrary"))?,
                _ => {
                    todo!()
                }
            }
        }
        Constraint::Min(_) | Constraint::Max(_) => {
            todo!()
        }
        Constraint::Arbitrary(_) => {
            unreachable!()
        }
    };

    constraint.value = reified;

    constraints.push(constraint);
    Ok(constraints)
}

pub fn read_chronicle(
    ctx: &mut Ctx,
    table: &mut ActingVarRefTable,
    ch: &Chronicle,
    container: Container,
    scope: Option<aLit>,
) -> anyhow::Result<aChronicleTemplate> {
    /*println!(
        "reading chronicle: {}",
        chronicle.format_with_sym_table(&ch.sym_table, false)
    );*/

    enum VarVal {
        Range(i64, i64),
        Val(Cst),
    }

    let st = ch.st.clone();
    let lattice = st.get_lattice();

    let mut params: Vec<Variable> = Vec::new();
    let mut constraints: Vec<aConstraint> = vec![];
    let mut conditions: Vec<Condition> = vec![];
    let mut effects: Vec<Effect> = vec![];
    let mut subtasks: Vec<SubTask> = vec![];
    // Declaration of the presence variable

    //Declaration of the variables

    let presence = ch.get_presence();
    let d_presence = st.get_domain_of_var(presence);
    let prez = if d_presence.is_true() {
        aLit::TRUE
    } else {
        let prez_var = match scope {
            Some(scope) => ctx
                .model
                .new_presence_variable(scope, container / VarType::Presence),
            None => ctx.model.new_bvar(container / VarType::Presence),
        };
        let variable: Variable = prez_var.into();
        table.add_binding(ch.get_presence(), variable);
        params.push(variable);
        let prez: aLit = prez_var.true_lit();
        prez
    };

    if ch.meta_data.kind == ChronicleKind::Root {
        table.add_binding(
            ch.get_interval().get_start(),
            Variable::try_from(Atom::from(ctx.origin()))?,
        );
        table.add_binding(
            ch.get_interval().get_end(),
            Variable::try_from(Atom::from(ctx.horizon()))?,
        );
    } else {
        let start = ctx.model.new_optional_fvar(
            INT_CST_MIN,
            INT_CST_MAX,
            TIME_SCALE.get(), //Not sure of that
            prez,
            container / VarType::ChronicleStart,
        );
        let end = ctx.model.new_optional_fvar(
            INT_CST_MIN,
            INT_CST_MAX,
            TIME_SCALE.get(), //Not sure of that
            prez,
            container / VarType::ChronicleEnd,
        );
        table.add_binding(ch.get_interval().get_start(), start.into());
        table.add_binding(ch.get_interval().get_end(), end.into());
    }

    //print!("init params...");
    //TODO: handle case where some parameters are already instantiated.
    for var in &ch.get_variables() {
        let var = st.get_var_parent(*var);
        let var_domain = st.get_var_domain(st.get_domain_id(var));
        let domain = &var_domain.domain;
        let label = st.get_label(var, true);
        let var_type = if label.starts_with(TIMEPOINT_PREFIX)
            || label.starts_with(START_TASK_PREFIX)
            || label.starts_with(END_TASK_PREFIX)
        {
            VarType::InternalTimepoint
        } else if label.starts_with(START_PREFIX) {
            VarType::ChronicleStart
        } else if label.starts_with(END_PREFIX) {
            VarType::ChronicleEnd
        } else if label.starts_with(PRESENCE_PREFIX) {
            VarType::Presence
        } else if label.starts_with(ARBITRARY_PREFIX) {
            VarType::Arbitrary
        } else if label.starts_with(RESULT_PREFIX) {
            VarType::Reification
        } else {
            VarType::Parameter(label.clone())
        };

        let (t, var_val) = &match domain {
            Simple(TYPE_ID_NIL | TYPE_ID_FALSE) => {
                (TYPE_ID_BOOLEAN, Some(VarVal::Val(Cst::Bool(false))))
            }
            Simple(TYPE_ID_TRUE) => (TYPE_ID_BOOLEAN, Some(VarVal::Val(Cst::Bool(true)))),
            Simple(t) => (*t, None),
            Domain::IntRange(l, u) => (TYPE_ID_INT, Some(VarVal::Range(*l, *u))),
            Domain::Cst(t, c) => {
                if let Simple(t) = t.deref() {
                    (*t, Some(VarVal::Val(c.clone())))
                } else {
                    unreachable!()
                }
            }
            d => {
                if d == &Domain::Union(vec![TYPE_ID_FALSE.into(), TYPE_ID_EMPTY_LIST.into()]) {
                    (TYPE_ID_BOOLEAN, Some(VarVal::Val(Cst::Bool(false))))
                } else {
                    panic!(
                        "{} is not supported in domaine def",
                        d.format(&ch.st.get_lattice())
                    )
                }
            }
        };

        let param = if let Some(var) = table.get_var(var) {
            *var
        } else if t == lattice.get_type_id(TYPE_TIMEPOINT).unwrap() {
            let fvar = ctx.model.new_optional_fvar(
                0,
                TIMEPOINT_UB.get(),
                TIME_SCALE.get(),
                prez,
                container / var_type,
            );

            table.add_binding(var, fvar.into());
            fvar.into()
        } else if *t == TYPE_ID_INT {
            let (lb, ub): (IntCst, IntCst) = if OMPAS_PLAN_ENCODING_OPTIMIZATION.get() {
                if let Some(VarVal::Range(l, u)) = var_val {
                    (*l as IntCst, *u as IntCst)
                } else {
                    (INT_CST_MIN, INT_CST_MAX)
                }
            } else {
                (INT_CST_MIN, INT_CST_MAX)
            };

            let ivar = ctx
                .model
                .new_optional_ivar(lb, ub, prez, container / var_type);
            table.add_binding(var, ivar.into());
            ivar.into()
        } else if *t == TYPE_ID_FLOAT || *t == TYPE_ID_NUMBER {
            let fvar = ctx.model.new_optional_fvar(
                INT_CST_MIN,
                INT_CST_MAX,
                TIME_SCALE.get(), //Not sure of that
                prez,
                container / var_type,
            );
            table.add_binding(var, fvar.into());
            fvar.into()
        } else if *t == TYPE_ID_BOOLEAN {
            let bvar = ctx.model.new_optional_bvar(prez, container / var_type);
            table.add_binding(var, bvar.into());
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
                .unwrap_or_else(|| {
                    panic!(
                        "{str} should be defined in type hierarchy, encoded chronicle:\n{}",
                        ch
                    )
                });
            let svar = ctx
                .model
                .new_optional_sym_var(t, prez, container / var_type);
            table.add_binding(var, svar.into());
            svar.into()
        };
        if let Some(var_val) = var_val {
            match var_val {
                VarVal::Range(_, _) => {}
                VarVal::Val(cst) => {
                    let cst = atom_from_cst(ctx, cst);
                    constraints.push(aConstraint::eq(param, cst));
                }
            }
        }
        if !params.contains(&param) {
            params.push(param);
        }
    }
    //println!("ok!");

    //End declaration of the variables

    let get_atom = |a: &VarId, context: &Ctx| -> aAtom { var_id_into_atom(a, &st, table, context) };
    /*
    CREATION of the name
     */
    //For the moment lacking the fact that we can add any kind of variables

    for x in ch.get_constraints() {
        let mut x = convert_constraint(x, prez, container, table, &st, ctx, None)?;
        constraints.append(&mut x);
    }

    //print!("init conditions...");
    for c in ch.get_conditions() {
        let fluent = ctx
            .get_fluent(
                ctx.model
                    .get_symbol_table()
                    .id(&c.sv[0].format(&st, true))
                    .unwrap(),
            )
            .unwrap()
            .clone();

        let args = c.sv[1..]
            .iter()
            .map(|a| {
                get_atom(a, ctx)
                    .try_into()
                    .unwrap_or_else(|e| panic!("{}", e))
            })
            .collect();
        let sv = StateVar { fluent, args };

        let value = get_atom(&c.value, ctx);
        let start: FAtom = get_atom(&c.get_start(), ctx).try_into()?;
        let end: FAtom = get_atom(&c.get_end(), ctx).try_into()?;
        let condition = Condition {
            start,
            state_var: sv,
            value,
            end,
        };
        conditions.push(condition);
    }
    //println!("ok!");

    //print!("init effects...");
    for e in ch.get_effects() {
        let fluent = ctx
            .get_fluent(
                ctx.model
                    .get_symbol_table()
                    .id(&e.sv[0].format(&st, true))
                    .unwrap(),
            )
            .unwrap()
            .clone();

        let args = e.sv[1..]
            .iter()
            .map(|a| {
                get_atom(a, ctx)
                    .try_into()
                    .unwrap_or_else(|e| panic!("{}", e))
            })
            .collect();
        let sv = StateVar { fluent, args };

        let value = get_atom(&e.operation.var_id, ctx);
        let operation = match &e.operation.inner {
            EffectOperationInner::Assign => EffectOp::Assign(value),
            EffectOperationInner::Increase => {
                if let Atom::Int(iatom) = value {
                    let sum = LinearSum::from(iatom);
                    EffectOp::Increase(sum)
                } else {
                    panic!("Increase support only integer variable.");
                }
            }
            EffectOperationInner::Decrease => {
                if let Atom::Int(iatom) = value {
                    let sum = -LinearSum::from(iatom);
                    EffectOp::Increase(sum)
                } else {
                    panic!("Decrease support only integer variable.");
                }
            }
        };
        let start: FAtom = get_atom(&e.get_start(), ctx).try_into()?;
        let end: FAtom = get_atom(&e.get_end(), ctx).try_into()?;

        let effect = Effect {
            transition_start: start, // + FAtom::EPSILON,
            transition_end: end,     // + FAtom::EPSILON,
            min_mutex_end: vec![],
            state_var: sv,
            operation,
        };
        effects.push(effect);
    }
    //println!("ok!");

    //print!("init subtasks...");
    for s in ch.get_subtasks() {
        let start: FAtom = get_atom(&s.interval.get_start(), ctx)
            .try_into()
            .unwrap_or_else(|t| panic!("{:?}", t));
        let end: FAtom = get_atom(&s.interval.get_end(), ctx)
            .try_into()
            .unwrap_or_else(|t| panic!("{:?}", t));
        let mut task_name = vec![get_atom(&s.result, ctx)];
        s.name.iter().for_each(|a| task_name.push(get_atom(a, ctx)));
        let st = SubTask {
            id: None,
            start,
            end,
            task_name,
        };

        subtasks.push(st);
    }

    let (start, end): (FAtom, FAtom) = (
        get_atom(&ch.get_interval().get_start(), ctx).try_into()?,
        get_atom(&ch.get_interval().get_end(), ctx).try_into()?,
    );

    let mut name: Vec<aAtom> = vec![get_atom(&ch.get_result(), ctx)];
    ch.get_name()
        .iter()
        .for_each(|a| name.push(get_atom(a, ctx)));

    let mut task: Vec<aAtom> = vec![get_atom(&ch.get_result(), ctx)];
    ch.get_task()
        .iter()
        .for_each(|a| task.push(get_atom(a, ctx)));

    let template = aChronicle {
        kind: match ch.meta_data.kind {
            ChronicleKind::Command => aChronicleKind::Action,
            ChronicleKind::Method => aChronicleKind::Method,
            ChronicleKind::Task => aChronicleKind::Action,
            ChronicleKind::Root => aChronicleKind::Problem,
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
