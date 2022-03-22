use crate::planning::structs::atom::Atom;
use crate::planning::structs::constraint::Constraint;
use crate::planning::structs::lit::Lit;
use crate::planning::structs::symbol_table::{AtomId, SymTable};
use crate::planning::structs::traits::GetVariables;
use crate::planning::structs::type_table::{AtomKind, PlanningAtomType};
use crate::planning::structs::{ConversionCollection, Problem};
use anyhow::{anyhow, Result};
use aries_core::Lit as aLit;
use aries_core::*;
use aries_model::extensions::Shaped;
use aries_model::lang::{Atom as aAtom, FAtom, FVar, IAtom, SAtom, SVar, Type, Variable};
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::constraints::Constraint as aConstraint;
use aries_planning::chronicles::{
    Chronicle as aChronicle, ChronicleInstance, ChronicleKind, ChronicleOrigin, ChronicleTemplate,
    Condition, Container, Ctx, Effect, Problem as aProblem, StateFun, SubTask, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;
use ompas_lisp::core::structs::lerror::LError;
use ompas_lisp::core::structs::lnumber::LNumber;
use ompas_lisp::core::structs::lvalues::LValueS;
use std::convert::{TryFrom, TryInto};
use std::ops::Deref;
use std::sync::Arc;

static TASK_TYPE: &str = "★task★";
static ABSTRACT_TASK_TYPE: &str = "★abstract_task★";
static ACTION_TYPE: &str = "★action★";
//static DURATIVE_ACTION_TYPE: &str = "★durative-action★";
static METHOD_TYPE: &str = "★method★";
static PREDICATE_TYPE: &str = "★predicate★";
static OBJECT_TYPE: &str = "★object★";
static STATE_FUNCTION_TYPE: &str = "★state-function★";
static FUNCTION_TYPE: &str = "★function★";
static TYPE_TYPE: &str = "★type★";
static INSTANCE_SFN: &str = "instance";

static FLOAT_SCALE: IntCst = 10;
static NIL_OBJECT: &str = "★nil★";

pub fn build_chronicles(problem: &Problem) -> Result<chronicles::Problem> {
    let mut types: Vec<(Sym, Option<Sym>)> = vec![
        (TASK_TYPE.into(), None),
        (ABSTRACT_TASK_TYPE.into(), Some(TASK_TYPE.into())),
        (ACTION_TYPE.into(), Some(TASK_TYPE.into())),
        //(DURATIVE_ACTION_TYPE.into(), Some(TASK_TYPE.into())),
        (METHOD_TYPE.into(), None),
        (PREDICATE_TYPE.into(), None),
        (FUNCTION_TYPE.into(), None),
        (STATE_FUNCTION_TYPE.into(), None),
        (OBJECT_TYPE.into(), None),
        (TYPE_TYPE.into(), None),
    ];
    //let top_type: Sym = OBJECT_TYPE.into();

    let mut symbols: Vec<TypedSymbol> = vec![];

    for t in &problem.types {
        types.push((t.into(), Some(OBJECT_TYPE.into())));
        symbols.push(TypedSymbol::new(t, TYPE_TYPE));
    }

    let th = TypeHierarchy::new(types)?;

    for (o, t) in &problem.objects {
        symbols.push(TypedSymbol::new(o, t))
    }

    symbols.push(TypedSymbol::new(INSTANCE_SFN, STATE_FUNCTION_TYPE));
    symbols.push(TypedSymbol::new(NIL_OBJECT, OBJECT_TYPE));
    for (label, _) in &problem.state_functions {
        symbols.push(TypedSymbol::new(label, STATE_FUNCTION_TYPE));
    }
    for a in &problem.actions {
        symbols.push(TypedSymbol::new(a, ACTION_TYPE));
    }
    for t in &problem.tasks {
        symbols.push(TypedSymbol::new(t, ABSTRACT_TASK_TYPE));
    }
    for m in &problem.methods {
        symbols.push(TypedSymbol::new(m, METHOD_TYPE));
    }

    let symbols = symbols
        .drain(..)
        .map(|ts| (ts.symbol, ts.tpe.unwrap_or_else(|| OBJECT_TYPE.into())))
        .collect();
    let symbol_table = SymbolTable::new(th, symbols)?;

    let mut state_functions = Vec::with_capacity(problem.state_functions.len() + 1);
    /*
     * Add state function for type.
     * instance(<object>) -> type
     */
    {
        let sym = symbol_table
            .id(INSTANCE_SFN)
            .ok_or_else(|| anyhow!("{} undefined", INSTANCE_SFN))?;
        let mut args = Vec::with_capacity(2);
        args.push(Type::Sym(
            symbol_table
                .types
                .id_of(OBJECT_TYPE)
                .ok_or_else(|| anyhow!("{} undefined.", OBJECT_TYPE))?,
        ));
        args.push(Type::Sym(
            symbol_table
                .types
                .id_of(TYPE_TYPE)
                .ok_or_else(|| anyhow!("{} undefined", TYPE_TYPE))?,
        ));
        state_functions.push(StateFun { sym, tpe: args })
    }

    for sf in &problem.state_functions {
        let sym = symbol_table
            .id(&sf.0)
            .ok_or_else(|| anyhow!("{} Unknown symbol", sf.0))?;
        let mut args = Vec::with_capacity(sf.1.get_number());
        for tpe in &sf.1.get_types() {
            let tpe = symbol_table
                .types
                .id_of(&tpe.to_string())
                .ok_or_else(|| anyhow!("{} Unknown type", sf.0))?;
            args.push(Type::Sym(tpe));
        }
        state_functions.push(StateFun { sym, tpe: args })
    }

    let mut ctx = Ctx::new(Arc::new(symbol_table), state_functions);

    let _init_container = Container::Instance(0);
    // Initial chronicle construction
    let mut init_ch = aChronicle {
        kind: ChronicleKind::Problem,
        presence: aLit::TRUE,
        start: ctx.origin(),
        end: ctx.horizon(),
        name: vec![],
        task: None,
        conditions: vec![],
        effects: vec![],
        constraints: vec![],
        subtasks: vec![],
    };

    initialize_state(&mut init_ch, problem, &ctx);
    initialize_goal_task(&mut init_ch, problem, &mut ctx);

    /*
    Goals: Add subtask
     */

    let init_ch = ChronicleInstance {
        parameters: vec![],
        origin: ChronicleOrigin::Original,
        chronicle: init_ch,
    };

    let mut templates: Vec<ChronicleTemplate> = Vec::new();
    for t in &problem.cc.chronicle_templates {
        let cont = Container::Template(templates.len());
        let template = read_chronicle(cont, t, &problem.cc, &mut ctx)?;
        templates.push(template);
    }

    let problem = aProblem {
        context: ctx,
        templates,
        chronicles: vec![init_ch],
    };

    Ok(problem)
}

fn satom_from_lvalues(ctx: &Ctx, v: &LValueS) -> SAtom {
    let v: String = v.try_into().expect("");
    ctx.typed_sym(ctx.model.get_symbol_table().id(&v).unwrap())
        .into()
}

fn atom_from_lvalues(ctx: &Ctx, v: &LValueS) -> aAtom {
    match v {
        LValueS::Symbol(s) => {
            SAtom::from(ctx.typed_sym(ctx.model.get_symbol_table().id(s.as_str()).unwrap())).into()
        }
        LValueS::Int(i) => IAtom::from(*i as i32).into(),
        LValueS::Float(f) => {
            let f: i32 = (f * FLOAT_SCALE as f64) as i32;
            FAtom::new(IAtom::from(f), FLOAT_SCALE).into()
        }
        LValueS::Bool(b) => match b {
            true => aLit::TRUE.into(),
            false => aLit::FALSE.into(),
        },
        _ => panic!(""),
    }
}

/**
Add initial state from RAEStateSnapshot
 */
fn initialize_state(init_ch: &mut aChronicle, p: &Problem, ctx: &Ctx) {
    /*
    Initialisation of instance state variable
     */
    let state = &p.initial_state;
    for (key, value) in &state.instance.inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        assert_eq!(key.len(), 2);
        assert_eq!(&key[1].to_string(), INSTANCE_SFN);
        let sf: SAtom = satom_from_lvalues(ctx, &key[0]);
        let object: SAtom = satom_from_lvalues(ctx, &key[1]);
        let value: aAtom = atom_from_lvalues(ctx, value);
        let sv = vec![sf, object];

        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            state_var: sv,
            value: value,
        });
    }

    /*
    Initialisation of static state variables
     */
    //We suppose for the moment that all args of state variable are objects
    for (key, value) in &state._static.inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        let key: Vec<SAtom> = key.iter().map(|lv| satom_from_lvalues(ctx, lv)).collect();
        let value = atom_from_lvalues(ctx, &value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            state_var: key,
            value: value,
        });
    }

    /*
    Initialisation of dynamic state variables
     */

    for (key, value) in &state.dynamic.inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        let key: Vec<SAtom> = key.iter().map(|v| satom_from_lvalues(ctx, v)).collect();
        let value = atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            state_var: key,
            value: value,
        });
    }

    /*
    Initilisation of inner world
    */
    for (key, value) in &state.inner_world.inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        let key: Vec<SAtom> = key.iter().map(|v| satom_from_lvalues(ctx, v)).collect();
        let value = atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            state_var: key,
            value: value,
        });
    }
}

fn initialize_goal_task(init_ch: &mut aChronicle, p: &Problem, ctx: &mut Ctx) {
    let c = Container::Instance(0);
    for t in &p.goal_tasks {
        let t: Vec<LValueS> = t.try_into().expect("");
        let task_name: Vec<SAtom> = t.iter().map(|v| satom_from_lvalues(ctx, v)).collect();

        let prez = init_ch.presence;
        let start =
            ctx.model
                .new_optional_fvar(0, INT_CST_MAX, TIME_SCALE, prez, c / VarType::TaskStart);
        let end =
            ctx.model
                .new_optional_fvar(0, INT_CST_MAX, TIME_SCALE, prez, c / VarType::TaskEnd);
        let start = FAtom::from(start);
        let end = FAtom::from(end);
        let id = None;
        let st = SubTask {
            id,
            start,
            end,
            task_name,
        };
        init_ch.subtasks.push(st);
    }
}

#[derive(Default)]
pub struct BindingAriesAtoms {
    inner: im::HashMap<AtomId, Variable>,
    reverse: im::HashMap<Variable, AtomId>,
}

impl BindingAriesAtoms {
    pub fn add_binding(&mut self, id: &AtomId, var: &Variable) {
        self.inner.insert(*id, *var);
        self.reverse.insert(*var, *id);
    }

    pub fn get_var(&self, id: &AtomId) -> Option<&Variable> {
        self.inner.get(id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&AtomId> {
        self.reverse.get(var)
    }
}

fn convert_constraint(bindings: &BindingAriesAtoms, x: &Constraint) -> Result<aConstraint, LError> {
    match x {
        Constraint::LEq(a, b) => {
            let a: AtomId = a.try_into()?;
            let b: AtomId = b.try_into()?;
            let a: SVar = bindings.get_var(&a).expect("").try_into()?;
            let b: SVar = bindings.get_var(&b).expect("").try_into()?;
            Ok(aConstraint::leq(SAtom::from(a), SAtom::from(b)))
        }
        Constraint::Eq(a, b) => {
            let a: AtomId = a.try_into()?;
            let a: SVar = bindings.get_var(&a).expect("").try_into()?;
            match b {
                Lit::Atom(b) => {
                    let b: SVar = bindings.get_var(&b).expect("").try_into()?;
                    Ok(aConstraint::eq(SAtom::from(a), SAtom::from(b)))
                }
                Lit::Constraint(c) => Ok(aConstraint::reify(
                    a,
                    convert_constraint(bindings, c.deref())?,
                )),
                Lit::Exp(_) => Err(Default::default()),
            }
        }
        Constraint::Neg(_) => panic!("not supported yet"),
        Constraint::LT(a, b) => {
            let a: AtomId = a.try_into()?;
            let b: AtomId = b.try_into()?;
            let a: SVar = bindings.get_var(&a).expect("").try_into()?;
            let b: SVar = bindings.get_var(&b).expect("").try_into()?;
            Ok(aConstraint::lt(SAtom::from(a), SAtom::from(b)))
        }
        Constraint::And(_, _)
        | Constraint::Or(_, _)
        | Constraint::Type(_, _)
        | Constraint::Arbitrary(_, _) => Err(Default::default()),
    }
}

fn atom_id_into_atom(
    a: &AtomId,
    sym_table: &SymTable,
    bindings: &BindingAriesAtoms,
    context: &Ctx,
) -> aAtom {
    let ompas_atom = sym_table.get_atom(a, false).unwrap();
    let ompas_type = sym_table.get_type_of(a).unwrap();

    match ompas_atom {
        Atom::Bool(b) => match b {
            true => aLit::TRUE.into(),
            false => aLit::FALSE.into(),
        },
        Atom::Number(n) => match n {
            LNumber::Int(i) => IAtom::from(*i as i32).into(),
            LNumber::Float(f) => {
                let f: i32 = (f * FLOAT_SCALE as f64) as i32;
                FAtom::new(IAtom::from(f), FLOAT_SCALE).into()
            }
            LNumber::Usize(u) => IAtom::from(*u as i32).into(),
        },
        Atom::Sym(s) => match ompas_type.kind {
            AtomKind::Constant => context
                .typed_sym(context.model.get_symbol_table().id(s.get_sym()).unwrap())
                .into(),
            AtomKind::Variable(_) => (*bindings.get_var(a).unwrap()).into(),
        },
    }
}

fn read_chronicle(
    c: Container,
    chronicle: &crate::planning::structs::chronicle::ChronicleTemplate,
    ch: &ConversionCollection,
    context: &mut Ctx,
) -> Result<ChronicleTemplate> {
    let mut bindings = BindingAriesAtoms::default();

    let _top_type: Sym = OBJECT_TYPE.into();
    let mut params: Vec<Variable> = Vec::new();
    // Declaration of the presence variable

    //Declaration of the variables
    let prez_var = context.model.new_bvar(c / VarType::Presence);
    bindings.add_binding(chronicle.get_presence(), &prez_var.into());
    let prez = prez_var.true_lit();

    //TODO: handle case where some parameters are already instantiated.
    for var in &chronicle.get_variables() {
        let t = ch.sym_table.get_type_of(var).unwrap();
        let param: Variable = match &t.a_type.unwrap() {
            PlanningAtomType::Timepoint => {
                //TODO: separate start, end and other timepoints
                let fvar = context.model.new_optional_fvar(
                    0,
                    INT_CST_MAX,
                    TIME_SCALE,
                    prez,
                    c / VarType::Parameter,
                );

                bindings.add_binding(var, &fvar.into());
                fvar.into()
            }
            PlanningAtomType::Presence => {
                if let Some(var) = bindings.get_var(var) {
                    *var
                } else {
                    panic!()
                }
            }
            PlanningAtomType::Int => {
                let ivar = context.model.new_optional_ivar(
                    INT_CST_MIN,
                    INT_CST_MAX,
                    prez,
                    c / VarType::Parameter,
                );
                bindings.add_binding(var, &ivar.into());
                ivar.into()
            }
            PlanningAtomType::Float => {
                let fvar = context.model.new_optional_fvar(
                    INT_CST_MIN,
                    INT_CST_MAX,
                    TIME_SCALE, //Not sure of that
                    prez,
                    c / VarType::Parameter,
                );
                bindings.add_binding(var, &fvar.into());
                fvar.into()
            }
            PlanningAtomType::Bool => {
                let bvar = context
                    .model
                    .new_optional_bvar(prez, c / VarType::Parameter);
                bindings.add_binding(var, &bvar.into());
                bvar.into()
            }
            PlanningAtomType::Object => {
                let t = context
                    .model
                    .get_symbol_table()
                    .types
                    .id_of(OBJECT_TYPE)
                    .expect("object should be defined in type hierarchy");
                //let s = ch.sym_table.get_atom(var, true).unwrap();
                let svar = context
                    .model
                    .new_optional_sym_var(t, prez, c / VarType::Parameter);
                bindings.add_binding(var, &svar.into());
                svar.into()
            }
            PlanningAtomType::Other(t) => {
                let t_symbol = ch.sym_table.get_atom(t, false).unwrap();
                let t = context
                    .model
                    .get_symbol_table()
                    .types
                    .id_of(&t_symbol.to_string())
                    .expect("object should be defined in type hierarchy");
                //let s = ch.sym_table.get_atom(var, true).unwrap();
                let svar = context
                    .model
                    .new_optional_sym_var(t, prez, c / VarType::Parameter);
                bindings.add_binding(var, &svar.into());
                svar.into()
            }
            pat => panic!("a parameter cannot be a {}", pat),
        };
        params.push(param);
    }
    //End declaration of the variables

    let id_into_satom = |a: &AtomId| -> SAtom {
        SVar::try_from(bindings.get_var(a).unwrap())
            .expect("")
            .into()
    };
    /*
    CREATION of the name
     */
    //For the moment lacking the fact that we can add any kind of variables
    let mut name: Vec<SAtom> = vec![];
    for (i, p) in chronicle.name.iter().enumerate() {
        if i == 0 {
            name.push(
                context
                    .typed_sym(
                        context
                            .model
                            .get_symbol_table()
                            .id(&ch.sym_table.get_atom(p, false).unwrap().to_string())
                            .unwrap(),
                    )
                    .into(),
            )
        } else {
            name.push(SVar::try_from(bindings.get_var(p).unwrap())?.into())
        }
    }

    let mut constraints: Vec<aConstraint> = vec![];
    let mut conditions: Vec<Condition> = vec![];
    let mut effects: Vec<Effect> = vec![];
    let mut subtasks: Vec<SubTask> = vec![];

    let get_atom =
        |a: &AtomId| -> aAtom { atom_id_into_atom(a, &ch.sym_table, &bindings, &context) };

    for x in chronicle.get_constraints() {
        let x = convert_constraint(&bindings, x)?;
        constraints.push(x);
    }

    for c in chronicle.get_conditions() {
        let sv: Vec<Lit> = (&c.sv).try_into()?;
        let sv = sv
            .iter()
            .map(|l| {
                let a: AtomId = l.try_into().expect("");
                id_into_satom(&a)
            })
            .collect();
        let value = atom_id_into_atom(
            &AtomId::try_from(&c.value)?,
            &ch.sym_table,
            &bindings,
            context,
        );
        let start = FVar::try_from(bindings.get_var(c.get_start()).unwrap())?;
        let start = FAtom::from(start);
        let end = FVar::try_from(bindings.get_var(c.get_end()).unwrap())?;
        let end = FAtom::from(end);
        let condition = Condition {
            start,
            state_var: sv,
            value: value.into(),
            end,
        };
        conditions.push(condition);
    }

    for e in chronicle.get_effects() {
        let sv: Vec<Lit> = (&e.sv).try_into()?;
        let sv = sv
            .iter()
            .map(|l| {
                let a: AtomId = l.try_into().expect("");
                id_into_satom(&a)
            })
            .collect();
        let value = atom_id_into_atom(
            &AtomId::try_from(&e.value)?,
            &ch.sym_table,
            &bindings,
            context,
        );
        let start = FVar::try_from(bindings.get_var(e.get_start()).unwrap())?;
        let start = FAtom::from(start);
        let end = FVar::try_from(bindings.get_var(e.get_end()).unwrap())?;
        let end = FAtom::from(end);
        let effect = Effect {
            transition_start: start,
            persistence_start: end,
            state_var: sv,
            value: value.into(),
        };
        effects.push(effect);
    }

    for s in chronicle.get_subtasks() {
        let start: FAtom = get_atom(s.interval.start()).try_into()?;
        let end: FAtom = get_atom(s.interval.end()).try_into()?;
        let e: Vec<Lit> = (&s.lit).try_into()?;
        let e: Vec<SAtom> = e
            .iter()
            .map(|l| {
                let a: AtomId = l.try_into().expect("");
                get_atom(&a).try_into().expect("")
            })
            .collect();
        let st = SubTask {
            id: None,
            start,
            end,
            task_name: e,
        };

        subtasks.push(st);
    }

    let start = FVar::try_from(bindings.get_var(chronicle.get_start()).unwrap())?;
    let start = FAtom::from(start);
    let end = FVar::try_from(bindings.get_var(chronicle.get_end()).unwrap())?;
    let end = FAtom::from(end);

    let task: Vec<SAtom> = chronicle
        .task
        .iter()
        .map(|a| get_atom(a).try_into().expect(""))
        .collect();

    let template = aChronicle {
        kind: chronicle.chronicle_kind,
        presence: prez,
        start,
        end,
        name,
        task: Some(task),
        conditions,
        effects,
        constraints,
        subtasks,
    };

    let template = ChronicleTemplate {
        label: None,
        parameters: params,
        chronicle: template,
    };

    Ok(template)
}
