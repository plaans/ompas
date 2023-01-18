pub mod solver;

use anyhow::{anyhow, Result};
use aries_core::{IntCst, Lit as aLit, INT_CST_MAX, INT_CST_MIN};
use aries_model::extensions::Shaped;
use aries_model::lang::{
    Atom as aAtom, ConversionError, FAtom, FVar, IAtom, SAtom, SVar, Type as aType, Variable,
};
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::constraints::Constraint as aConstraint;
use aries_planning::chronicles::VarType::Reification;
use aries_planning::chronicles::{
    Chronicle as aChronicle, ChronicleInstance, ChronicleKind, ChronicleOrigin,
    ChronicleTemplate as aChronicleTemplate, Condition, Container, Ctx, Effect,
    Problem as aProblem, StateFun, SubTask, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;
use ompas_rae_language::exec::state::{INSTANCE, INSTANCES};
use ompas_rae_structs::conversion::chronicle::constraint::Constraint;
use ompas_rae_structs::conversion::chronicle::template::ChronicleTemplate;
use ompas_rae_structs::conversion::chronicle::{FormatWithSymTable, GetVariables};
use ompas_rae_structs::planning::problem::PlanningProblem;
use ompas_rae_structs::state::partial_state::PartialState;
use ompas_rae_structs::state::world_state::WorldStateSnapshot;
use ompas_rae_structs::sym_table::domain::basic_type::{
    TYPE_ID_BOOLEAN, TYPE_ID_FLOAT, TYPE_ID_INT,
};
use ompas_rae_structs::sym_table::domain::cst::Cst;
use ompas_rae_structs::sym_table::domain::type_lattice::TypeLattice;
use ompas_rae_structs::sym_table::domain::Domain;
use ompas_rae_structs::sym_table::lit::Lit;
use ompas_rae_structs::sym_table::r#ref::RefSymTable;
use ompas_rae_structs::sym_table::{
    VarId, MAX_Q, QUANTITY, TYPE_ABSTRACT_TASK, TYPE_COMMAND, TYPE_METHOD, TYPE_OBJECT,
    TYPE_OBJECT_TYPE, TYPE_PREDICATE, TYPE_PRESENCE, TYPE_STATE_FUNCTION, TYPE_TASK,
    TYPE_TIMEPOINT,
};
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalues::LValueS;
use std::borrow::Borrow;
use std::convert::TryInto;
use std::sync::Arc;

static BUILD_CHRONICLES: &str = "build_chronicles";
pub const FLOAT_SCALE: IntCst = 2;

fn get_type(lattice: &TypeLattice, st: &SymbolTable, t: &Domain) -> lruntimeerror::Result<aType> {
    match t {
        Domain::Simple(t) => match *t {
            TYPE_ID_BOOLEAN => Ok(aType::Bool),
            TYPE_ID_INT => Ok(aType::Int),
            TYPE_ID_FLOAT => Ok(aType::Fixed(FLOAT_SCALE)),
            t => {
                let other: String = lattice.format_type(&t);
                match st.types.id_of(&other) {
                    Some(t) => Ok(aType::Sym(t)),
                    None => Err(lruntimeerror!(
                        BUILD_CHRONICLES,
                        format!("{} Unknown type", other)
                    )),
                }
            }
        },
        _ => todo!(),
    }
}

pub fn generate_templates(problem: &PlanningProblem) -> anyhow::Result<chronicles::Problem> {
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

    //Add TypeHierarchy
    for t in &domain.new_types {
        let sym = lattice.format_type(&t);
        let parent = lattice.format_type(&lattice.get_parent(&t).first().unwrap());
        types.push((sym.to_string().into(), Some(parent.into())));
        symbols.push(TypedSymbol::new(sym, TYPE_OBJECT_TYPE));
    }

    /*for t in &problem.types {
        types.push((t.into(), Some(OBJECT_TYPE.into())));
        symbols.push(TypedSymbol::new(t, TYPE_TYPE));
    }*/

    let th = TypeHierarchy::new(types)?;

    for (t, instances) in &instance.state.instance.inner {
        for sym in instances {
            symbols.push(TypedSymbol::new(sym, t));
        }
    }

    //Adding custom state functions
    symbols.push(TypedSymbol::new(INSTANCE, TYPE_STATE_FUNCTION));
    symbols.push(TypedSymbol::new(INSTANCES, TYPE_STATE_FUNCTION));
    symbols.push(TypedSymbol::new(QUANTITY, TYPE_STATE_FUNCTION));
    symbols.push(TypedSymbol::new(MAX_Q, TYPE_STATE_FUNCTION));
    //symbols.push(TypedSymbol::new(NIL_OBJECT, OBJECT_TYPE));
    for sf in &domain.sf {
        symbols.push(TypedSymbol::new(sf.get_label(), TYPE_STATE_FUNCTION));
    }
    for command in &domain.commands {
        symbols.push(TypedSymbol::new(command.command.get_label(), TYPE_COMMAND));
    }
    for task in &domain.tasks {
        symbols.push(TypedSymbol::new(task.task.get_label(), TYPE_ABSTRACT_TASK));
    }
    for method in &domain.methods {
        symbols.push(TypedSymbol::new(
            method.method.label.to_string(),
            TYPE_METHOD,
        ));
    }

    let symbols = symbols
        .drain(..)
        .map(|ts| (ts.symbol, ts.tpe.unwrap_or_else(|| TYPE_OBJECT.into())))
        .collect();
    let symbol_table = SymbolTable::new(th, symbols)?;

    //We have 4 synthetic state functions.
    let mut state_functions = Vec::with_capacity(domain.sf.len() + 4);
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
        args.push(aType::Fixed(FLOAT_SCALE));
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
        args.push(aType::Fixed(FLOAT_SCALE));
        state_functions.push(StateFun { sym, tpe: args })
    }

    for sf in &domain.sf {
        let sym = symbol_table.id(sf.get_label()).ok_or_else(|| {
            lruntimeerror!(
                BUILD_CHRONICLES,
                format!("{} Unknown symbol", sf.get_label())
            )
        })?;
        let mut args = Vec::with_capacity(sf.parameters.get_number());
        for tpe in &sf.parameters.get_type_domain() {
            args.push(get_type(&lattice, &symbol_table, tpe)?);
        }
        state_functions.push(StateFun { sym, tpe: args })
    }

    let mut ctx = Ctx::new(Arc::new(symbol_table), state_functions);

    let mut bindings = BindingAriesAtoms::default();

    let mut templates: Vec<aChronicleTemplate> = Vec::new();
    let cont = Container::Template(domain.commands.len() + domain.methods.len());

    for command in &domain.commands {
        let template = read_chronicle(&mut ctx, &mut bindings, &command.template, cont.clone())?;
        //println!("template {}: {:?}", templates.len(), template.chronicle);
        templates.push(template);
    }

    for method in &domain.methods {
        let template = read_chronicle(&mut ctx, &mut bindings, &method.template, cont.clone())?;
        //println!("template {}: {:?}", templates.len(), template.chronicle);
        templates.push(template);
    }

    Ok(aProblem {
        context: ctx,
        templates,
        chronicles: vec![],
    })
}

fn satom_from_lvalues(ctx: &Ctx, v: &LValueS) -> SAtom {
    let v: String = v.try_into().expect("");
    ctx.typed_sym(
        ctx.model
            .get_symbol_table()
            .id(&v)
            .unwrap_or_else(|| panic!("{} undefined in symbol table", v)),
    )
    .into()
}

fn atom_from_lvalues(ctx: &Ctx, v: &LValueS) -> aAtom {
    match v {
        LValueS::Symbol(s) => {
            let id = ctx
                .model
                .get_symbol_table()
                .id(s.as_str())
                .unwrap_or_else(|| panic!("{} should have been defined as a parameter", s));
            SAtom::from(ctx.typed_sym(id)).into()
            //SAtom::from(ctx.typed_sym(ctx.model.get_symbol_table().id(s.as_str()).unwrap())).into()
        }
        LValueS::Int(i) => IAtom::from(*i as i32).into(),
        LValueS::Float(f) => {
            let f: i32 = (f * FLOAT_SCALE as f64) as i32;
            FAtom::new(IAtom::from(f), FLOAT_SCALE).into()
        }
        LValueS::Bool(b) => match b {
            false => aLit::FALSE.into(),
            true => aLit::TRUE.into(),
        },
        LValueS::List(_) => panic!("cannot convert LValueS::List into atom: {}", v),
        LValueS::Map(_) => panic!("cannot convert LValueS::Map into atom: {}", v),
    }
}

pub fn create_initial_chronicle(
    goal_tasks: &[LValueS],
    state: &WorldStateSnapshot,
    ctx: &mut Ctx,
) -> ChronicleInstance {
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
        cost: None,
    };

    initialize_state(&mut init_ch, state, ctx);
    initialize_goal_task(&mut init_ch, goal_tasks, ctx);

    //println!("problem initialized");

    /*
    Goals: Add subtask
     */

    ChronicleInstance {
        parameters: vec![],
        origin: ChronicleOrigin::Original,
        chronicle: init_ch,
    }
}

/**
Add initial state from RAEStateSnapshot
 */
fn initialize_state(init_ch: &mut aChronicle, state: &WorldStateSnapshot, ctx: &Ctx) {
    /*
    Initialisation of instance state variable
     */
    for (key, value) in &PartialState::from(state.instance.clone()).inner {
        let key: Vec<LValueS> = key.try_into().expect("");
        assert_eq!(key.len(), 2);
        assert_eq!(&key[0].to_string(), INSTANCE);
        let sf: SAtom = satom_from_lvalues(ctx, &key[0]);
        let t: SAtom = satom_from_lvalues(ctx, &key[1]);
        let objects: Vec<LValueS> = value.try_into().expect("");
        for obj in &objects {
            let object: SAtom = satom_from_lvalues(ctx, obj);
            let sv = vec![sf, object];
            init_ch.effects.push(Effect {
                transition_start: init_ch.start,
                persistence_start: init_ch.start,
                min_persistence_end: vec![],
                state_var: sv,
                value: t.into(),
            });
        }
    }

    /*
    Initialisation of static state variables
     */
    //We suppose for the moment that all args of state variable are objects
    for (key, value) in &state._static.inner {
        let key: Vec<SAtom> = match key {
            LValueS::List(vec) => vec.iter().map(|lv| satom_from_lvalues(ctx, lv)).collect(),
            LValueS::Symbol(_) => vec![satom_from_lvalues(ctx, key)],
            _ => panic!("state variable is either a symbol or a list of symbols"),
        };
        let value = atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            min_persistence_end: vec![],
            state_var: key,
            value,
        });
    }

    /*
    Initialisation of dynamic state variables
     */

    for (key, value) in &state.dynamic.inner {
        let key: Vec<SAtom> = match key {
            LValueS::List(vec) => vec.iter().map(|lv| satom_from_lvalues(ctx, lv)).collect(),
            LValueS::Symbol(_) => vec![satom_from_lvalues(ctx, key)],
            _ => panic!("state variable is either a symbol or a list of symbols"),
        };
        let value = atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            min_persistence_end: vec![],
            state_var: key,
            value,
        });
    }

    /*
    Initilisation of inner world
    */
    for (key, value) in &state.inner_world.inner {
        let key: Vec<SAtom> = match key {
            LValueS::List(vec) => vec.iter().map(|lv| satom_from_lvalues(ctx, lv)).collect(),
            LValueS::Symbol(_) => vec![satom_from_lvalues(ctx, key)],
            _ => panic!("state variable is either a symbol or a list of symbols"),
        };
        let value = atom_from_lvalues(ctx, value);
        init_ch.effects.push(Effect {
            transition_start: init_ch.start,
            persistence_start: init_ch.start,
            min_persistence_end: vec![],
            state_var: key,
            value,
        });
    }
}

fn initialize_goal_task(init_ch: &mut aChronicle, goal_tasks: &[LValueS], ctx: &mut Ctx) {
    let c = Container::Instance(0);
    for t in goal_tasks {
        let t: Vec<LValueS> = t.try_into().expect("");
        let task_name: Vec<SAtom> = t.iter().map(|v| satom_from_lvalues(ctx, v)).collect();

        let prez = init_ch.presence;
        let start = ctx.model.new_optional_fvar(
            0,
            INT_CST_MAX,
            TIME_SCALE,
            prez,
            c / VarType::TaskStart(0),
        );
        let end =
            ctx.model
                .new_optional_fvar(0, INT_CST_MAX, TIME_SCALE, prez, c / VarType::TaskEnd(0));
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
    inner: im::HashMap<VarId, Variable>,
    reverse: im::HashMap<Variable, VarId>,
}

impl BindingAriesAtoms {
    pub fn add_binding(&mut self, id: &VarId, var: &Variable) {
        self.inner.insert(*id, *var);
        self.reverse.insert(*var, *id);
    }

    pub fn get_var(&self, id: &VarId) -> Option<&Variable> {
        self.inner.get(id)
    }

    pub fn get_id(&self, var: &Variable) -> Option<&VarId> {
        self.reverse.get(var)
    }
}

impl FormatWithSymTable for BindingAriesAtoms {
    fn format(&self, st: &RefSymTable, sym_version: bool) -> String {
        let mut str = "#BINDINGS: \n".to_string();
        for (var, id) in &self.reverse {
            str.push_str(
                format!(
                    "{:?} <- {}\n",
                    aAtom::from(*var),
                    id.format(st, sym_version)
                )
                .as_str(),
            )
        }
        str
    }
}

#[allow(dead_code)]
fn convert_constraint(
    x: &Constraint,
    prez: aLit,
    container: Container,
    bindings: &BindingAriesAtoms,
    st: &RefSymTable,
    ctx: &mut Ctx,
) -> lruntimeerror::Result<Vec<aConstraint>> {
    let get_atom = |a: &VarId, ctx| -> aAtom { var_id_into_atom(a, st, bindings, ctx) };
    let mut constraints = vec![];
    match x {
        Constraint::Leq(a, b) => {
            let a: VarId = a.try_into()?;
            let b: VarId = b.try_into()?;
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
            ));
            //constraints.push(aConstraint::or(vec![lt, eq]));
        }
        Constraint::Eq(a, b) => {
            //let a: VarId = a.try_into()?;
            match (a, b) {
                (Lit::Atom(a), Lit::Atom(b)) => {
                    constraints.push(aConstraint::eq(get_atom(a, ctx), get_atom(b, ctx)))
                }
                (Lit::Atom(_), Lit::Constraint(_)) => {}
                (Lit::Constraint(_), Lit::Atom(_)) => {}
                (Lit::Constraint(_), Lit::Constraint(_)) => {}
                _ => Err(LRuntimeError::default())?,
            }
        }
        Constraint::Not(a) => {
            let a: VarId = a.try_into()?;
            constraints.push(aConstraint::eq(get_atom(&a, ctx), aLit::FALSE))
        }
        Constraint::Lt(a, b) => {
            let a: VarId = a.try_into()?;
            let b: VarId = b.try_into()?;
            constraints.push(aConstraint::lt(get_atom(&a, ctx), get_atom(&b, ctx)))
        }
        Constraint::And(_)
        | Constraint::Or(_)
        | Constraint::Type(_, _)
        | Constraint::Arbitrary(_) => Err(LRuntimeError::default())?,
        Constraint::Neq(a, b) => {
            let a: VarId = a.try_into()?;
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
                Lit::Exp(_) => Err(LRuntimeError::default())?,
                _ => {}
            }
        }
        Constraint::Min(_) => {}
        Constraint::Max(_) => {}
    };
    Ok(constraints)
}

#[allow(dead_code)]
fn var_id_into_atom(a: &VarId, st: &RefSymTable, bindings: &BindingAriesAtoms, ctx: &Ctx) -> aAtom {
    let ompas_var = st.get_var_domain(a);
    let domain = &ompas_var.domain;
    if domain.is_true() {
        aLit::TRUE.into()
    } else if domain.is_false() {
        aLit::FALSE.into()
    } else if let Domain::Cst(_t, cst) = domain {
        match cst {
            Cst::Int(i) => IAtom::from(*i as i32).into(),
            Cst::Float(f) => {
                let f: i32 = (f * FLOAT_SCALE as f64) as i32;
                FAtom::new(IAtom::from(f), FLOAT_SCALE).into()
            }
            Cst::Symbol(s) => ctx
                .typed_sym(
                    ctx.model
                        .get_symbol_table()
                        .id(s)
                        .unwrap_or_else(|| panic!("{} is not defined in symbol table", s)),
                )
                .into(),
        }
    } else {
        (*bindings
            .get_var(a)
            .unwrap_or_else(|| panic!("{} undefined in bindings", st.format_variable(a))))
        .into()
    }
}

fn read_chronicle(
    context: &mut Ctx,
    bindings: &mut BindingAriesAtoms,
    ch: &ChronicleTemplate,
    container: Container,
) -> Result<aChronicleTemplate> {
    /*println!(
        "reading chronicle: {}",
        chronicle.format_with_sym_table(&ch.sym_table, false)
    );*/

    let st = ch.st.clone();
    let lattice = st.get_lattice();

    let mut params: Vec<Variable> = Vec::new();
    // Declaration of the presence variable

    //Declaration of the variables
    let prez_var = context.model.new_bvar(container / VarType::Presence);
    bindings.add_binding(ch.get_presence(), &prez_var.into());
    let prez = prez_var.true_lit();

    //print!("init params...");
    //TODO: handle case where some parameters are already instantiated.
    for var in &ch.get_variables() {
        let var_domain = st.get_var_domain(var);
        let domain = &var_domain.domain;
        let label = st.get_label(var, false);

        let param: Variable = match domain {
            Domain::Simple(t) => {
                if t == lattice.get_type_id(TYPE_TIMEPOINT).unwrap() {
                    let fvar = context.model.new_optional_fvar(
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
                    let ivar = context.model.new_optional_ivar(
                        INT_CST_MIN,
                        INT_CST_MAX,
                        prez,
                        container / VarType::Parameter(label),
                    );
                    bindings.add_binding(var, &ivar.into());
                    ivar.into()
                } else if *t == TYPE_ID_FLOAT {
                    let fvar = context.model.new_optional_fvar(
                        INT_CST_MIN,
                        INT_CST_MAX,
                        TIME_SCALE, //Not sure of that
                        prez,
                        container / VarType::Parameter(label),
                    );
                    bindings.add_binding(var, &fvar.into());
                    fvar.into()
                } else if *t == TYPE_ID_BOOLEAN {
                    let bvar = context
                        .model
                        .new_optional_bvar(prez, container / VarType::Parameter(label));
                    bindings.add_binding(var, &bvar.into());
                    bvar.into()
                } else {
                    let t = context
                        .model
                        .get_symbol_table()
                        .types
                        .id_of(&domain.format(&lattice))
                        .expect("object should be defined in type hierarchy");
                    //let s = ch.sym_table.get_atom(var, true).unwrap();
                    let svar = context.model.new_optional_sym_var(
                        t,
                        prez,
                        container / VarType::Parameter(label),
                    );
                    bindings.add_binding(var, &svar.into());
                    svar.into()
                }
            }
            Domain::Cst(_, _) => {
                todo!()
            }
            _ => unreachable!(),
        };
        params.push(param);
    }
    //println!("ok!");

    //End declaration of the variables

    /*
    CREATION of the name
     */
    //For the moment lacking the fact that we can add any kind of variables
    //print!("init name...");
    let mut name: Vec<SAtom> = vec![];
    for (i, p) in ch.get_name().iter().enumerate() {
        if i == 0 {
            name.push(
                context
                    .typed_sym(
                        context
                            .model
                            .get_symbol_table()
                            .id(&st.format_variable(p).to_string())
                            .unwrap(),
                    )
                    .into(),
            )
        } else {
            name.push(SVar::try_from(*bindings.get_var(p).unwrap())?.into())
        }
    }
    //println!("ok!");

    let mut constraints: Vec<aConstraint> = vec![];
    let mut conditions: Vec<Condition> = vec![];
    let mut effects: Vec<Effect> = vec![];
    let mut subtasks: Vec<SubTask> = vec![];

    let get_atom = |a: &VarId, context| -> aAtom { var_id_into_atom(a, &st, bindings, context) };

    for x in ch.get_constraints() {
        let mut x = convert_constraint(x, prez, container, bindings, &st, context)?;
        constraints.append(&mut x);
    }

    //print!("init conditions...");
    for c in ch.get_conditions() {
        let sv =
            c.sv.iter()
                .map(|a| {
                    get_atom(a, context)
                        .try_into()
                        .unwrap_or_else(|e| panic!("{}", e))
                })
                .collect();
        let value = get_atom(&c.value, context);
        let start: FVar =
            try_variable_into_fvar(bindings.get_var(&c.get_start()).cloned().unwrap())?;
        let start = FAtom::from(start);
        let end: FVar = try_variable_into_fvar(bindings.get_var(&c.get_end()).cloned().unwrap())?;
        let end = FAtom::from(end);
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
        let sv =
            e.sv.iter()
                .map(|a| {
                    get_atom(a, context)
                        .try_into()
                        .unwrap_or_else(|e| panic!("{}", e))
                })
                .collect();
        let value = get_atom(&e.value, context);
        let start: FVar = try_variable_into_fvar(*bindings.get_var(&e.get_start()).unwrap())?;
        let start = FAtom::from(start);
        let end: FVar = try_variable_into_fvar(*bindings.get_var(&e.get_end()).unwrap())?;
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
    //println!("ok!");

    //print!("init subtasks...");
    for s in ch.get_subtasks() {
        let start: FAtom = get_atom(&s.interval.get_start(), context).try_into()?;
        let end: FAtom = get_atom(&s.interval.get_end(), context).try_into()?;
        let e: Vec<Lit> = s.lit.borrow().try_into()?;
        let e: Vec<SAtom> = e
            .iter()
            .map(|l| {
                let a: VarId = l.try_into().expect("");
                get_atom(&a, context).try_into().expect("")
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
    //println!("ok!");

    let start = try_variable_into_fvar(
        bindings
            .get_var(&ch.get_interval().get_start())
            .unwrap()
            .clone(),
    )?;
    let start = FAtom::from(start);
    let end = try_variable_into_fvar(
        bindings
            .get_var(&ch.get_interval().get_end())
            .unwrap()
            .clone(),
    )?;
    let end = FAtom::from(end);

    //print!("init task...");
    let task: Vec<SAtom> = ch
        .get_task()
        .iter()
        .map(|a| get_atom(a, context).try_into().expect(""))
        .collect();
    //println!("ok!");
    //print!("\n\n");

    let template = aChronicle {
        kind: ChronicleKind::Method,
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

pub fn try_variable_into_fvar(variable: Variable) -> Result<FVar, ConversionError> {
    if let Variable::Fixed(fvar) = variable {
        Ok(fvar)
    } else {
        Err(ConversionError::TypeError)
    }
}
/*
pub fn generate_chronicles(_problem: &Problem) -> Result<chronicles::Problem> {
    todo!();
    /*println!("# SYMBOL TABLE: \n{:?}", ctx.model.get_symbol_table());
    println!("{}", bindings.format(&problem.cc.sym_table, false));
    println!("initial chronicle: {:?}", init_ch.chronicle);

    for (i, t) in templates.iter().enumerate() {
        println!("template {}: {:?}", i, t.chronicle)
    }*/

    /*let instant = Instant::now();
    let mut p = generate_templates(problem)?;

    let init_ch =
        create_initial_chronicle(&problem.goal_tasks, &problem.initial_state, &mut p.context);

    p.chronicles.push(init_ch);

    info!(
        "Generation of the planning problem: {:.3} ms",
        instant.elapsed().as_micros() as f64 / 1000.0
    );
    Ok(p)*/
}
*/
