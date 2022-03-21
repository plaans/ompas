use crate::planning::structs::chronicle::Chronicle;
use crate::planning::structs::symbol_table::AtomId;
use crate::planning::structs::traits::GetVariables;
use crate::planning::structs::type_table::PlanningAtomType;
use crate::planning::structs::ChronicleHierarchy;
use anyhow::{anyhow, Result};
use aries_core::Lit as ariesLit;
use aries_core::*;
use aries_model::extensions::Shaped;
use aries_model::lang::*;
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::constraints::Constraint;
use aries_planning::chronicles::{
    Chronicle as ariesChronicle, ChronicleInstance, ChronicleKind, ChronicleOrigin,
    ChronicleTemplate, Condition, Container, Ctx, Effect, Problem as ariesProblem, StateFun,
    SubTask, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;
use std::convert::TryInto;
use std::sync::Arc;

static TASK_TYPE: &str = "★task★";
static ABSTRACT_TASK_TYPE: &str = "★abstract_task★";
static ACTION_TYPE: &str = "★action★";
//static DURATIVE_ACTION_TYPE: &str = "★durative-action★";
static METHOD_TYPE: &str = "★method★";
static PREDICATE_TYPE: &str = "★predicate★";
static OBJECT_TYPE: &str = "★object★";
static FUNCTION_TYPE: &str = "★function★";

pub fn build_chronicles(ch: &ChronicleHierarchy) -> Result<chronicles::Problem> {
    let mut types: Vec<(Sym, Option<Sym>)> = vec![
        (TASK_TYPE.into(), None),
        (ABSTRACT_TASK_TYPE.into(), Some(TASK_TYPE.into())),
        (ACTION_TYPE.into(), Some(TASK_TYPE.into())),
        //(DURATIVE_ACTION_TYPE.into(), Some(TASK_TYPE.into())),
        (METHOD_TYPE.into(), None),
        (PREDICATE_TYPE.into(), None),
        (FUNCTION_TYPE.into(), None),
        (OBJECT_TYPE.into(), None),
    ];
    //let top_type: Sym = OBJECT_TYPE.into();

    let problem = &ch.problem;

    {
        for t in &problem.types {
            types.push((t.into(), Some(OBJECT_TYPE.into())));
        }
    }

    let th = TypeHierarchy::new(types)?;
    let mut symbols: Vec<TypedSymbol> = vec![];
    for (o, t) in &problem.objects {
        symbols.push(TypedSymbol::new(o, t))
    }

    for (label, _) in &problem.state_functions {
        symbols.push(TypedSymbol::new(label, FUNCTION_TYPE));
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

    let mut state_functions = Vec::with_capacity(problem.state_functions.len());
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

    let mut context = Ctx::new(Arc::new(symbol_table), state_functions);

    let _init_container = Container::Instance(0);
    // Initial chronicle construction
    let init_ch = ariesChronicle {
        kind: ChronicleKind::Problem,
        presence: ariesLit::TRUE,
        start: context.origin(),
        end: context.horizon(),
        name: vec![],
        task: None,
        conditions: vec![],
        effects: vec![],
        constraints: vec![],
        subtasks: vec![],
    };

    let init_ch = ChronicleInstance {
        parameters: vec![],
        origin: ChronicleOrigin::Original,
        chronicle: init_ch,
    };

    let mut templates: Vec<ChronicleTemplate> = Vec::new();
    for t in &ch.chronicle_templates {
        let cont = Container::Template(templates.len());
        let template = read_chronicle(cont, t, ch, &mut context)?;
        templates.push(template);
    }

    let problem = ariesProblem {
        context,
        templates,
        chronicles: vec![init_ch],
    };

    Ok(problem)
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

fn read_chronicle(
    c: Container,
    chronicle: &crate::planning::structs::chronicle::Chronicle,
    ch: &ChronicleHierarchy,
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

    /*
    CREATION of the name
     */
    //For the moment lacking the fact that we can add any kind of variables
    let mut name: Vec<SAtom> = vec![];
    for (i, p) in chronicle.name.iter().enumerate() {
        if i == 4 {
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
            //name.push(bindings.get_var(p).unwrap().into())
        }
    }

    let task: Vec<SAtom> = vec![];
    for p in &chronicle.task {}

    let mut constraints: Vec<Constraint> = vec![];
    let mut conditions: Vec<Condition> = vec![];
    let mut effects: Vec<Effect> = vec![];
    let mut subtasks: Vec<SubTask> = vec![];

    for x in chronicle.get_constraints() {
        /*let mut x;
        constraints.push(x);*/
    }

    for c in chronicle.get_conditions() {}

    for e in chronicle.get_effects() {}

    for s in chronicle.get_subtasks() {}

    /*let template = ariesChronicle {
        kind: ChronicleKind::Action,
        presence: prez,
        start: bindings
        .get_var(chronicle.get_start())
        .unwrap()
        .try_into()?,
        end: Default::default(), //bindings.get_var(chronicle.get_end()).unwrap().try_into()?,
        name,
        task: Some(task),
        conditions,
        effects,
        constraints,
        subtasks,
    };

    Ok(template);*/

    todo!()
}
