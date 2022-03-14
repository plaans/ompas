use crate::planning::structs::ChronicleHierarchy;
use anyhow::{anyhow, Result};
use aries_core::Lit as ariesLit;
use aries_core::*;
use aries_model::lang::*;
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::{
    Chronicle as ariesChronicle, ChronicleInstance, ChronicleKind, ChronicleOrigin,
    ChronicleTemplate, Container, Ctx, Problem as ariesProblem, StateFun, VarType, TIME_SCALE,
};
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;
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

fn read_chronicle(
    c: Container,
    _chronicle: &crate::planning::structs::chronicle::Chronicle,
    _ch: &ChronicleHierarchy,
    context: &mut Ctx,
) -> Result<ChronicleTemplate> {
    let _top_type: Sym = OBJECT_TYPE.into();
    let mut params: Vec<Variable> = Vec::new();
    let prez_var = context.model.new_bvar(c / VarType::Presence);
    params.push(prez_var.into());
    let prez = prez_var.true_lit();

    let start = context.model.new_optional_fvar(
        0,
        INT_CST_MAX,
        TIME_SCALE,
        prez,
        c / VarType::ChronicleStart,
    );
    params.push(start.into());
    let _start = FAtom::from(start);
    let end = context.model.new_optional_fvar(
        0,
        INT_CST_MAX,
        TIME_SCALE,
        prez,
        c / VarType::ChronicleEnd,
    );
    params.push(end.into());
    let _end: FAtom = end.into();
    todo!()
}
