use crate::planning::structs::ChronicleHierarchy;
use anyhow::{anyhow, Result};
use aries_model::lang::Type;
use aries_model::symbols::SymbolTable;
use aries_model::types::TypeHierarchy;
use aries_planning::chronicles;
use aries_planning::chronicles::StateFun;
use aries_planning::parsing::pddl::TypedSymbol;
use aries_utils::input::Sym;

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
        let mut args = Vec::with_capacity(sf.1.get_number() + 1);
        for tpe in &sf.1.get_types() {
            let tpe = symbol_table
                .types
                .id_of(tpe)
                .ok_or_else(|| anyhow!("{} Unknown type", sf.0))?;
            args.push(Type::Sym(tpe));
        }
        // TODO: set to a fixed-point numeral of appropriate precision
        args.push(Type::Int); // return type (last one) is a int value
        state_functions.push(StateFun { sym, tpe: args })
    }

    // determine the top types in the user-defined hierarchy.
    // this is typically "object" by convention but might something else (e.g. "obj" in some hddl problems).
    /*{
        let all_types: HashSet<&Sym> = ch.types.iter().map(|tpe| &tpe.symbol).collect();
        let top_types = dom
            .types
            .iter()
            .filter_map(|tpe| tpe.tpe.as_ref())
            .filter(|tpe| !all_types.contains(tpe))
            .unique();
        for t in top_types {
            types.push((t.clone(), Some(OBJECT_TYPE.into())));
        }
    }*/

    todo!()
}
