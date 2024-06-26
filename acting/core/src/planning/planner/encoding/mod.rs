use crate::model::acting_domain::model::ActingModel;
use crate::model::acting_domain::state_function::StateFunction;
use crate::model::sym_domain::basic_type::{TYPE_ID_BOOLEAN, TYPE_ID_FLOAT, TYPE_ID_INT};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_domain::Domain;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::VarId;
use crate::ompas::manager::planning::acting_var_ref_table::ActingVarRefTable;
use crate::ompas::manager::state::world_state_snapshot::WorldStateSnapshot;
use crate::planning::planner::encoding::domain::encode_ctx;
use crate::planning::planner::encoding::instance::generate_instances;
use crate::planning::planner::problem::ChronicleInstance;
use anyhow::Result;
use aries::core::{Lit as aLit, INT_CST_MAX, INT_CST_MIN};
use aries::model::extensions::Shaped;
use aries::model::lang::{
    Atom as aAtom, ConversionError, FAtom, FVar, IAtom, IVar, SAtom, Type as aType, Variable,
};
use aries::model::symbols::SymbolTable;
use aries_planning::chronicles;
use aries_planning::chronicles::Ctx;
use aries_planning::chronicles::TIME_SCALE;
use function_name::named;
use ompas_language::sym_table::EPSILON;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalues::LValueS;

pub mod domain;
pub mod instance;
pub mod problem_generation;

//pub const TIME_SCALE.get(): IntCst = TIME_SCALE;
/// Resolution of ms

pub struct PlannerProblem {
    pub st: RefSymTable,
    pub instances: Vec<ChronicleInstance>,
    pub templates: Vec<ActingModel>,
    pub domain: PlannerDomain,
    pub state: WorldStateSnapshot,
}

pub struct PlannerDomain {
    pub sf: Vec<StateFunction>,
    pub methods: Vec<String>,
    pub tasks: Vec<String>,
    pub commands: Vec<String>,
}

pub async fn encode_chronicles(
    table: &mut ActingVarRefTable,
    st: &RefSymTable,
    problem: &PlannerProblem,
) -> Result<chronicles::Problem> {
    let domain = &problem.domain;

    let mut ctx = encode_ctx(st, domain, &problem.state.instance)?;

    let templates = domain::generate_templates(table, &mut ctx, &problem.templates)?;

    let chronicles = generate_instances(&mut ctx, table, &problem.instances)?;

    Ok(chronicles::Problem {
        context: ctx,
        templates,
        chronicles,
    })
}

#[named]
pub fn get_type(
    lattice: &TypeLattice,
    st: &SymbolTable,
    t: &Domain,
) -> lruntimeerror::Result<aType> {
    match t {
        Domain::Simple(t) => match *t {
            TYPE_ID_BOOLEAN => Ok(aType::Bool),
            TYPE_ID_INT => Ok(aType::Int {
                lb: INT_CST_MIN,
                ub: INT_CST_MAX,
            }),
            TYPE_ID_FLOAT => Ok(aType::Fixed(TIME_SCALE.get())),
            t => {
                let other: String = lattice.format_type(&t);
                match st.types.id_of(&other) {
                    Some(t) => Ok(aType::Sym(t)),
                    None => Err(lruntimeerror!(
                        function_name!(),
                        format!("{} Unknown type", other)
                    )),
                }
            }
        },
        Domain::IntRange(lb, ub) => Ok(aType::Int {
            lb: *lb as i32,
            ub: *ub as i32,
        }),
        t => Err(LRuntimeError::new(
            function_name!(),
            format!("{} not supported yet in aries encoding", t.format(lattice)),
        )),
    }
}

pub fn satom_from_lvalues(ctx: &Ctx, v: &LValueS) -> SAtom {
    let v: String = v.try_into().expect("");
    ctx.typed_sym(
        ctx.model
            .get_symbol_table()
            .id(&v)
            .unwrap_or_else(|| panic!("{} undefined in symbol table", v)),
    )
    .into()
}

pub fn atom_from_cst(ctx: &Ctx, cst: &Cst) -> aAtom {
    match cst {
        Cst::Int(i) => IAtom::from(*i as i32).into(),
        Cst::Float(f) => {
            let f: i32 = (f * TIME_SCALE.get() as f64) as i32;
            FAtom::new(IAtom::from(f), TIME_SCALE.get()).into()
        }
        Cst::Symbol(s) => {
            let id = ctx
                .model
                .get_symbol_table()
                .id(s.as_str())
                .unwrap_or_else(|| panic!("{} should have been defined as a parameter", s));
            SAtom::from(ctx.typed_sym(id)).into()
        }
        Cst::Bool(true) => aLit::TRUE.into(),
        Cst::Bool(false) => aLit::FALSE.into(),
    }
}

pub fn atom_from_lvalues(ctx: &Ctx, v: &LValueS) -> aAtom {
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
            let f: i32 = (f * TIME_SCALE.get() as f64) as i32;
            FAtom::new(IAtom::from(f), TIME_SCALE.get()).into()
        }
        LValueS::Bool(b) => match b {
            false => aLit::FALSE.into(),
            true => aLit::TRUE.into(),
        },
        LValueS::List(_) => panic!("cannot convert LValueS::List into atom: {}", v),
        LValueS::Map(_) => panic!("cannot convert LValueS::Map into atom: {}", v),
    }
}

//#[allow(dead_code)]
pub fn var_id_into_atom(
    a: &VarId,
    st: &RefSymTable,
    table: &ActingVarRefTable,
    ctx: &Ctx,
) -> aAtom {
    let ompas_var = st.get_var_domain(st.get_domain_id(*a));
    let domain = &ompas_var.domain;
    if domain.is_true() {
        aLit::TRUE.into()
    } else if domain.is_false() | domain.is_nil() {
        aLit::FALSE.into()
    } else if let Domain::Cst(_t, cst) = domain {
        match cst {
            Cst::Int(i) => IAtom::from(*i as i32).into(),
            Cst::Float(f) => {
                let f: i32 = (f * TIME_SCALE.get() as f64) as i32;
                FAtom::new(IAtom::from(f), TIME_SCALE.get()).into()
            }
            Cst::Symbol(s) => {
                if s.as_str() == EPSILON {
                    (FVar::new(IVar::ZERO, TIME_SCALE.get()) + FAtom::EPSILON).into()
                } else {
                    ctx.typed_sym(
                        ctx.model
                            .get_symbol_table()
                            .id(s)
                            .unwrap_or_else(|| panic!("{} is not defined in symbol table", s)),
                    )
                    .into()
                }
            }
            Cst::Bool(b) => match b {
                true => aLit::TRUE.into(),
                false => aLit::FALSE.into(),
            },
        }
    } else {
        (*table.get_var(*a).unwrap_or_else(|| {
            panic!(
                "{}(domain = {}) undefined in bindings",
                st.format_variable(*a),
                st.format_domain(domain)
            )
        }))
        .into()
    }
}

pub fn try_variable_into_fvar(variable: Variable) -> anyhow::Result<FVar, ConversionError> {
    if let Variable::Fixed(fvar) = variable {
        Ok(fvar)
    } else {
        Err(ConversionError::TypeError)
    }
}
