use crate::aries::{BindingAriesAtoms, FLOAT_SCALE};
use aries_core::Lit as aLit;
use aries_model::extensions::Shaped;
use aries_model::lang::{
    Atom as aAtom, ConversionError, FAtom, FVar, IAtom, IVar, SAtom, Type as aType, Variable,
};
use aries_model::symbols::SymbolTable;
use aries_planning::chronicles::Ctx;
use function_name::named;
use ompas_language::sym_table::EPSILON;
use ompas_structs::sym_table::domain::basic_type::{TYPE_ID_BOOLEAN, TYPE_ID_FLOAT, TYPE_ID_INT};
use ompas_structs::sym_table::domain::cst::Cst;
use ompas_structs::sym_table::domain::type_lattice::TypeLattice;
use ompas_structs::sym_table::domain::{cst, Domain};
use ompas_structs::sym_table::r#ref::RefSymTable;
use ompas_structs::sym_table::VarId;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalues::LValueS;

#[named]
pub fn get_type(
    lattice: &TypeLattice,
    st: &SymbolTable,
    t: &Domain,
) -> lruntimeerror::Result<aType> {
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
                        function_name!(),
                        format!("{} Unknown type", other)
                    )),
                }
            }
        },
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

pub fn atom_from_cst(ctx: &Ctx, cst: &cst::Cst) -> aAtom {
    match cst {
        Cst::Int(i) => IAtom::from(*i as i32).into(),
        Cst::Float(f) => {
            let f: i32 = (f * FLOAT_SCALE as f64) as i32;
            FAtom::new(IAtom::from(f), FLOAT_SCALE).into()
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

#[allow(dead_code)]
pub fn var_id_into_atom(
    a: &VarId,
    st: &RefSymTable,
    bindings: &BindingAriesAtoms,
    ctx: &Ctx,
) -> aAtom {
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
            Cst::Symbol(s) => {
                if s.as_str() == EPSILON {
                    (FVar::new(IVar::ZERO, FLOAT_SCALE) + FAtom::EPSILON).into()
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
        (*bindings
            .get_var(a)
            .unwrap_or_else(|| panic!("{} undefined in bindings", st.format_variable(a))))
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
