pub mod forest;
pub mod id;
pub mod lit;
pub mod meta_data;
pub mod r#ref;
pub mod var_domain;

use crate::structs::chronicle::interval::Interval;
use crate::structs::domain::root_type::RootType::{Any, Boolean, Handle};
use crate::structs::domain::root_type::{FALSE_ID, TRUE_ID};
use crate::structs::domain::type_lattice::TypeLattice;
use crate::structs::domain::Domain;
use crate::structs::sym_table::forest::{Forest, Node, NodeId};
use crate::structs::sym_table::id::SymbolTableId;
use crate::structs::sym_table::meta_data::SymTableMetaData;
//use ompas_rae_language::exec::state::{ASSERT, INSTANCE, RETRACT};
//use sompas_language::primitives::DO;
use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::root_type::RootType;
use crate::structs::sym_table::r#ref::RefSymTable;
use crate::structs::sym_table::var_domain::VarDomain;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::fmt::Display;
use std::fmt::Write;
use std::ops::DerefMut;

pub const RESULT_PREFIX: char = 'r';
pub const HANDLE_PREFIX: char = 'h';
pub const IF_PREFIX: &str = "if";
pub const TIMEPOINT_PREFIX: char = 't';
pub const START_PREFIX: char = 's';
pub const END_PREFIX: char = 'e';
pub const PRESENCE_PREFIX: char = 'p';
pub const COND_PREFIX: char = 'c';
pub const CHRONICLE_RESULT_PREFIX: &str = "cr";

pub const START: &str = "start";
pub const END: &str = "end";
pub const PREZ: &str = "prez";
pub const RESULT: &str = "result";
pub const COND: &str = "cond";
pub const IF_TASK_PROTOTYPE: &str = "t_if";

pub const TIMEPOINT_TYPE: &str = "Timepoint";
pub const TASK_TYPE: &str = "Task";
pub const METHOD_TYPE: &str = "Method";

pub type AtomId = NodeId;

#[derive(Clone)]
pub enum EmptyDomains {
    None,
    Some(Vec<AtomId>),
}

impl EmptyDomains {
    pub fn append(&mut self, mut other: Self) {
        match other {
            EmptyDomains::None => {}
            EmptyDomains::Some(mut vec2) => {
                if let Self::Some(vec1) = self {
                    vec1.append(&mut vec2)
                } else {
                    *self = Self::Some(vec2)
                }
            }
        }
    }
}

impl Default for EmptyDomains {
    fn default() -> Self {
        Self::None
    }
}

pub(crate) struct SymTable {
    domains: Forest<VarDomain>,
    //variable_symbols: HashMap<AtomId, String>,
    lattice: TypeLattice,
    ids: SymbolTableId,
    meta_data: SymTableMetaData,
    scopes: im::HashMap<AtomId, Interval>,
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            domains: Default::default(),
            ids: Default::default(),
            meta_data: Default::default(),
            scopes: Default::default(),
            lattice: Default::default(),
        };

        st.lattice
            .add_type(TIMEPOINT_TYPE, vec![RootType::Number as usize]);

        st.lattice
            .add_type(TASK_TYPE, vec![RootType::Symbol as usize]);
        st.lattice
            .add_type(METHOD_TYPE, vec![RootType::Symbol as usize]);

        //Symbols of lisp functions that are useful
        //Not exhaustive
        st
    }
}

impl SymTable {
    /*
    SCOPES FUNCTIONS
     */
    pub fn scopes(&self) -> im::HashMap<AtomId, Interval> {
        self.scopes.clone()
    }

    pub fn get_scope(&self, id: &AtomId) -> Option<&Interval> {
        self.scopes.get(id)
    }

    pub fn get_start(&self, id: &AtomId) -> Option<&AtomId> {
        self.scopes.get(id).map(|i| i.get_start())
    }

    pub fn get_end(&self, id: &AtomId) -> Option<&AtomId> {
        self.scopes.get(id).map(|i| i.get_end())
    }

    pub fn new_scope(&mut self, id: &AtomId, start: &AtomId) {
        self.scopes.insert(*id, Interval::new_instantaneous(start));
    }

    pub fn set_end(&mut self, id: &AtomId, end: &AtomId) {
        self.scopes.get_mut(id).unwrap().set_end(end);
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_bool(&mut self, b: bool) -> AtomId {
        let id = self.domains.new_node(VarDomain::new(b, b));
        id
    }

    pub fn new_int(&mut self, i: i64) -> AtomId {
        let id = self.domains.new_node(VarDomain::new(i, i));
        id
    }

    pub fn new_float(&mut self, f: f64) -> AtomId {
        let id = self.domains.new_node(VarDomain::new(f, f));
        id
    }

    pub fn new_number(&mut self, n: &LNumber) -> AtomId {
        match n {
            LNumber::Int(i) => self.new_int(*i),
            LNumber::Float(f) => self.new_float(*f),
        }
    }

    /*
    DECLARATION FUNCTION
     */
    /*pub fn new_type(&mut self, sym: impl Display, a_type: Option<TypeId>) -> TypeId {
        let sym = sym.to_string();
        let id = self.symbols.new_node(sym.as_str().into());
        self.ids.insert(sym.as_str(), &id);
        let atom_type = match a_type {
            None => AtomType::RootType,
            Some(t) => AtomType::SubType(t),
        };
        self.types.add_new_atom(&id, atom_type);
        self.types.add_type(sym, id);
        id
    }*/

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> AtomId {
        let index = self.meta_data.new_result_index();
        let sym = format!("_{RESULT_PREFIX}{index}_").to_string();
        let id = self
            .domains
            .new_node(VarDomain::new(sym.to_string(), RootType::Any));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_timepoint(&mut self) -> AtomId {
        let index = self.meta_data.new_timepoint_index();
        let sym = format!("_{TIMEPOINT_PREFIX}{index}_").to_string();
        let id = self.domains.new_node(VarDomain::new(
            sym.to_string(),
            self.get_type_as_domain(TIMEPOINT_TYPE),
        ));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_if(&mut self) -> (AtomId, AtomId, AtomId) {
        let index = self.meta_data.new_if_index();

        let sym_if = &format!("_{IF_PREFIX}{index}_");

        let id_if = self
            .domains
            .new_node(VarDomain::new(sym_if, self.get_type_as_domain(TASK_TYPE)));
        self.ids.insert(&sym_if, &id_if);

        let sym_m_true = &format!("m_{}_true", sym_if);
        let id_m_true = self.domains.new_node(VarDomain::new(
            sym_m_true,
            self.get_type_as_domain(METHOD_TYPE),
        ));
        self.ids.insert(&sym_m_true, &id_m_true);

        let sym_m_false = &format!("m_{}_false", sym_if);
        let id_m_false = self.domains.new_node(VarDomain::new(
            sym_m_false,
            self.get_type_as_domain(METHOD_TYPE),
        ));
        self.ids.insert(&sym_m_false, &id_m_false);
        (id_if, id_m_true, id_m_false)
    }

    pub fn new_handle(&mut self) -> AtomId {
        let index = self.meta_data.new_handle_index();
        let sym = &format!("_{HANDLE_PREFIX}{index}_");
        let id = self.domains.new_node(VarDomain::new(sym, Handle));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_start(&mut self) -> AtomId {
        let index = self.meta_data.new_start_index();
        let sym = &format!("_{START_PREFIX}{index}_");
        let id = self
            .domains
            .new_node(VarDomain::new(sym, self.get_type_as_domain(TIMEPOINT_TYPE)));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_end(&mut self) -> AtomId {
        let index = self.meta_data.new_end_index();
        let sym = &format!("_{END_PREFIX}{index}_");
        let id = self
            .domains
            .new_node(VarDomain::new(sym, self.get_type_as_domain(TIMEPOINT_TYPE)));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_presence(&mut self) -> AtomId {
        let index = self.meta_data.new_presence_index();
        let sym = &format!("_{PRESENCE_PREFIX}{index}_");
        let id = self.domains.new_node(VarDomain::new(sym, Boolean));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_chronicle_result(&mut self) -> AtomId {
        let index = self.meta_data.new_chronicle_result_index();
        let sym = &format!("_{CHRONICLE_RESULT_PREFIX}{index}_");
        let id = self.domains.new_node(VarDomain::new(sym, Any));
        self.ids.insert(&sym, &id);
        id
    }

    /*pub fn new_typed_symbol(&mut self, sym: impl Display, domain: Domain) {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.id(sym).unwrap()
        } else {
            let sym: String = sym.to_string();
            let id = self.domains.new_node(domain);
            self.variable_symbols.insert(id, sym.to_string());
            self.ids.insert(&sym, &id);
            id
        }
    }*/

    pub fn new_symbol(&mut self, sym: impl Display) -> AtomId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.id(sym).unwrap()
        } else {
            let sym: &String = &sym.to_string();
            let id = self.domains.new_node(VarDomain::new(sym, sym.as_str()));
            self.ids.insert(&sym, &id);
            id
        }
    }

    /*pub fn new_variable(&mut self, sym: impl Display) -> AtomId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.id(sym).unwrap()
        } else {
            let sym: String = sym.to_string();
            let id = self.domains.new_node(Domain::default());
            self.variable_symbols.insert(id, sym.to_string());
            self.ids.insert(&sym, &id);
            id
        }
    }*/

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: Domain) -> AtomId {
        let symbol = symbol.to_string();
        let version = self.ids.version(&symbol);
        let sym = format!("{symbol}_{version}");
        let id = self.domains.new_node(VarDomain::new(sym, domain));
        self.ids.insert(&symbol, &id);
        id
    }

    /*
    GETTERS
    */
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<VarDomain>> {
        self.domains.get_node(id)
    }

    pub fn get_domain(&self, id: &AtomId, parent: bool) -> Option<&Domain> {
        let id = &match parent {
            true => self.get_parent(id),
            false => *id,
        };

        self.domains.get_value(id).map(|v| &v.domain)
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        *self.domains.get_parent(a)
    }

    pub fn id(&self, sym: &str) -> Option<AtomId> {
        self.ids.get_id(sym)
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        *self.domains.find(a)
    }

    /*
    Domain operators
     */

    pub fn meet(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__meet(d1, d2)
    }

    pub fn union(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__union(d1, d2)
    }

    pub fn substract(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__substract(d1, d2)
    }

    pub fn meet_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn union_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn substract_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.substract(d1, d2)
    }

    pub fn update_domain(&mut self, id_d1: &AtomId) -> EmptyDomains {
        if !self.domains[*id_d1].union.is_empty() {
            let mut domain = Domain::empty();
            for d in &self.domains[*id_d1].union {
                domain = self.union(&self.domains[*d].domain, &domain);
            }
            let domain = self.meet(&self.domains[*id_d1].domain, &domain);
            self.domains[*id_d1].domain = domain;

            if self.domains[*id_d1].domain.is_empty() {
                EmptyDomains::Some(vec![*id_d1])
            } else {
                EmptyDomains::None
            }
        } else {
            EmptyDomains::None
        }
    }

    pub fn meet_to_domain(&mut self, id_d1: &AtomId, domain: impl Into<Domain>) -> EmptyDomains {
        let var_domain = self.domains[*id_d1].clone();
        let mut emptys = EmptyDomains::None;

        if !var_domain.union.is_empty() {
            let domain = domain.into();
            for d in &var_domain.union {
                emptys.append(self.meet_to_domain(d, domain.clone()));
            }

            emptys.append(self.update_domain(id_d1));
        } else {
            let d1 = &self.domains[*id_d1].domain;

            let d = self.meet(d1, &domain.into());
            if d.is_empty() {
                emptys.append(EmptyDomains::Some(vec![*id_d1]));
            }

            self.domains[*id_d1].domain = d;
        }

        for id_parent in &var_domain.parents {
            emptys.append(self.update_domain(&id_parent));
        }

        emptys
    }

    pub fn substract_to_domain(
        &mut self,
        id_d1: &AtomId,
        domain: impl Into<Domain>,
    ) -> EmptyDomains {
        let var_domain = self.domains[*id_d1].clone();
        let mut emptys = EmptyDomains::None;

        if !var_domain.union.is_empty() {
            let domain = domain.into();
            for d in &var_domain.union {
                emptys.append(self.substract_to_domain(d, domain.clone()));
            }

            emptys.append(self.update_domain(id_d1));
        } else {
            let d1 = &self.domains[*id_d1].domain;

            let d = self.substract(d1, &domain.into());
            if d.is_empty() {
                emptys.append(EmptyDomains::Some(vec![*id_d1]));
            }

            self.domains[*id_d1].domain = d;
        }

        for id_parent in var_domain.parents {
            emptys.append(self.update_domain(&id_parent));
        }

        emptys
    }

    pub fn set_domain(&mut self, id: &AtomId, domain: impl Into<Domain>) -> EmptyDomains {
        let domain = domain.into();
        let empty = domain.is_empty();
        self.domains[*id].domain = domain;
        if empty {
            EmptyDomains::Some(vec![*id])
        } else {
            EmptyDomains::None
        }
    }

    pub fn add_union_dependency(&mut self, id: &AtomId, mut union: Vec<AtomId>) {
        self.domains[*id].union.append(&mut union)
    }

    pub fn add_parent_dependency(&mut self, id: &AtomId, parent: AtomId) {
        self.domains[*id].parents.push(parent);
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        self.lattice.contained_in(d1, d2)
    }

    pub fn get_type_as_domain(&self, r#type: impl Into<BasicType>) -> Domain {
        self.lattice.get_type_id(r#type).unwrap().into()
    }

    /*pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }*/
    /*
    BOOLEAN FUNCTION
     */
    pub fn it_exists(&self, sym: &str) -> bool {
        self.ids.contains(sym)
    }

    /*
    FOREST FUNCTIONS
     */
    pub fn flat_bindings(&mut self) {
        self.domains.flat_bindings();
    }

    pub fn try_union_atom(&mut self, id1: &AtomId, id2: &AtomId) -> EmptyDomains {
        let p1 = self.get_parent(id1);
        let p2 = self.get_parent(id2);
        let d2 = self.domains[p2].domain.clone();
        let mut union = self.domains[p1].union.clone();
        union.append(&mut self.domains[p2].union.clone());
        let mut parents = self.domains[p1].parents.clone();
        parents.append(&mut self.domains[p2].parents.clone());

        self.domains[p1].union = union.clone();
        self.domains[p1].parents = parents.clone();
        self.domains.union_ordered(&p1, &p2);

        let r = self.meet_to_domain(&p1, d2);
        self.domains[p2] = self.domains[p1].clone();

        r
    }

    /*
    FORMAT Function
      */

    pub fn format_variable(&self, id: &AtomId) -> String {
        let d = &self.domains[*id];

        match &d.domain {
            Domain::Cst(_, cst) => cst.to_string(),
            Domain::Simple(TRUE_ID) => true.to_string(),
            Domain::Simple(FALSE_ID) => false.to_string(),
            _ => d.label.to_string(),
        }
    }

    pub fn format_domain(&self, id: &AtomId) -> String {
        let domain = &self.domains[*id];
        let mut str = format!("domain = {}", domain.domain.format(&self.lattice));
        if !domain.union.is_empty() {
            write!(str, ", union = {{");
            for (i, id) in domain.union.iter().enumerate() {
                if i != 0 {
                    str.push(',');
                }
                write!(str, "{}", self.format_variable(id));
            }

            write!(str, "}}");
        }

        if !domain.parents.is_empty() {
            write!(str, ", parents = {{");
            for (i, id) in domain.parents.iter().enumerate() {
                if i != 0 {
                    str.push(',');
                }
                write!(str, "{}", self.format_variable(id));
            }

            write!(str, "}}");
        }
        str
    }

    /*pub fn format_symbols_forest(&self) -> String {
        self.domains.to_string()
    }*/
}

pub fn lvalue_to_domain(lv: &LValue, st: &mut RefSymTable) -> Result<Domain, LRuntimeError> {
    match lv {
        LValue::List(list) => {
            let mut vec = vec![];
            for e in list.iter() {
                vec.push(lvalue_to_domain(e, st)?);
            }
            Ok(Domain::Composed(RootType::List as usize, vec))
        }
        LValue::Map(_) => Err(lruntimeerror!(
            "LValue to lit",
            "Map transformation to lit is not supported yet."
        )),
        LValue::Number(n) => match n {
            LNumber::Int(i) => Ok((*i).into()),
            LNumber::Float(f) => Ok((*f).into()),
        },
        LValue::True => Ok(true.into()),
        LValue::Nil => Ok(Domain::nil()),
        lv => Ok(match st.id(&lv.to_string()) {
            Some(id) => id.into(),
            None => {
                //println!("symbol {} does not exist", lv.to_string());
                st.new_symbol(&lv.to_string()).into()
            }
        }),
    }
}
