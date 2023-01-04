pub mod forest;
pub mod id;
pub mod meta_data;
pub mod r#ref;

use crate::structs::chronicle::interval::Interval;
use crate::structs::domain::root_type::RootType::{Boolean, Handle};
use crate::structs::domain::type_lattice::TypeLattice;
use crate::structs::domain::Domain;
use crate::structs::sym_table::forest::{Forest, Node, NodeId};
use crate::structs::sym_table::id::SymbolTableId;
use crate::structs::sym_table::meta_data::SymTableMetaData;
//use ompas_rae_language::exec::state::{ASSERT, INSTANCE, RETRACT};
//use sompas_language::primitives::DO;
use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::root_type::RootType;
use sompas_structs::lnumber::LNumber;
use std::collections::HashMap;
use std::fmt::Display;

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
pub const TIMEPOINT_TYPE: &str = "TIMEPOINT";

pub type AtomId = NodeId;

pub(crate) struct SymTable {
    domains: Forest<Domain>,
    debug: HashMap<AtomId, String>,
    lattice: TypeLattice,
    ids: SymbolTableId,
    meta_data: SymTableMetaData,
    scopes: im::HashMap<AtomId, Interval>,
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            domains: Default::default(),
            debug: Default::default(),
            ids: Default::default(),
            meta_data: Default::default(),
            scopes: Default::default(),
            lattice: Default::default(),
        };

        st.lattice
            .add_type(TIMEPOINT_TYPE, vec![RootType::Number as usize]);

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
        let id = self.domains.new_node(b.into());
        self.debug.insert(id, b.to_string());
        id
    }

    pub fn new_int(&mut self, i: i64) -> AtomId {
        let id = self.domains.new_node(i.into());
        self.debug.insert(id, i.to_string());
        id
    }

    pub fn new_float(&mut self, f: f64) -> AtomId {
        let id = self.domains.new_node(f.into());
        self.debug.insert(id, f.to_string());
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
        let id = self.domains.new_node(Domain::default());
        self.debug.insert(id, sym.to_string());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_timepoint(&mut self) -> AtomId {
        let index = self.meta_data.new_timepoint_index();
        let sym = format!("_{TIMEPOINT_PREFIX}{index}_").to_string();
        let id = self
            .domains
            .new_node(self.get_type_as_domain(TIMEPOINT_TYPE));
        self.debug.insert(id, sym.to_string());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_if(&mut self) -> (AtomId, AtomId, AtomId) {
        let index = self.meta_data.new_if_index();

        let sym = format!("_{IF_PREFIX}{index}_");

        let id_if = self.domains.new_node(Domain::default());
        self.ids.insert(&sym, &id_if);
        self.debug.insert(id_if, sym.to_string());

        let sym_m_true = format!("m_{}_true", sym);
        let id_m_true = self.domains.new_node(Domain::default());
        self.ids.insert(&sym_m_true, &id_m_true);
        self.debug.insert(id_m_true, sym_m_true.to_string());

        let sym_m_false = format!("m_{}_false", sym);
        let id_m_false = self.domains.new_node(Domain::default());
        self.ids.insert(&sym_m_false, &id_m_false);
        self.debug.insert(id_m_false, sym_m_false);
        (id_if, id_m_true, id_m_false)
    }

    pub fn new_handle(&mut self) -> AtomId {
        let index = self.meta_data.new_handle_index();
        let sym = format!("_{HANDLE_PREFIX}{index}_");
        let id = self.domains.new_node(Handle.into());
        self.ids.insert(&sym, &id);
        self.debug.insert(id, sym.to_string());
        id
    }

    pub fn new_start(&mut self) -> AtomId {
        let index = self.meta_data.new_start_index();
        let sym = format!("_{START_PREFIX}{index}_");
        let id = self
            .domains
            .new_node(self.get_type_as_domain(TIMEPOINT_TYPE));
        self.ids.insert(&sym, &id);
        self.debug.insert(id, sym.to_string());
        id
    }

    pub fn new_end(&mut self) -> AtomId {
        let index = self.meta_data.new_end_index();
        let sym = format!("_{END_PREFIX}{index}_");
        let id = self
            .domains
            .new_node(self.get_type_as_domain(TIMEPOINT_TYPE));
        self.ids.insert(&sym, &id);
        self.debug.insert(id, sym.to_string());
        id
    }

    pub fn new_presence(&mut self) -> AtomId {
        let index = self.meta_data.new_presence_index();
        let sym = format!("_{PRESENCE_PREFIX}{index}_");
        let id = self.domains.new_node(Boolean.into());
        self.ids.insert(&sym, &id);
        self.debug.insert(id, sym.to_string());
        id
    }

    pub fn new_chronicle_result(&mut self) -> AtomId {
        let index = self.meta_data.new_chronicle_result_index();
        let sym = format!("_{CHRONICLE_RESULT_PREFIX}{index}_");
        let id = self.domains.new_node(Domain::default());
        self.ids.insert(&sym, &id);
        self.debug.insert(id, sym.to_string());
        id
    }

    pub fn new_symbol(&mut self, sym: impl Display, domain: Option<Domain>) -> AtomId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.id(sym).unwrap()
        } else {
            let sym: String = sym.into();
            let id = self.domains.new_node(domain.unwrap_or_default());
            self.debug.insert(id, sym.to_string());
            self.ids.insert(&sym, &id);
            id
        }
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: Domain) -> AtomId {
        let symbol = symbol.to_string();
        let version = self.ids.version(&symbol);
        let id = self.domains.new_node(domain);
        self.ids.insert(&symbol, &id);
        id
    }

    /*
    GETTERS
    */
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<Domain>> {
        self.domains.get_node(id)
    }

    pub fn get_domain(&self, id: &AtomId, parent: bool) -> Option<&Domain> {
        match parent {
            true => self.domains.get_value(&self.get_parent(id)),
            false => self.domains.get_value(id),
        }
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        *self.domains.get_parent(a)
    }

    pub fn id(&self, sym: &str) -> Option<AtomId> {
        self.ids.get_id(sym)
    }

    pub fn meet_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__meet(d1, d2)
    }

    pub fn union_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__union(d1, d2)
    }

    pub fn substract_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.__substract(d1, d2)
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        self.lattice.contained_in(d1, d2)
    }

    pub fn get_debug(&self, id: &AtomId) -> &str {
        self.debug.get(id).unwrap()
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
    TYPES
     */
    /*pub fn set_type_of(&mut self, atom_id: &AtomId, atom_type: &AtomType) {
        self.types.set_type_of(atom_id, atom_type)
    }

    pub fn union_types(&mut self, a: &AtomId, b: &AtomId) {
        self.types.union_types(a, b).unwrap()
    }

    pub fn get_type_id_of(&self, atom_id: &AtomId) -> &TypeId {
        self.types.get_type_id_of(atom_id)
    }*/

    /*
    FOREST FUNCTIONS
     */
    /*pub fn union_atom(&mut self, a: &AtomId, b: &AtomId) {
        self.symbols.union_ordered(a, b);
        self.types.union_types(a, b).unwrap();
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        *self.symbols.find(a)
    }



    pub fn flat_bindings(&mut self) {
        self.domains.flat_bindings();
    }

    pub fn format_symbols_forest(&self) -> String {
        self.domains.to_string()
    }
    pub fn format_types_forest(&self) -> String {
        self.types.inner.to_string()
    }*/
}
