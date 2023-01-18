pub mod closure;
pub mod computation;
pub mod domain;
pub mod forest;
pub mod id;
pub mod lit;
pub mod meta_data;
pub mod r#ref;
pub mod var_domain;
pub mod variable;

use crate::conversion::flow_graph::graph::Dot;
use crate::sym_table::closure::Update;
use crate::sym_table::domain::basic_type::BasicType;
use crate::sym_table::domain::basic_type::BasicType::{Any, Boolean, Handle};
use crate::sym_table::domain::basic_type::{TYPE_ID_FALSE, TYPE_ID_NIL, TYPE_ID_TRUE};
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::domain::simple_type::SimpleType;
use crate::sym_table::domain::type_lattice::TypeLattice;
use crate::sym_table::domain::Domain;
use crate::sym_table::forest::{Forest, NodeId};
use crate::sym_table::id::SymbolTableId;
use crate::sym_table::meta_data::SymTableMetaData;
use crate::sym_table::r#ref::RefSymTable;
use crate::sym_table::var_domain::VarDomain;
use crate::sym_table::variable::Variable;
use sompas_language::kind::NIL;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::VecDeque;
use std::fmt::Display;
use std::fmt::Write;

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

pub const TYPE_TIMEPOINT: &str = "*Timepoint*";
pub const TYPE_PRESENCE: &str = "*Presence*";
pub const TYPE_TASK: &str = "*Task*";
pub const TYPE_METHOD: &str = "*Method*";
pub const TYPE_ABSTRACT_TASK: &str = "*AbstractTask*";
pub const TYPE_COMMAND: &str = "*Action*";
pub const TYPE_PREDICATE: &str = "*Predicate*";
pub const TYPE_STATE_FUNCTION: &str = "*StateFunctionType*";
pub const TYPE_OBJECT_TYPE: &str = "*ObjectType*";
pub const TYPE_OBJECT: &str = "*Object*";
pub const TYPE_RESSOURCE_HANDLE: &str = "*ResourceHandle*";

pub type VarId = NodeId;
pub type DomainId = NodeId;

#[derive(Clone)]
pub enum EmptyDomains {
    None,
    Some(Vec<VarId>),
}

impl EmptyDomains {
    pub fn append(&mut self, other: Self) {
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

pub struct SymTable {
    domains: Forest<VarDomain>,
    variables: Forest<Variable>,
    lattice: TypeLattice,
    ids: SymbolTableId,
    meta_data: SymTableMetaData,
}

impl Default for SymTable {
    fn default() -> Self {
        Self {
            domains: Default::default(),
            ids: Default::default(),
            meta_data: Default::default(),
            lattice: TypeLattice::new(),
            variables: Default::default(),
        }
    }
}

impl SymTable {
    pub fn new_from(lattice: TypeLattice) -> Self {
        Self {
            domains: Default::default(),
            variables: Default::default(),
            lattice,
            ids: Default::default(),
            meta_data: Default::default(),
        }
    }

    /*
    SCOPES FUNCTIONS
     */

    pub fn set_declaration(&mut self, id: &VarId, timepoint: &VarId) {
        self.variables[*id].declaration = Some(*timepoint);
    }

    pub fn set_drop(&mut self, id: &VarId, timepoint: &VarId) {
        self.variables[*id].drop = Some(*timepoint);
    }

    pub fn get_declaration(&self, id: &VarId) -> Option<VarId> {
        self.variables[*id]
            .declaration
            .map(|t| self.get_var_parent(&t))
    }

    pub fn get_drop(&self, id: &VarId) -> Option<VarId> {
        self.variables[*id].drop.map(|t| self.get_var_parent(&t))
    }

    pub fn get_domain_vars(&self, d: &DomainId) -> Vec<VarId> {
        self.domains[*d].vars.clone()
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_variable(&mut self, sym: impl Display, domain: impl Into<Domain>) -> VarId {
        let domain_id = self.domains.new_node(VarDomain::new(domain));
        let id = self.variables.new_node(Variable::new(sym, domain_id));
        self.add_var_to_domain(&domain_id, &id);
        id
    }

    pub fn new_bool(&mut self, b: bool) -> VarId {
        self.new_variable(b, b)
    }

    pub fn new_nil(&mut self) -> VarId {
        self.new_variable(NIL, BasicType::Nil)
    }

    pub fn new_int(&mut self, i: i64) -> VarId {
        self.new_variable(i, i)
    }

    pub fn new_float(&mut self, f: f64) -> VarId {
        self.new_variable(f, f)
    }

    pub fn new_number(&mut self, n: &LNumber) -> VarId {
        match n {
            LNumber::Int(i) => self.new_int(*i),
            LNumber::Float(f) => self.new_float(*f),
        }
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> VarId {
        let index = self.meta_data.new_result_index();
        let sym = format!("_{RESULT_PREFIX}{index}_").to_string();
        let id = self.new_variable(&sym, Domain::any());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn add_var_to_domain(&mut self, domain: &DomainId, var: &VarId) {
        self.domains[*domain].vars.push(*var)
    }

    pub fn new_timepoint(&mut self) -> VarId {
        let index = self.meta_data.new_timepoint_index();
        let sym = format!("_{TIMEPOINT_PREFIX}{index}_").to_string();
        let id = self.new_variable(&sym, self.get_type_as_domain(TYPE_TIMEPOINT));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_if(&mut self) -> (VarId, VarId, VarId) {
        let index = self.meta_data.new_if_index();

        let sym_if = &format!("_{IF_PREFIX}{index}_");
        let id_if = self.new_variable(sym_if, self.get_type_as_domain(TYPE_TASK));
        self.ids.insert(&sym_if, &id_if);

        let sym_m_true = &format!("m_{}_true", sym_if);
        let id_m_true = self.new_variable(sym_m_true, self.get_type_as_domain(TYPE_METHOD));
        self.ids.insert(&sym_m_true, &id_m_true);

        let sym_m_false = &format!("m_{}_false", sym_if);
        let id_m_false = self.new_variable(sym_m_true, self.get_type_as_domain(TYPE_METHOD));
        self.ids.insert(&sym_m_false, &id_m_false);

        (id_if, id_m_true, id_m_false)
    }

    pub fn new_handle(&mut self) -> VarId {
        let index = self.meta_data.new_handle_index();
        let sym = &format!("_{HANDLE_PREFIX}{index}_");
        let id = self.new_variable(sym, Domain::composed(Handle as usize, vec![Any]));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_start(&mut self) -> VarId {
        let index = self.meta_data.new_start_index();
        let sym = &format!("_{START_PREFIX}{index}_");
        let id = self.new_parameter(sym, self.get_type_as_domain(TYPE_TIMEPOINT));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_end(&mut self) -> VarId {
        let index = self.meta_data.new_end_index();
        let sym = &format!("_{END_PREFIX}{index}_");
        let id = self.new_parameter(sym, self.get_type_as_domain(TYPE_TIMEPOINT));
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_presence(&mut self) -> VarId {
        let index = self.meta_data.new_presence_index();
        let sym = &format!("_{PRESENCE_PREFIX}{index}_");
        let id = self.new_parameter(sym, Boolean);
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_chronicle_result(&mut self) -> VarId {
        let index = self.meta_data.new_chronicle_result_index();
        let sym = &format!("_{CHRONICLE_RESULT_PREFIX}{index}_");
        let id = self.new_parameter(sym, Domain::any());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_symbol(&mut self, sym: impl Display) -> VarId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.get_sym_id(sym).unwrap()
        } else {
            let sym: &String = &sym.to_string();
            let id = self.new_variable(sym, sym.as_str());
            self.ids.insert(&sym, &id);
            id
        }
    }

    pub fn new_constant_symbol(&mut self, sym: impl Display, domain: impl Into<Domain>) -> VarId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.get_sym_id(sym).unwrap()
        } else {
            let sym: &String = &sym.to_string();
            let id = self.new_variable(
                sym,
                Domain::Cst(Box::new(domain.into()), Cst::Symbol(sym.to_string())),
            );
            self.ids.insert(&sym, &id);
            id
        }
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: impl Into<Domain>) -> VarId {
        let symbol = symbol.to_string();
        let version = self.ids.version(&symbol);
        let sym = format!("{symbol}_{version}");
        let domain_id = self.domains.new_node(VarDomain::new(domain));
        let id = self
            .variables
            .new_node(Variable::new_parameter(&sym, domain_id));
        self.add_var_to_domain(&domain_id, &id);
        self.ids.insert(&sym, &id);
        id
    }

    /*
    GETTERS
    */

    //VarDomain
    pub fn get_var_domain(&self, id: &DomainId) -> &VarDomain {
        &self.domains[*id]
    }

    pub fn get_domain(&self, id: &DomainId) -> &Domain {
        &self.domains[*id].domain
    }

    pub fn get_domain_parent(&self, id: &DomainId) -> DomainId {
        *self.domains.get_parent(id)
    }

    //Variable
    pub fn get_variable(&self, id: &VarId) -> &Variable {
        &self.variables[*id]
    }

    pub fn get_domain_id(&self, v: &VarId) -> DomainId {
        self.variables[*v].domain
    }

    pub fn get_label(&self, id: &VarId, parent: bool) -> &str {
        let id = match parent {
            true => self.get_var_parent(id),
            false => *id,
        };

        self.variables[id].label.as_str()
    }

    pub fn get_var_parent(&self, v: &VarId) -> VarId {
        *self.variables.get_parent(v)
    }

    pub fn get_domain_of_var(&self, v: &VarId) -> &Domain {
        self.get_domain(&self.get_domain_parent(&self.get_domain_id(v)))
    }

    pub fn get_sym_id(&self, sym: &str) -> Option<VarId> {
        self.ids.get_id(sym)
    }

    /*pub fn find_parent(&mut self, a: &VarId) -> VarId {
        *self.domains.find(a)
    }*/

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

    pub fn meet_domains(&self, id_d1: &VarId, id_d2: &VarId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn union_domains(&self, id_d1: &VarId, id_d2: &VarId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn substract_domains(&self, id_d1: &VarId, id_d2: &VarId) -> Domain {
        let d1 = &self.domains[*id_d1].domain;
        let d2 = &self.domains[*id_d2].domain;

        self.substract(d1, d2)
    }

    pub fn update_domains(&mut self, mut queue: VecDeque<Update>) -> EmptyDomains {
        let mut emptys = EmptyDomains::None;

        while let Some(update) = queue.pop_front() {
            let id = self.get_var_parent(&update.id);
            let d = self.domains[id].domain.clone();

            emptys.append((update.closure)(self));

            if d != self.domains[id].domain {
                //queue.push_back()
                queue.append(&mut self.domains[id].updates.clone().into());
            }
        }
        emptys
    }

    pub fn meet_to_domain(&mut self, id: &VarId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = &self.get_var_parent(id);

        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[*id].clone();
        let domain = domain.into();
        //if var_domain.constraints.is_empty() {
        let d1 = &self.domains[*id].domain;

        let d = self.meet(d1, &domain.into());
        if d.is_empty() {
            emptys.append(EmptyDomains::Some(vec![*id]));
        }

        self.domains[*id].domain = d;

        emptys.append(self.update_domains(var_domain.updates.clone().into()));

        emptys
    }

    pub fn subtract_to_domain(&mut self, id: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = &self.get_var_parent(id);

        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[*id].clone();
        let domain = domain.into();
        //if var_domain.constraints.is_empty() {
        let d1 = &self.domains[*id].domain;

        let d = self.substract(d1, &domain.into());
        if d.is_empty() {
            emptys.append(EmptyDomains::Some(vec![*id]));
        }

        self.domains[*id].domain = d;

        emptys.append(self.update_domains(var_domain.updates.clone().into()));

        emptys
    }

    pub fn set_domain(&mut self, id: &VarId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = &self.get_var_parent(id);
        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[*id].clone();
        let domain = domain.into();
        //if var_domain.constraints.is_empty() {
        if domain.is_empty() {
            emptys.append(EmptyDomains::Some(vec![*id]));
        }

        self.domains[*id].domain = domain;

        emptys.append(self.update_domains(var_domain.updates.clone().into()));

        emptys
    }

    pub fn add_update(&mut self, elements: Vec<VarId>, update: Update) {
        for element in elements {
            if element != update.id {
                self.domains[element].updates.push(update.clone());
            }
        }

        self.update_domains(vec![update].into());
    }

    pub fn remove_update(&mut self, id: &VarId, dependent: &VarId) {
        let id = self.get_var_parent(id);
        let dependent = self.get_var_parent(dependent);

        let mut updates = self.domains[id].updates.clone();
        updates.retain(|up| self.get_var_parent(&up.id) != dependent);
        self.domains[id].updates = updates;
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        self.lattice.contained_in(d1, d2)
    }

    pub fn get_type_as_domain(&self, r#type: impl Into<SimpleType>) -> Domain {
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

    pub fn union_var(&mut self, v1: &VarId, v2: &VarId) -> EmptyDomains {
        let v1 = self.get_var_parent(v1);
        let v2 = self.get_var_parent(v2);

        let (v1, v2) = if self.variables[v2].parameter {
            (v2, v1)
        } else {
            (v1, v2)
        };

        let r = self.union_domain(&self.get_domain_id(&v1), &self.get_domain_id(&v2));
        self.variables.union_ordered(&v1, &v2);
        r
    }

    pub fn union_domain(&mut self, d1: &DomainId, d2: &DomainId) -> EmptyDomains {
        let d1 = self.get_domain_parent(d1);
        let d2 = self.get_domain_parent(d2);
        let domain_2 = self.domains[d2].domain.clone();
        let mut updates = self.domains[d1].updates.clone();
        updates.append(&mut self.domains[d2].updates.clone());

        self.domains[d1].updates = updates.clone();
        self.domains.union_ordered(&d1, &d2);
        self.remove_update(&d1, &d1);

        let r = self.meet_to_domain(&d1, domain_2);

        r
    }

    /*
    FORMAT Function
      */

    pub fn format_variable(&self, id: &VarId) -> String {
        let var = &self.variables[*id];
        let d = &self.domains[var.domain];

        match &d.domain {
            Domain::Cst(_, cst) => cst.to_string(),
            Domain::Simple(TYPE_ID_TRUE) => true.to_string(),
            Domain::Simple(TYPE_ID_FALSE) => false.to_string(),
            Domain::Simple(TYPE_ID_NIL) => NIL.to_string(),
            _ => var.label.to_string(),
        }
    }

    pub fn format_domain_id(&self, domain: &DomainId) -> String {
        self.domains[*domain].domain.format(&self.lattice)
    }

    pub fn format_domain(&self, domain: &Domain) -> String {
        domain.format(&self.lattice)
    }

    pub fn format_var_domain(&self, id: &DomainId) -> String {
        let domain = &self.domains[*id];
        let mut str = format!("domain = {}", domain.domain.format(&self.lattice));

        if !domain.updates.is_empty() {
            write!(str, ", dependent(s) = {{").unwrap();
            for (i, up) in domain.updates.iter().enumerate() {
                if i != 0 {
                    str.push(',');
                }
                write!(str, "{}", self.get_var_parent(&up.id)).unwrap();
            }

            write!(str, "}}").unwrap();
        }
        str
    }

    pub fn export_lattice_dot(&self) -> Dot {
        self.lattice.export_dot()
    }

    pub fn get_lattice(&self) -> TypeLattice {
        self.lattice.clone()
    }
}

pub fn lvalue_to_domain(lv: &LValue, st: &mut RefSymTable) -> Result<Domain, LRuntimeError> {
    match lv {
        LValue::List(list) => {
            let mut vec = vec![];
            for e in list.iter() {
                vec.push(lvalue_to_domain(e, st)?);
            }
            Ok(Domain::Composed(BasicType::List as usize, vec))
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
        lv => Ok(match st.get_sym_id(&lv.to_string()) {
            Some(id) => id.into(),
            None => {
                //println!("symbol {} does not exist", lv.to_string());
                st.new_symbol(&lv.to_string()).into()
            }
        }),
    }
}

pub const MAX_Q: &str = "max-q";
pub const QUANTITY: &str = "quantity";
