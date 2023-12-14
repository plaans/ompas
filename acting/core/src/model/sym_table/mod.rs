use crate::model::chronicle;
use crate::model::sym_domain::basic_type::BasicType::{Any, Boolean, Handle};
use crate::model::sym_domain::basic_type::{
    BasicType, TYPE_ID_FALSE, TYPE_ID_INT, TYPE_ID_NIL, TYPE_ID_NUMBER, TYPE_ID_TRUE,
};
use crate::model::sym_domain::cst::Cst;
use crate::model::sym_domain::type_lattice::TypeLattice;
use crate::model::sym_domain::{cst, Domain};
use crate::model::sym_table::closure::Update;
use crate::model::sym_table::forest::{Forest, NodeId};
use crate::model::sym_table::id::SymbolTableId;
use crate::model::sym_table::meta_data::SymTableMetaData;
use crate::model::sym_table::r#ref::RefSymTable;
use crate::model::sym_table::var_domain::VarDomain;
use crate::model::sym_table::variable::Variable;
use crate::ompas::manager::acting::acting_var::AsCst;
use crate::planning::conversion::flow_graph::graph::Dot;
use new_type::newtype;
use ompas_language::exec::resource::{MAX_Q, QUANTITY};
use ompas_language::exec::state::{INSTANCE, UNKNOWN};
use ompas_language::sym_table::*;
use sompas_language::kind::{ERR, NIL};
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use sompas_structs::lruntimeerror::LRuntimeError;
use sompas_structs::lvalue::LValue;
use std::collections::VecDeque;
use std::fmt::Write;
use std::fmt::{Display, Formatter};

pub mod closure;
pub mod forest;
pub mod id;
pub mod meta_data;
pub mod r#ref;
pub mod r#trait;
pub mod var_domain;
pub mod variable;

newtype!(VarId: usize);
impl From<VarId> for NodeId {
    fn from(value: VarId) -> Self {
        NodeId(value.0)
    }
}

impl From<NodeId> for VarId {
    fn from(value: NodeId) -> Self {
        VarId(value.0)
    }
}

impl Display for VarId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

newtype!(DomainId: usize);

impl From<DomainId> for NodeId {
    fn from(value: DomainId) -> Self {
        NodeId(value.0)
    }
}

impl From<NodeId> for DomainId {
    fn from(value: NodeId) -> Self {
        DomainId(value.0)
    }
}

impl Display for DomainId {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

#[derive(Clone)]
pub enum EmptyDomains {
    None,
    Some(Vec<DomainId>),
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

    pub fn is_none(&self) -> bool {
        matches!(&self, &Self::None)
    }
}

impl Default for EmptyDomains {
    fn default() -> Self {
        Self::None
    }
}

#[derive(Clone)]
pub struct SymTable {
    domains: Forest<VarDomain, DomainId>,
    variables: Forest<Variable, VarId>,
    pub(in crate::model::sym_table) lattice: TypeLattice,
    ids: SymbolTableId,
    meta_data: SymTableMetaData,
}

impl Default for SymTable {
    fn default() -> Self {
        Self::new_from(TypeLattice::new())
    }
}

impl SymTable {
    pub fn clear(&mut self) {
        *self = Self::new_from(self.lattice.clone())
    }

    pub fn new_from(lattice: TypeLattice) -> Self {
        let mut st = Self {
            domains: Default::default(),
            variables: Default::default(),
            lattice,
            ids: Default::default(),
            meta_data: Default::default(),
        };

        st.new_constant_symbol(
            EPSILON,
            Domain::constant(
                Domain::Simple(TYPE_ID_NUMBER),
                Cst::Symbol(EPSILON.to_string()),
            ),
        );

        st.new_constant_symbol(
            INSTANCE,
            Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION).unwrap()),
                vec![st.get_type_as_domain(TYPE_OBJECT).unwrap()],
                Box::new(st.get_type_as_domain(TYPE_OBJECT_TYPE).unwrap()),
            ),
        );

        st.new_constant_symbol(
            INSTANCE,
            Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION).unwrap()),
                vec![st.get_type_as_domain(TYPE_OBJECT).unwrap()],
                Box::new(st.get_type_as_domain(TYPE_OBJECT_TYPE).unwrap()),
            ),
        );

        st.new_constant_symbol(
            QUANTITY,
            Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION).unwrap()),
                vec![st.get_type_as_domain(TYPE_OBJECT).unwrap()],
                Box::new(Domain::Simple(TYPE_ID_INT)),
            ),
        );

        st.new_constant_symbol(
            MAX_Q,
            Domain::Application(
                Box::new(st.get_type_as_domain(TYPE_STATE_FUNCTION).unwrap()),
                vec![st.get_type_as_domain(TYPE_OBJECT).unwrap()],
                Box::new(Domain::Simple(TYPE_ID_INT)),
            ),
        );

        //Add unk as symbol
        st.new_constant_symbol(UNKNOWN, Domain::any());

        st
    }

    /*
    SCOPES FUNCTIONS
     */

    pub fn set_declaration(&mut self, id: VarId, timepoint: VarId) {
        let id = self.get_var_parent(id);
        let timepoint = self.get_var_parent(timepoint);
        self.variables[id].declaration = Some(timepoint);
    }

    pub fn set_drop(&mut self, id: VarId, timepoint: VarId) {
        let id = self.get_var_parent(id);
        let timepoint = self.get_var_parent(timepoint);
        self.variables[id].drop = Some(timepoint);
    }

    pub fn get_declaration(&mut self, id: VarId) -> Option<VarId> {
        //let id = self.get_var_parent(id);
        self.variables[id].declaration
    }

    pub fn get_drop(&mut self, id: VarId) -> Option<VarId> {
        let id = self.get_var_parent(id);
        self.variables[id].drop.map(|t| self.get_var_parent(t))
    }

    pub fn get_domain_vars(&mut self, d: DomainId) -> Vec<VarId> {
        let d = self.get_domain_parent(d);
        self.domains[d].vars.clone()
    }

    pub fn add_var_to_domain(&mut self, domain: DomainId, var: VarId) {
        let domain = self.get_domain_parent(domain);
        let var = self.get_var_parent(var);
        self.domains[domain].vars.push(var)
    }
}

// New functions
impl SymTable {
    pub fn new_variable(&mut self, sym: impl Display, domain: impl Into<Domain>) -> VarId {
        let domain_id = self.domains.new_node(VarDomain::new(domain));
        let id = self.variables.new_node(Variable::new(sym, domain_id));
        self.add_var_to_domain(domain_id, id);
        id
    }

    pub fn new_bool(&mut self, b: bool) -> VarId {
        self.new_variable(b, b)
    }

    pub fn new_nil(&mut self) -> VarId {
        self.new_variable(NIL, BasicType::Nil)
    }

    pub fn new_err(&mut self) -> VarId {
        self.new_variable(ERR, BasicType::Err)
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

    pub fn new_interval(&mut self) -> chronicle::interval::Interval {
        chronicle::interval::Interval::new(self.new_timepoint(), self.new_timepoint())
    }

    pub fn new_cst(&mut self, cst: cst::Cst) -> VarId {
        match cst {
            Cst::Int(i) => self.new_int(i),
            Cst::Float(f) => self.new_float(f),
            Cst::Symbol(s) => self.new_symbol(s),
            Cst::Bool(b) => self.new_bool(b),
        }
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> VarId {
        let index = self.meta_data.new_result_index();
        let sym = format!("{RESULT_PREFIX}{index}");
        let id = self.new_variable(&sym, Domain::any());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_timepoint(&mut self) -> VarId {
        let index = self.meta_data.new_timepoint_index();
        let sym = format!("{TIMEPOINT_PREFIX}{index}");
        let id = self.new_variable(&sym, self.get_type_as_domain(TYPE_TIMEPOINT).unwrap());
        self.ids.insert(&sym, &id);
        id
    }

    pub fn new_start(&mut self) -> VarId {
        let index = self.meta_data.new_start_index();
        let sym = &format!("{START_PREFIX}{index}");
        let id = self.new_parameter(
            sym,
            self.get_type_as_domain(TYPE_TIMEPOINT).unwrap(),
            VarId(0),
        );
        self.set_declaration(id, id);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_end(&mut self, start: VarId) -> VarId {
        let index = self.meta_data.new_end_index();
        let sym = &format!("{END_PREFIX}{index}");
        let id = self.new_parameter(sym, self.get_type_as_domain(TYPE_TIMEPOINT).unwrap(), start);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_start_task(&mut self, start: VarId) -> VarId {
        let index = self.meta_data.new_start_task_index();
        let sym = &format!("{START_TASK_PREFIX}{index}");
        let id = self.new_parameter(sym, self.get_type_as_domain(TYPE_TIMEPOINT).unwrap(), start);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_end_task(&mut self, start: VarId) -> VarId {
        let index = self.meta_data.new_end_task_index();
        let sym = &format!("{END_TASK_PREFIX}{index}");
        let id = self.new_parameter(sym, self.get_type_as_domain(TYPE_TIMEPOINT).unwrap(), start);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_if(&mut self) -> (VarId, VarId, VarId) {
        let index = self.meta_data.new_if_index();

        let sym_if = &format!("{IF_PREFIX}{index}");
        let id_if = self.new_variable(
            sym_if,
            Domain::Cst(
                Box::new(self.get_type_as_domain(TYPE_TASK).unwrap()),
                cst::Cst::Symbol(sym_if.to_string()),
            ),
        );
        self.ids.insert(sym_if, &id_if);

        let sym_m_true = &format!("m_{}_true", sym_if);
        let id_m_true = self.new_variable(
            sym_m_true,
            Domain::Cst(
                Box::new(self.get_type_as_domain(TYPE_METHOD).unwrap()),
                cst::Cst::Symbol(sym_m_true.to_string()),
            ),
        );
        self.ids.insert(sym_m_true, &id_m_true);

        let sym_m_false = &format!("m_{}_false", sym_if);
        let id_m_false = self.new_variable(
            sym_m_false,
            Domain::Cst(
                Box::new(self.get_type_as_domain(TYPE_METHOD).unwrap()),
                Cst::Symbol(sym_m_false.to_string()),
            ),
        );
        self.ids.insert(sym_m_false, &id_m_false);

        (id_if, id_m_true, id_m_false)
    }

    pub fn new_handle(&mut self) -> VarId {
        let index = self.meta_data.new_handle_index();
        let sym = &format!("{HANDLE_PREFIX}{index}");
        let id = self.new_variable(sym, Domain::composed(Handle as usize, vec![Any]));
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_presence(&mut self, start: VarId) -> VarId {
        let index = self.meta_data.new_presence_index();
        let sym = &format!("{PRESENCE_PREFIX}{index}");
        //self.get_type_as_domain(TYPE_PRESENCE).unwrap()
        let id = self.new_parameter(sym, Boolean, start);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_chronicle_result(&mut self, start: VarId) -> VarId {
        let index = self.meta_data.new_chronicle_result_index();
        let sym = &format!("{CHRONICLE_RESULT_PREFIX}{index}");
        let id = self.new_parameter(sym, Domain::any(), start);
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_arbitrary(&mut self) -> VarId {
        let index = self.meta_data.new_arbitrary_index();
        let sym = &format!("{ARBITRARY_PREFIX}{index}");
        let id = self.new_variable(sym, Domain::any());
        self.ids.insert(sym, &id);
        id
    }

    pub fn new_symbol(&mut self, sym: impl Display) -> VarId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            self.get_sym_id(sym).unwrap()
        } else {
            let sym: &String = &sym.to_string();
            let id = self.new_variable(sym, sym.as_str());
            self.ids.insert(sym, &id);
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
            self.ids.insert(sym, &id);
            id
        }
    }

    pub fn new_parameter(
        &mut self,
        symbol: impl ToString,
        domain: impl Into<Domain>,
        declaration: VarId,
    ) -> VarId {
        let symbol = symbol.to_string();
        let version = self.ids.version(&symbol);
        let sym = format!("{symbol}_{version}");
        let domain_id = self.domains.new_node(VarDomain::new(domain));
        let id = self
            .variables
            .new_node(Variable::new_parameter(sym, domain_id));
        self.add_var_to_domain(domain_id, id);
        self.ids.insert(&symbol, &id);
        self.set_declaration(id, declaration);
        id
    }

    pub fn duplicate(&mut self, id: VarId) -> VarId {
        let id = self.get_var_parent(id);
        let domain_id = self.get_domain_id(id);
        let domain = &self.domains[domain_id].clone();
        let var_domain = VarDomain {
            domain: domain.domain.clone(),
            updates: vec![],
            vars: vec![],
        };

        let domain_id = self.domains.new_node(var_domain);
        let variable = &self.variables[id];

        let symbol = variable.label.to_string();
        let version = self.ids.version(&symbol);
        let sym = format!("{symbol}_{version}");
        //println!("dup");

        let variable = Variable {
            domain: domain_id,
            parameter: variable.parameter,
            label: sym,
            declaration: None,
            drop: None,
        };
        let var_id = self.variables.new_node(variable);
        self.add_var_to_domain(domain_id, var_id);
        self.ids.insert(&symbol, &id);
        var_id
    }
}
/*
GETTERS
*/
impl SymTable {
    //VarDomain
    pub fn get_var_domain(&mut self, id: DomainId) -> &VarDomain {
        let id = self.get_domain_parent(id);
        &self.domains[id]
    }

    pub fn get_domain(&mut self, id: DomainId) -> &Domain {
        let id = self.get_domain_parent(id);
        &self.domains[id].domain
    }

    pub fn get_domain_parent(&mut self, id: DomainId) -> DomainId {
        self.domains.find(id)
    }

    pub fn get_var_parent(&mut self, v: VarId) -> VarId {
        self.variables.find(v)
    }

    //Variable
    pub fn get_variable(&mut self, id: VarId) -> &Variable {
        let id = self.get_var_parent(id);
        &self.variables[id]
    }

    pub fn get_domain_id(&mut self, v: VarId) -> DomainId {
        let v = self.get_var_parent(v);
        let d = self.variables[v].domain;
        self.get_domain_parent(d)
    }

    pub fn get_label(&mut self, id: VarId, parent: bool) -> &str {
        let id = match parent {
            true => self.get_var_parent(id),
            false => id,
        };

        self.variables[id].label.as_str()
    }

    pub fn get_domain_of_var(&mut self, v: VarId) -> &Domain {
        let domain_id = self.get_domain_id(v);
        self.get_domain(domain_id)
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
        self.lattice.meet(d1, d2)
    }

    pub fn union(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.union(d1, d2)
    }

    pub fn substract(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.lattice.substract(d1, d2)
    }

    pub fn meet_domains(&mut self, id_d1: DomainId, id_d2: DomainId) -> Domain {
        let id_d1 = self.get_domain_parent(id_d1);
        let id_d2 = self.get_domain_parent(id_d2);
        let d1 = &self.domains[id_d1].domain;
        let d2 = &self.domains[id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn union_domains(&mut self, id_d1: DomainId, id_d2: DomainId) -> Domain {
        let id_d1 = self.get_domain_parent(id_d1);
        let id_d2 = self.get_domain_parent(id_d2);
        let d1 = &self.domains[id_d1].domain;
        let d2 = &self.domains[id_d2].domain;

        self.meet(d1, d2)
    }

    pub fn substract_domains(&mut self, id_d1: DomainId, id_d2: DomainId) -> Domain {
        let id_d1 = self.get_domain_parent(id_d1);
        let id_d2 = self.get_domain_parent(id_d2);
        let d1 = &self.domains[id_d1].domain;
        let d2 = &self.domains[id_d2].domain;

        self.substract(d1, d2)
    }

    pub fn update_domains(&mut self, mut queue: VecDeque<Update>) -> EmptyDomains {
        let mut emptys = EmptyDomains::None;

        while let Some(update) = queue.pop_front() {
            let domain = self.get_domain_parent(update.id);
            let d = self.domains[domain].domain.clone();

            emptys.append((update.closure)(self));

            if d != self.domains[domain].domain {
                queue.append(&mut self.domains[domain].updates.clone().into());
            }
        }
        emptys
    }

    pub fn meet_to_domain(&mut self, id: DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = self.get_domain_parent(id);
        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[id].clone();
        let domain = domain.into();
        let d1 = &self.domains[id].domain;

        let d = self.meet(d1, &domain);
        if d.is_empty() {
            emptys.append(EmptyDomains::Some(vec![id]));
        }

        self.domains[id].domain = d;

        emptys.append(self.update_domains(var_domain.updates.into()));
        emptys
    }

    pub fn subtract_to_domain(&mut self, id: DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = self.get_domain_parent(id);

        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[id].clone();
        let domain = domain.into();
        //if var_domain.constraints.is_empty() {
        let d1 = &self.domains[id].domain;

        let d = self.substract(d1, &domain);
        if d.is_empty() {
            emptys.append(EmptyDomains::Some(vec![id]));
        }

        self.domains[id].domain = d;

        emptys.append(self.update_domains(var_domain.updates.into()));

        emptys
    }

    pub fn set_domain(&mut self, id: DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        let id = self.get_domain_parent(id);
        let mut emptys = EmptyDomains::None;

        let var_domain = self.domains[id].clone();
        let domain = domain.into();
        //if var_domain.constraints.is_empty() {
        if domain.is_empty() {
            emptys.append(EmptyDomains::Some(vec![id]));
        }

        self.domains[id].domain = domain;

        emptys.append(self.update_domains(var_domain.updates.into()));

        emptys
    }

    pub fn set_domain_of_var(&mut self, var: VarId, domain: impl Into<Domain>) -> EmptyDomains {
        let domain_id = self.get_domain_id(var);
        self.set_domain(domain_id, domain)
    }

    pub fn add_update(&mut self, elements: Vec<DomainId>, update: Update) {
        for element in elements {
            let element = self.get_domain_parent(element);
            if element != update.id {
                self.domains[element].updates.push(update.clone());
            }
        }

        self.update_domains(vec![update].into());
    }

    pub fn remove_update(&mut self, id: DomainId, dependent: DomainId) {
        let id = self.get_domain_parent(id);
        let dependent = self.get_domain_parent(dependent);

        let mut updates = self.domains[id].updates.clone();
        updates.retain(|up| self.get_domain_parent(up.id) != dependent);
        self.domains[id].updates = updates;
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        self.lattice.contained_in(d1, d2)
    }

    pub fn get_type_as_domain(&self, r#type: impl Display) -> Option<Domain> {
        self.lattice.get_type_id(r#type).map(|t| t.into())
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

    pub fn union_var(&mut self, v1: VarId, v2: VarId) -> EmptyDomains {
        let v1 = self.get_var_parent(v1);
        let v2 = self.get_var_parent(v2);
        if v1 != v2 {
            let (v1, v2) = if self.variables[v2].parameter
                || self.domains[self.variables[v2].domain].domain.is_constant()
            {
                (v2, v1)
            } else {
                (v1, v2)
            };
            let d1 = self.get_domain_id(v1);
            let d2 = self.get_domain_id(v2);
            let r = self.union_domain(d1, d2);
            self.variables.union_ordered(v1, v2);
            r
        } else {
            EmptyDomains::None
        }
    }

    pub fn union_domain(&mut self, d1: DomainId, d2: DomainId) -> EmptyDomains {
        let d1 = self.get_domain_parent(d1);
        let d2 = self.get_domain_parent(d2);
        if d1 != d2 {
            let domain_2 = self.domains[d2].domain.clone();
            let mut updates = self.domains[d1].updates.clone();

            updates.append(&mut self.domains[d2].updates.clone());

            self.domains[d1].updates = updates;
            self.domains.union_ordered(d1, d2);
            self.remove_update(d1, d1);

            self.meet_to_domain(d1, domain_2)
        } else {
            EmptyDomains::None
        }
    }

    /*
    OTHER
     */

    pub fn var_as_cst(&mut self, var_id: VarId) -> Option<Cst> {
        self.get_domain_of_var(var_id).as_cst()
    }

    /*
    FORMAT Function
      */

    pub fn format_variable(&mut self, id: VarId) -> String {
        let id = self.get_var_parent(id);
        let d = self.get_domain_id(id);

        match &self.domains[d].domain {
            Domain::Cst(_, cst) => cst.to_string(),
            Domain::Simple(TYPE_ID_TRUE) => true.to_string(),
            Domain::Simple(TYPE_ID_FALSE) => false.to_string(),
            Domain::Simple(TYPE_ID_NIL) => NIL.to_string(),
            _ => self.get_variable(id).label.to_string(),
        }
    }

    pub fn format_domain_id(&mut self, domain: DomainId) -> String {
        let domain = self.get_domain_parent(domain);
        self.domains[domain].domain.format(&self.lattice)
    }

    pub fn format_domain(&self, domain: &Domain) -> String {
        domain.format(&self.lattice)
    }

    pub fn format_var_domain(&mut self, id: DomainId) -> String {
        let id = self.get_domain_parent(id);
        let domain = &self.domains[id];
        let mut str = format!("domain = {}", domain.domain.format(&self.lattice));
        let mut domain_up_ids: Vec<_> = domain.updates.iter().map(|u| u.id).collect();
        if !domain_up_ids.is_empty() {
            write!(str, ", dependent(s) = {{").unwrap();
            for (i, up) in domain_up_ids.drain(..).enumerate() {
                if i != 0 {
                    str.push(',');
                }
                write!(str, "{}", self.get_domain_parent(up)).unwrap();
            }

            write!(str, "}}").unwrap();
        }
        str
    }

    pub fn export_lattice_dot(&self) -> Dot {
        self.lattice.export_dot()
    }

    pub fn get_lattice(&self) -> &TypeLattice {
        &self.lattice
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
        lv => Ok({
            let id = match st.get_sym_id(&lv.to_string()) {
                Some(id) => id,
                None => {
                    //println!("symbol {} does not exist", lv.to_string());
                    st.new_symbol(lv.to_string())
                }
            };
            st.get_domain_of_var(id)
        }),
    }
}
