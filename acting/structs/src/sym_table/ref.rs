use crate::conversion::chronicle::interval;
use crate::conversion::flow_graph::graph::Dot;
use crate::sym_table::closure::Update;
use crate::sym_table::domain::cst::Cst;
use crate::sym_table::domain::type_lattice::TypeLattice;
use crate::sym_table::domain::{cst, Domain};
use crate::sym_table::var_domain::VarDomain;
use crate::sym_table::variable::Variable;
use crate::sym_table::{DomainId, EmptyDomains, SymTable, VarId};
use sompas_structs::lnumber::LNumber;
use std::fmt::{Display, Formatter};
use std::sync::{Arc, RwLock};

#[derive(Default, Clone)]
pub struct RefSymTable(Arc<RwLock<SymTable>>);

impl RefSymTable {
    /*
    SCOPES FUNCTIONS
     */

    pub fn set_declaration(&self, id: &VarId, timepoint: &VarId) {
        self.0.write().unwrap().set_declaration(id, timepoint)
    }

    pub fn set_drop(&self, id: &VarId, timepoint: &VarId) {
        self.0.write().unwrap().set_drop(id, timepoint)
    }

    pub fn get_declaration(&self, id: &VarId) -> Option<VarId> {
        self.0.read().unwrap().get_declaration(id)
    }

    pub fn get_drop(&self, id: &VarId) -> Option<VarId> {
        self.0.read().unwrap().get_drop(id)
    }

    pub fn get_domain_vars(&self, d: &DomainId) -> Vec<VarId> {
        self.0.read().unwrap().get_domain_vars(d)
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_bool(&self, b: bool) -> VarId {
        self.0.write().unwrap().new_bool(b)
    }

    pub fn new_nil(&self) -> VarId {
        self.0.write().unwrap().new_nil()
    }

    pub fn new_int(&self, i: i64) -> VarId {
        self.0.write().unwrap().new_int(i)
    }

    pub fn new_float(&self, f: f64) -> VarId {
        self.0.write().unwrap().new_float(f)
    }

    pub fn new_number(&self, n: &LNumber) -> VarId {
        self.0.write().unwrap().new_number(n)
    }

    pub fn new_interval(&self) -> interval::Interval {
        self.0.write().unwrap().new_interval()
    }

    pub fn new_cst(&self, cst: cst::Cst) -> VarId {
        self.0.write().unwrap().new_cst(cst)
    }

    /*
    GETTERS
    */

    //VarDomain
    pub fn get_var_domain(&self, id: &DomainId) -> VarDomain {
        self.0.read().unwrap().get_var_domain(id).clone()
    }

    pub fn get_domain(&self, id: &DomainId) -> Domain {
        self.0.read().unwrap().get_domain(id).clone()
    }

    pub fn get_domain_parent(&self, id: &DomainId) -> DomainId {
        self.0.read().unwrap().get_domain_parent(id)
    }

    //Variable
    pub fn get_variable(&self, id: &VarId) -> Variable {
        self.0.read().unwrap().get_variable(id).clone()
    }

    pub fn get_domain_id(&self, v: &VarId) -> DomainId {
        self.0.read().unwrap().get_domain_id(v)
    }

    pub fn get_label(&self, id: &VarId, parent: bool) -> String {
        self.0.read().unwrap().get_label(id, parent).to_string()
    }

    pub fn get_var_parent(&self, v: &VarId) -> VarId {
        self.0.read().unwrap().get_var_parent(v)
    }

    pub fn get_domain_of_var(&self, v: &VarId) -> Domain {
        self.0.read().unwrap().get_domain_of_var(v).clone()
    }

    pub fn get_sym_id(&self, sym: &str) -> Option<VarId> {
        self.0.read().unwrap().get_sym_id(sym)
    }

    /*pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }*/
    /*
    BOOLEAN FUNCTION
     */
    pub fn it_exists(&self, sym: &str) -> bool {
        self.0.read().unwrap().it_exists(sym)
    }

    /*
    DECLARATION FUNCTION
     */
    /*pub fn new_type(& self, sym: impl Display, a_type: Option<TypeId>) -> TypeId {
        self.0.write().unwrap().new_type(sym, a_type)
    }*/

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&self) -> VarId {
        self.0.write().unwrap().new_result()
    }

    pub fn new_timepoint(&self) -> VarId {
        self.0.write().unwrap().new_timepoint()
    }

    pub fn new_if(&self) -> (VarId, VarId, VarId) {
        self.0.write().unwrap().new_if()
    }

    pub fn new_handle(&self) -> VarId {
        self.0.write().unwrap().new_handle()
    }

    pub fn new_start(&self) -> VarId {
        self.0.write().unwrap().new_start()
    }

    pub fn new_end(&self) -> VarId {
        self.0.write().unwrap().new_end()
    }

    pub fn new_presence(&self) -> VarId {
        self.0.write().unwrap().new_presence()
    }

    pub fn new_chronicle_result(&self) -> VarId {
        self.0.write().unwrap().new_chronicle_result()
    }

    pub fn new_symbol(&self, sym: impl Display) -> VarId {
        self.0.write().unwrap().new_symbol(sym)
    }

    pub fn new_parameter(&self, symbol: impl ToString, domain: impl Into<Domain>) -> VarId {
        self.0.write().unwrap().new_parameter(symbol, domain)
    }

    /*
    Domain operators
     */

    pub fn meet(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.0.read().unwrap().meet(d1, d2)
    }

    pub fn union(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.0.read().unwrap().union(d1, d2)
    }

    pub fn substract(&self, d1: &Domain, d2: &Domain) -> Domain {
        self.0.read().unwrap().substract(d1, d2)
    }

    pub fn meet_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        self.0.read().unwrap().meet_domains(id_d1, id_d2)
    }

    pub fn union_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        self.0.read().unwrap().union_domains(id_d1, id_d2)
    }

    pub fn substract_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        self.0.read().unwrap().substract_domains(id_d1, id_d2)
    }

    pub fn meet_to_domain(&self, id_d1: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        self.0.write().unwrap().meet_to_domain(id_d1, domain)
    }

    pub fn substract_to_domain(&self, id_d1: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        self.0.write().unwrap().subtract_to_domain(id_d1, domain)
    }

    pub fn set_domain(&self, id: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        self.0.write().unwrap().set_domain(id, domain)
    }

    pub fn add_update(&self, elements: Vec<VarId>, update: Update) {
        self.0.write().unwrap().add_update(elements, update);
    }

    pub fn remove_update(&self, id: &VarId, dependent: &VarId) {
        self.0.write().unwrap().remove_update(id, dependent);
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        self.0.read().unwrap().contained_in_domain(d1, d2)
    }

    pub fn get_type_as_domain(&self, r#type: impl Display) -> Option<Domain> {
        self.0.read().unwrap().get_type_as_domain(r#type)
    }

    /*
    FOREST FUNCTIONS
     */
    pub fn flat_bindings(&self) {
        self.0.write().unwrap().flat_bindings()
    }

    pub fn union_var(&self, id1: &VarId, id2: &VarId) -> EmptyDomains {
        self.0.write().unwrap().union_var(id1, id2)
    }

    pub fn union_domain(&self, id1: &DomainId, id2: &DomainId) -> EmptyDomains {
        self.0.write().unwrap().union_domain(id1, id2)
    }

    /*
    OTHER
     */

    pub fn var_as_cst(&self, var_id: &VarId) -> Option<Cst> {
        self.0.read().unwrap().var_as_cst(var_id)
    }

    /*
    FORMAT Function
      */

    pub fn format_variable(&self, id: &VarId) -> String {
        self.0.read().unwrap().format_variable(id)
    }

    pub fn format_domain_id(&self, domain: &DomainId) -> String {
        self.0.read().unwrap().format_domain_id(domain)
    }

    pub fn format_domain(&self, domain: &Domain) -> String {
        self.0.read().unwrap().format_domain(domain)
    }

    pub fn format_var_domain(&self, id: &DomainId) -> String {
        self.0.read().unwrap().format_var_domain(id)
    }

    pub fn get_lattice(&self) -> TypeLattice {
        self.0.read().unwrap().get_lattice()
    }

    pub fn export_lattice_dot(&self) -> Dot {
        self.0.read().unwrap().export_lattice_dot()
    }
}

impl Display for RefSymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let st = self.0.read().unwrap();

        writeln!(f, "## SYM TABLE ").unwrap();

        for e in 0..st.domains.len() {
            writeln!(
                f,
                "- ({}){}({})",
                e,
                self.get_label(&e, false),
                self.format_var_domain(&self.get_domain_id(&e)),
            )?;
        }
        Ok(())
    }
}

impl From<SymTable> for RefSymTable {
    fn from(value: SymTable) -> Self {
        RefSymTable(Arc::new(RwLock::new(value)))
    }
}
