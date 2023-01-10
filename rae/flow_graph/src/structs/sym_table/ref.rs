use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::Update;
use crate::structs::sym_table::forest::Node;
use crate::structs::sym_table::var_domain::VarDomain;
use crate::structs::sym_table::variable::Variable;
use crate::structs::sym_table::{DomainId, EmptyDomains, SymTable, VarId};
use sompas_structs::lnumber::LNumber;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct RefSymTable(Rc<RefCell<SymTable>>);

impl RefSymTable {
    /*
    SCOPES FUNCTIONS
     */

    pub fn set_declaration(&mut self, id: &VarId, timepoint: &VarId) {
        RefCell::borrow_mut(&self.0).set_declaration(id, timepoint)
    }

    pub fn set_drop(&mut self, id: &VarId, timepoint: &VarId) {
        RefCell::borrow_mut(&self.0).set_drop(id, timepoint)
    }

    pub fn get_declaration(&self, id: &VarId) -> Option<VarId> {
        RefCell::borrow(&self.0).get_declaration(id)
    }

    pub fn get_drop(&self, id: &VarId) -> Option<VarId> {
        RefCell::borrow(&self.0).get_drop(id)
    }

    pub fn get_domain_vars(&self, d: &DomainId) -> Vec<VarId> {
        RefCell::borrow(&self.0).get_domain_vars(d)
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_bool(&mut self, b: bool) -> VarId {
        RefCell::borrow_mut(&self.0).new_bool(b)
    }

    pub fn new_nil(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_nil()
    }

    pub fn new_int(&mut self, i: i64) -> VarId {
        RefCell::borrow_mut(&self.0).new_int(i)
    }

    pub fn new_float(&mut self, f: f64) -> VarId {
        RefCell::borrow_mut(&self.0).new_float(f)
    }

    pub fn new_number(&mut self, n: &LNumber) -> VarId {
        RefCell::borrow_mut(&self.0).new_number(n)
    }

    /*
    GETTERS
    */

    //VarDomain
    pub fn get_var_domain(&self, id: &DomainId) -> VarDomain {
        RefCell::borrow(&self.0).get_var_domain(id).clone()
    }

    pub fn get_domain(&self, id: &DomainId) -> Domain {
        RefCell::borrow(&self.0).get_domain(id).clone()
    }

    pub fn get_domain_parent(&self, id: &DomainId) -> DomainId {
        RefCell::borrow(&self.0).get_domain_parent(id)
    }

    //Variable
    pub fn get_variable(&self, id: &VarId) -> Variable {
        RefCell::borrow(&self.0).get_variable(id).clone()
    }

    pub fn get_domain_id(&self, v: &VarId) -> DomainId {
        RefCell::borrow(&self.0).get_domain_id(v)
    }

    pub fn get_label(&self, id: &VarId, parent: bool) -> String {
        RefCell::borrow(&self.0).get_label(id, parent).to_string()
    }

    pub fn get_var_parent(&self, v: &VarId) -> VarId {
        RefCell::borrow(&self.0).get_var_parent(v)
    }

    pub fn get_domain_of_var(&self, v: &VarId) -> Domain {
        RefCell::borrow(&self.0).get_domain_of_var(v).clone()
    }

    pub fn get_sym_id(&self, sym: &str) -> Option<VarId> {
        RefCell::borrow(&self.0).get_sym_id(sym)
    }

    /*pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }*/
    /*
    BOOLEAN FUNCTION
     */
    pub fn it_exists(&self, sym: &str) -> bool {
        RefCell::borrow(&self.0).it_exists(sym)
    }

    /*
    DECLARATION FUNCTION
     */
    /*pub fn new_type(&mut self, sym: impl Display, a_type: Option<TypeId>) -> TypeId {
        RefCell::borrow_mut(&self.0).new_type(sym, a_type)
    }*/

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_result()
    }

    pub fn new_timepoint(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_timepoint()
    }

    pub fn new_if(&mut self) -> (VarId, VarId, VarId) {
        RefCell::borrow_mut(&self.0).new_if()
    }

    pub fn new_handle(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_handle()
    }

    pub fn new_start(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_start()
    }

    pub fn new_end(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_end()
    }

    pub fn new_presence(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_presence()
    }

    pub fn new_chronicle_result(&mut self) -> VarId {
        RefCell::borrow_mut(&self.0).new_chronicle_result()
    }

    pub fn new_symbol(&mut self, sym: impl Display) -> VarId {
        RefCell::borrow_mut(&self.0).new_symbol(sym)
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: impl Into<Domain>) -> VarId {
        RefCell::borrow_mut(&self.0).new_parameter(symbol, domain)
    }

    /*
    Domain operators
     */

    pub fn meet(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).meet(d1, d2)
    }

    pub fn union(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).union(d1, d2)
    }

    pub fn substract(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).substract(d1, d2)
    }

    pub fn meet_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        RefCell::borrow(&self.0).meet_domains(id_d1, id_d2)
    }

    pub fn union_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        RefCell::borrow(&self.0).union_domains(id_d1, id_d2)
    }

    pub fn substract_domains(&self, id_d1: &DomainId, id_d2: &DomainId) -> Domain {
        RefCell::borrow(&self.0).substract_domains(id_d1, id_d2)
    }

    pub fn meet_to_domain(&mut self, id_d1: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).meet_to_domain(id_d1, domain)
    }

    pub fn substract_to_domain(
        &mut self,
        id_d1: &DomainId,
        domain: impl Into<Domain>,
    ) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).subtract_to_domain(id_d1, domain)
    }

    pub fn set_domain(&mut self, id: &DomainId, domain: impl Into<Domain>) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).set_domain(id, domain)
    }

    pub fn add_update(&mut self, elements: Vec<VarId>, update: Update) {
        RefCell::borrow_mut(&self.0).add_update(elements, update);
    }

    pub fn remove_update(&mut self, id: &VarId, dependent: &VarId) {
        RefCell::borrow_mut(&self.0).remove_update(id, dependent);
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        RefCell::borrow(&self.0).contained_in_domain(d1, d2)
    }

    pub fn get_type_as_domain(&self, r#type: impl Into<BasicType>) -> Domain {
        RefCell::borrow(&self.0).get_type_as_domain(r#type)
    }

    /*
    FOREST FUNCTIONS
     */
    pub fn flat_bindings(&mut self) {
        RefCell::borrow_mut(&self.0).flat_bindings()
    }

    pub fn union_var(&mut self, id1: &VarId, id2: &VarId) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).union_var(id1, id2)
    }

    pub fn union_domain(&mut self, id1: &DomainId, id2: &DomainId) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).union_domain(id1, id2)
    }
    /*
    FORMAT Function
      */

    pub fn format_variable(&self, id: &VarId) -> String {
        RefCell::borrow(&self.0).format_variable(id)
    }

    pub fn format_domain(&self, domain: &Domain) -> String {
        RefCell::borrow(&self.0).format_domain(domain)
    }

    pub fn format_var_domain(&self, id: &DomainId) -> String {
        RefCell::borrow(&self.0).format_var_domain(id)
    }
}

impl Display for RefSymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let st = RefCell::borrow(Rc::borrow(&self.0));

        write!(f, "## SYM TABLE \n").unwrap();

        for e in 0..st.domains.len() {
            write!(
                f,
                "- ({}){}({})\n",
                e,
                self.format_variable(&e),
                self.format_var_domain(&self.get_domain_id(&e)),
            )?;
        }
        Ok(())
    }
}
