use crate::structs::chronicle::interval::Interval;
use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::Domain;
use crate::structs::sym_table::closure::{ConstraintClosure, UpdateClosure};
use crate::structs::sym_table::forest::Node;
use crate::structs::sym_table::var_domain::VarDomain;
use crate::structs::sym_table::{AtomId, EmptyDomains, SymTable};
use sompas_structs::lnumber::LNumber;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct RefSymTable(Rc<RefCell<SymTable>>);

impl RefSymTable {
    /*fn add_basic_types(&mut self) {
        RefCell::borrow_mut(&self.0).add_basic_types();
    }*/

    /*pub fn get_type_id(&self, symbol: impl ToString) -> Option<TypeId> {
        RefCell::borrow(&self.0).get_type_id(symbol)
    }

    pub fn get_type_string(&self, id: &TypeId) -> Option<String> {
        RefCell::borrow(&self.0).get_type_string(id)
    }*/

    /*pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<&str>,
        sym_type: Option<AtomType>,
    ) -> lruntimeerror::Result<()> {
        RefCell::borrow_mut(&self.0).add_list_of_symbols_of_same_type(list, sym_type)
    }*/

    /*
    SCOPES FUNCTIONS
     */
    pub fn get_scope(&self, id: &AtomId) -> Option<Interval> {
        RefCell::borrow(&self.0).get_scope(id).cloned()
    }

    pub fn get_start(&self, id: &AtomId) -> Option<AtomId> {
        RefCell::borrow(&self.0).get_start(id).cloned()
    }

    pub fn get_end(&self, id: &AtomId) -> Option<AtomId> {
        RefCell::borrow(&self.0).get_end(id).cloned()
    }

    pub fn new_scope(&mut self, id: &AtomId, start: &AtomId) {
        RefCell::borrow_mut(&self.0).new_scope(id, start)
    }

    pub fn set_end(&mut self, id: &AtomId, end: &AtomId) {
        RefCell::borrow_mut(&self.0).set_end(id, end)
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_bool(&mut self, b: bool) -> AtomId {
        RefCell::borrow_mut(&self.0).new_bool(b)
    }

    pub fn new_int(&mut self, i: i64) -> AtomId {
        RefCell::borrow_mut(&self.0).new_int(i)
    }

    pub fn new_float(&mut self, f: f64) -> AtomId {
        RefCell::borrow_mut(&self.0).new_float(f)
    }

    pub fn new_number(&mut self, n: &LNumber) -> AtomId {
        RefCell::borrow_mut(&self.0).new_number(n)
    }

    /*
    GETTERS
    */
    pub fn get_node(&self, id: &AtomId) -> Option<Node<VarDomain>> {
        RefCell::borrow(&self.0).get_node(id).cloned()
    }

    pub fn get_domain(&self, id: &AtomId, parent: bool) -> Option<Domain> {
        RefCell::borrow(&self.0).get_domain(id, parent).cloned()
    }

    /*pub fn get_type_of(&self, id: &AtomId) -> AtomType {
        RefCell::borrow(&self.0).get_type_of(id)
    }*/

    pub fn id(&self, atom: &str) -> Option<AtomId> {
        RefCell::borrow(&self.0).id(atom)
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
    pub fn new_result(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_result()
    }

    pub fn new_timepoint(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_timepoint()
    }

    pub fn new_if(&mut self) -> (AtomId, AtomId, AtomId) {
        RefCell::borrow_mut(&self.0).new_if()
    }

    pub fn new_handle(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_handle()
    }

    pub fn new_start(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_start()
    }

    pub fn new_end(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_end()
    }

    pub fn new_presence(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_presence()
    }

    pub fn new_chronicle_result(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_chronicle_result()
    }

    pub fn new_symbol(&mut self, sym: impl Display) -> AtomId {
        RefCell::borrow_mut(&self.0).new_symbol(sym)
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: Domain) -> AtomId {
        RefCell::borrow_mut(&self.0).new_parameter(symbol, domain)
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        RefCell::borrow(&self.0).get_parent(a)
    }

    /*pub fn get_debug(&self, id: &AtomId) -> String {
        RefCell::borrow(&self.0).get_debug(id).to_string()
    }*/
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

    pub fn meet_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        RefCell::borrow(&self.0).meet_domains(id_d1, id_d2)
    }

    pub fn union_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        RefCell::borrow(&self.0).union_domains(id_d1, id_d2)
    }

    pub fn substract_domains(&self, id_d1: &AtomId, id_d2: &AtomId) -> Domain {
        RefCell::borrow(&self.0).substract_domains(id_d1, id_d2)
    }

    pub fn meet_to_domain(&mut self, id_d1: &AtomId, domain: impl Into<Domain>) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).meet_to_domain(id_d1, domain)
    }

    pub fn substract_to_domain(
        &mut self,
        id_d1: &AtomId,
        domain: impl Into<Domain>,
    ) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).subtract_to_domain(id_d1, domain)
    }

    pub fn set_domain(&mut self, id: &AtomId, domain: impl Into<Domain>) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).set_domain(id, domain)
    }

    pub fn add_constraint(
        &mut self,
        id: &AtomId,
        constraint: ConstraintClosure,
        update: UpdateClosure,
    ) {
        RefCell::borrow_mut(&self.0).add_constraint(id, constraint, update)
    }

    pub fn add_update(&mut self, id: &AtomId, update: UpdateClosure) {
        RefCell::borrow_mut(&self.0).add_update(id, update)
    }

    pub fn add_union_constraint(&mut self, id: &AtomId, union: Vec<AtomId>) {
        RefCell::borrow_mut(&self.0).add_union_constraint(id, union)
    }

    pub fn add_dependent(&mut self, id: &AtomId, parent: AtomId) {
        RefCell::borrow_mut(&self.0).add_dependent(id, parent)
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

    pub fn try_union_atom(&mut self, id1: &AtomId, id2: &AtomId) -> EmptyDomains {
        RefCell::borrow_mut(&self.0).union_atom(id1, id2)
    }

    /*
    FORMAT Function
      */

    pub fn format_variable(&self, id: &AtomId) -> String {
        RefCell::borrow(&self.0).format_variable(id)
    }

    pub fn format_domain(&self, id: &AtomId) -> String {
        RefCell::borrow(&self.0).format_domain(id)
    }
}

impl Display for RefSymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let st = RefCell::borrow(Rc::borrow(&self.0));

        write!(f, "## SYM TABLE \n").unwrap();

        for e in 0..st.domains.len() {
            if e == st.get_parent(&e) {
                assert_eq!(e, self.get_parent(&e));
                write!(
                    f,
                    "- ({}){}({})\n",
                    e,
                    self.format_variable(&e),
                    self.format_domain(&e),
                )?;
            }
        }
        Ok(())
    }
}
