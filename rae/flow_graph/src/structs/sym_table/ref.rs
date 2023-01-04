use crate::structs::chronicle::interval::Interval;
use crate::structs::domain::basic_type::BasicType;
use crate::structs::domain::Domain;
use crate::structs::sym_table::forest::Node;
use crate::structs::sym_table::{AtomId, SymTable};
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
    pub fn get_node(&self, id: &AtomId) -> Option<Node<Domain>> {
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

    pub fn new_symbol(&mut self, sym: impl Display, domain: Option<Domain>) -> AtomId {
        RefCell::borrow_mut(&self.0).new_symbol(sym, domain)
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, domain: Domain) -> AtomId {
        RefCell::borrow_mut(&self.0).new_parameter(symbol, domain)
    }

    pub fn meet_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).meet_domain(d1, d2)
    }

    pub fn union_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).union_domain(d1, d2)
    }

    pub fn substract_domain(&self, d1: &Domain, d2: &Domain) -> Domain {
        RefCell::borrow(&self.0).substract_domain(d1, d2)
    }

    pub fn contained_in_domain(&self, d1: &Domain, d2: &Domain) -> bool {
        RefCell::borrow(&self.0).contained_in_domain(d1, d2)
    }

    pub fn get_debug(&self, id: &AtomId) -> String {
        RefCell::borrow(&self.0).get_debug(id).to_string()
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        RefCell::borrow(&self.0).get_parent(a)
    }

    pub fn get_type_as_domain(&self, r#type: impl Into<BasicType>) -> Domain {
        RefCell::borrow(&self.0).get_type_as_domain(r#type)
    }

    /*
    TYPES
     */
    /*pub fn set_type_of(&mut self, atom_id: &AtomId, atom_type: &AtomType) {
        RefCell::borrow_mut(&self.0).set_type_of(atom_id, atom_type)
    }
    pub fn union_types(&mut self, a: &AtomId, b: &AtomId) {
        RefCell::borrow_mut(&self.0).union_types(a, b)
    }*/
    /*pub fn get_type_id_of(&self, atom_id: &AtomId) -> TypeId {
        *RefCell::borrow(&self.0).get_type_id_of(atom_id)
    }*/

    /*
    FOREST FUNCTIONS
     */
    /*pub fn union_atom(&mut self, a: &AtomId, b: &AtomId) {
        RefCell::borrow_mut(&self.0).union_atom(a, b);
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        RefCell::borrow_mut(&self.0).find_parent(a)
    }



    pub fn flat_bindings(&mut self) {
        RefCell::borrow_mut(&self.0).flat_bindings()
    }

    pub fn format_symbols_forest(&self) -> String {
        RefCell::borrow(&self.0).format_symbols_forest()
    }
    pub fn format_types_forest(&self) -> String {
        RefCell::borrow(&self.0).format_types_forest()
    }

    pub fn format_types(&self) -> String {
        RefCell::borrow(&self.0).format_types()
    }

    pub fn format_scopes(&self) -> String {
        let scopes = &RefCell::borrow(&self.0).scopes;

        let mut str = "SCOPES:\n".to_string();

        for (atom, interval) in scopes.iter().sorted_by(|a, b| a.0.cmp(b.0)) {
            writeln!(
                str,
                "{}: {}",
                atom.format(self, false),
                interval.format(self, false)
            )
            .unwrap();
        }
        str
    }*/
}

impl Display for RefSymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "#SYM TABLE: \n\n".to_string();
        let mut constant_typed = vec![];
        let mut constant_untyped = vec![];
        let mut var_typed = vec![];
        let mut var_untyped = vec![];
        let st = RefCell::borrow(Rc::borrow(&self.0));

        for x in 0..st.domains.len() {
            if x == st.get_parent(&x) {
                let domain = st.domains.get_value(&x).unwrap();
                let constant = domain.is_constant();
                match (domain, constant) {
                    (Domain::Simple(1), true) => constant_untyped.push(x),
                    (Domain::Simple(1), false) => var_untyped.push(x),
                    (_, true) => constant_typed.push(x),
                    (_, false) => var_typed.push(x),
                }
            }
        }

        let mut c = |vec: Vec<AtomId>, preambule: &str| {
            str.push_str(format!("\n## {}:\n", preambule).as_str());
            for e in vec {
                assert_eq!(e, self.get_parent(&e));
                str.push_str(
                    format!(
                        "- ({}){}({})\n",
                        e,
                        self.get_debug(&e),
                        self.get_domain(&e, true).unwrap(),
                    )
                    .as_str(),
                );
            }
        };

        c(constant_typed, "CONSTANT TYPED");
        c(constant_untyped, "CONSTANT UNTYPED");
        c(var_typed, "VAR TYPED");
        c(var_untyped, "VAR UNTYPED");

        write!(f, "{}", str)
    }
}
