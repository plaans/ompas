use crate::structs::chronicle::atom::{Atom, Symbol, SyntheticTask, Variable};
use crate::structs::chronicle::forest::{Forest, Node};
use crate::structs::chronicle::interval::Interval;
use crate::structs::chronicle::type_table::{AtomType, TypeId, TypeTable};
use crate::structs::chronicle::{AtomId, FormatWithSymTable};
use itertools::Itertools;
use ompas_rae_language::*;
use sompas_core::modules::get_scheme_primitives;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use std::borrow::Borrow;
use std::cell::RefCell;
use std::collections::{HashMap, HashSet};
use std::fmt::Write;
use std::fmt::{Display, Formatter};
use std::rc::Rc;

#[derive(Default, Clone)]
pub struct RefSymTable(Rc<RefCell<SymTable>>);

impl RefSymTable {
    fn add_basic_types(&mut self) {
        RefCell::borrow_mut(&self.0).add_basic_types();
    }

    pub fn get_type_id(&self, symbol: impl ToString) -> Option<TypeId> {
        RefCell::borrow(&self.0).get_type_id(symbol)
    }

    pub fn get_type_string(&self, id: &TypeId) -> Option<String> {
        RefCell::borrow(&self.0).get_type_string(id)
    }

    pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<&str>,
        sym_type: Option<AtomType>,
    ) -> lruntimeerror::Result<()> {
        RefCell::borrow_mut(&self.0).add_list_of_symbols_of_same_type(list, sym_type)
    }

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
    pub fn get_node(&self, id: &AtomId) -> Option<Node<Atom>> {
        RefCell::borrow(&self.0).get_node(id).cloned()
    }

    pub fn get_atom(&self, id: &AtomId, parent: bool) -> Option<Atom> {
        RefCell::borrow(&self.0).get_atom(id, parent).cloned()
    }

    pub fn get_type_of(&self, id: &AtomId) -> AtomType {
        RefCell::borrow(&self.0).get_type_of(id)
    }

    pub fn id(&self, atom: &str) -> Option<AtomId> {
        RefCell::borrow(&self.0).id(atom).cloned()
    }

    pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }
    /*
    BOOLEAN FUNCTION
     */
    pub fn it_exists(&self, sym: &str) -> bool {
        RefCell::borrow(&self.0).it_exists(sym)
    }

    /*
    DECLARATION FUNCTION
     */
    pub fn new_type(&mut self, sym: impl Display, a_type: Option<TypeId>) -> TypeId {
        RefCell::borrow_mut(&self.0).new_type(sym, a_type)
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_result()
    }

    pub fn new_timepoint(&mut self) -> AtomId {
        RefCell::borrow_mut(&self.0).new_timepoint()
    }

    pub fn new_if(&mut self) -> AtomId {
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

    pub fn new_symbol(&mut self, sym: impl Display, a_type: Option<AtomType>) -> AtomId {
        RefCell::borrow_mut(&self.0).new_symbol(sym, a_type)
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, var_type: AtomType) -> AtomId {
        RefCell::borrow_mut(&self.0).new_parameter(symbol, var_type)
    }

    /*
    SETTERS
     */
    pub fn set_type_of(&mut self, atom_id: &AtomId, atom_type: &AtomType) {
        RefCell::borrow_mut(&self.0).set_type_of(atom_id, atom_type)
    }

    /*
    FOREST FUNCTIONS
     */
    pub fn union_atom(&mut self, a: &AtomId, b: &AtomId) {
        RefCell::borrow_mut(&self.0).union_atom(a, b);
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        RefCell::borrow_mut(&self.0).find_parent(a)
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        RefCell::borrow(&self.0).get_parent(a)
    }

    pub fn flat_bindings(&mut self) {
        RefCell::borrow_mut(&self.0).flat_bindings()
    }

    pub fn format_forest(&self) -> String {
        RefCell::borrow(&self.0).format_forest()
    }

    pub fn format_scopes(&self) -> String {
        let scopes = &RefCell::borrow(&self.0).scopes;

        let mut str = "SCOPES:\n".to_string();

        for (atom, interval) in scopes.iter().sorted_by(|a, b| a.0.cmp(&b.0)) {
            write!(
                str,
                "{}: {}\n",
                atom.format(&self, false),
                interval.format(&self, false)
            );
        }
        str
    }
}

#[derive(Clone)]
struct SymTable {
    symbols: Forest<Atom>,
    ids: im::HashMap<String, AtomId>,
    types: SymbolTypes,
    meta_data: SymTableMetaData,
    scopes: im::HashMap<AtomId, Interval>,
}

impl SymTable {
    fn add_basic_types(&mut self) {
        for bt in &[
            AtomType::Command,
            AtomType::StateFunction,
            AtomType::Method,
            AtomType::Task,
            AtomType::Timepoint,
            AtomType::Int,
            AtomType::Float,
            AtomType::Bool,
            AtomType::Symbol,
            AtomType::Function,
            AtomType::Object,
        ] {
            self.new_type(&bt.to_string(), None);
        }
    }

    /*pub fn str_as_planning_atom_type(&self, sym: &str) -> Option<AtomType> {
        self..try_get_from_str(sym)
    }*/

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

    pub fn get_type_id(&self, symbol: impl ToString) -> Option<TypeId> {
        self.types.get_type_id(symbol)
    }

    pub fn get_type_string(&self, id: &TypeId) -> Option<String> {
        self.types.get_type(id)
    }

    pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<&str>,
        sym_type: Option<AtomType>,
    ) -> lruntimeerror::Result<()> {
        let t = sym_type.unwrap_or(AtomType::Untyped);

        for element in list {
            if self.it_exists(element) {
                return Err(lruntimeerror!(
                    "add_list_of_symbols_of_same_type",
                    format!("{} already exists", element)
                ));
            }

            let id = self.symbols.new_node(element.into());
            self.ids.insert(element.into(), id);
            self.types.add_new_atom(&id, t);
        }
        Ok(())
    }

    /*
    NEW SIMPLE ATOMS FUNCTIONS
     */

    pub fn new_bool(&mut self, b: bool) -> AtomId {
        let id = self.symbols.new_node(b.into());
        self.types.add_new_atom(&id, AtomType::Bool);
        id
    }

    pub fn new_int(&mut self, i: i64) -> AtomId {
        let id = self.symbols.new_node(i.into());
        self.types.add_new_atom(&id, AtomType::Int);
        id
    }

    pub fn new_float(&mut self, f: f64) -> AtomId {
        let id = self.symbols.new_node(f.into());
        self.types.add_new_atom(&id, AtomType::Float);
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
    pub fn new_type(&mut self, sym: impl Display, a_type: Option<TypeId>) -> TypeId {
        let sym = sym.to_string();
        let id = self.symbols.new_node(sym.as_str().into());
        self.ids.insert(sym.as_str().into(), id);
        let atom_type = match a_type {
            None => AtomType::RootType,
            Some(t) => AtomType::SubType(t),
        };
        self.types.add_new_atom(&id, atom_type);
        self.types.add_type(sym, id);
        id
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn new_result(&mut self) -> AtomId {
        let index = self.meta_data.new_result_index();
        let atom = Atom::Variable(Variable::Result(index));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Untyped);
        id
    }

    pub fn new_timepoint(&mut self) -> AtomId {
        let index = self.meta_data.new_timepoint_index();
        let atom = Atom::Variable(Variable::Timepoint(index));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Timepoint);
        id
    }

    pub fn new_if(&mut self) -> AtomId {
        let atom = Atom::Symbol(Symbol::SyntheticTask(SyntheticTask::If(
            self.meta_data.new_if_index(),
        )));

        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Timepoint);
        id
    }

    pub fn new_handle(&mut self) -> AtomId {
        let atom = Atom::Variable(Variable::Handle(self.meta_data.new_handle_index()));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Handle);
        id
    }

    pub fn new_start(&mut self) -> AtomId {
        let atom = Atom::Variable(Variable::Start(self.meta_data.new_start_index()));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Timepoint);
        id
    }

    pub fn new_end(&mut self) -> AtomId {
        let atom = Atom::Variable(Variable::End(self.meta_data.new_end_index()));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Timepoint);
        id
    }

    pub fn new_presence(&mut self) -> AtomId {
        let atom = Atom::Variable(Variable::Presence(self.meta_data.new_presence_index()));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Presence);
        id
    }

    pub fn new_chronicle_result(&mut self) -> AtomId {
        let atom = Atom::Variable(Variable::ChronicleResult(
            self.meta_data.new_chronicle_result_index(),
        ));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, AtomType::Untyped);
        id
    }

    pub fn new_symbol(&mut self, sym: impl Display, a_type: Option<AtomType>) -> AtomId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            *self.id(sym).unwrap()
        } else {
            let sym: String = sym.into();
            let id = self
                .symbols
                .new_node(Atom::Symbol(Symbol::Literal(sym.clone())));
            self.ids.insert(sym, id);
            self.types
                .add_new_atom(&id, a_type.unwrap_or(AtomType::Untyped));
            id
        }
    }

    pub fn new_parameter(&mut self, symbol: impl ToString, var_type: AtomType) -> AtomId {
        let atom = Atom::Variable(Variable::Parameter(symbol.to_string()));
        let sym = atom.to_string();
        let id = self.symbols.new_node(atom);
        self.ids.insert(sym, id);
        self.types.add_new_atom(&id, var_type);
        id
    }

    /*
    GETTERS
    */
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<Atom>> {
        self.symbols.get_node(id)
    }

    pub fn get_atom(&self, id: &AtomId, parent: bool) -> Option<&Atom> {
        match parent {
            true => self.symbols.get_value(&self.get_parent(id)),
            false => self.symbols.get_value(id),
        }
    }

    pub fn get_type_of(&self, id: &AtomId) -> AtomType {
        self.types.get_type_of(id)
    }

    pub fn id(&self, atom: &str) -> Option<&AtomId> {
        self.ids.get(atom)
    }

    pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }
    /*
    BOOLEAN FUNCTION
     */
    pub fn it_exists(&self, sym: &str) -> bool {
        self.ids.keys().any(|k| k == sym)
    }

    /*
    SETTERS
     */
    pub fn set_type_of(&mut self, atom_id: &AtomId, atom_type: &AtomType) {
        *self.types.inner.get_mut(atom_id).unwrap() = *atom_type;
    }

    /*
    FOREST FUNCTIONS
     */
    pub fn union_atom(&mut self, a: &AtomId, b: &AtomId) {
        self.symbols.union_ordered(a, b);
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        *self.symbols.find(a)
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        *self.symbols.get_parent(a)
    }

    pub fn flat_bindings(&mut self) {
        self.symbols.flat_bindings()
    }

    pub fn format_forest(&self) -> String {
        self.symbols.to_string()
    }
}

impl Display for RefSymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "#SYM TABLE: \n\n".to_string();
        let mut constant_typed = vec![];
        let mut constant_untyped = vec![];
        let mut var_typed = vec![];
        let mut var_untyped = vec![];
        let st = RefCell::borrow(&Rc::borrow(&self.0));

        for x in st.ids.values() {
            if x == &st.get_parent(x) {
                let t = self.get_type_of(x);
                let constant = st.symbols.get_node(x).unwrap().get_value().is_constant();
                match (t, constant) {
                    (AtomType::Untyped, true) => constant_untyped.push(x),
                    (AtomType::Untyped, false) => var_untyped.push(x),
                    (_, true) => constant_typed.push(x),
                    (_, false) => var_typed.push(x),
                }
            }
        }

        let mut c = |vec: Vec<&AtomId>, preambule: &str| {
            str.push_str(format!("\n## {}:\n", preambule).as_str());
            for e in vec {
                assert_eq!(*e, self.get_parent(e));
                str.push_str(
                    format!(
                        "- ({}){}({})\n",
                        e,
                        self.get_atom(e, true).unwrap(),
                        self.get_type_of(e).format(&self, true)
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

#[derive(Default, Clone)]
pub struct SymTableMetaData {
    n_timepoint: usize,
    n_result: usize,
    n_presence: usize,
    n_if: usize,
    n_handle: usize,
    n_chronicle_result: usize,
    n_start: usize,
    n_end: usize,
    n_prez: usize,
}

impl SymTableMetaData {
    pub fn new_timepoint_index(&mut self) -> usize {
        let n = self.n_timepoint;
        self.n_timepoint += 1;
        n
    }

    pub fn new_result_index(&mut self) -> usize {
        let n = self.n_result;
        self.n_result += 1;
        n
    }

    pub fn new_presence_index(&mut self) -> usize {
        let n = self.n_presence;
        self.n_presence += 1;
        n
    }

    pub fn new_if_index(&mut self) -> usize {
        let n = self.n_if;
        self.n_if += 1;
        n
    }
    pub fn new_handle_index(&mut self) -> usize {
        let n = self.n_handle;
        self.n_handle += 1;
        n
    }
    pub fn new_start_index(&mut self) -> usize {
        let n = self.n_start;
        self.n_start += 1;
        n
    }
    pub fn new_end_index(&mut self) -> usize {
        let n = self.n_end;
        self.n_end += 1;
        n
    }

    pub fn new_chronicle_result_index(&mut self) -> usize {
        let n = self.n_chronicle_result;
        self.n_chronicle_result += 1;
        n
    }
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            symbols: Forest::default(),
            ids: Default::default(),
            types: Default::default(),
            meta_data: Default::default(),
            scopes: Default::default(),
        };

        st.add_basic_types();

        //Symbols of lisp functions that are useful
        //Not exhaustive
        st.add_list_of_symbols_of_same_type(get_scheme_primitives(), Some(AtomType::Function))
            .expect("error while adding symbols of scheme primitives");

        st.add_list_of_symbols_of_same_type(
            vec![RAE_ASSERT, RAE_RETRACT, RAE_INSTANCE],
            Some(AtomType::Function),
        )
        .expect("error while adding symbols of rae");
        st
    }
}

#[derive(Default, Clone)]
struct SymbolTypes {
    inner: HashMap<AtomId, AtomType>,
    type_table: TypeTable,
}

impl SymbolTypes {
    pub fn get_type_of(&self, atom_id: &AtomId) -> AtomType {
        *self.inner.get(atom_id).unwrap()
    }

    pub fn add_new_atom(&mut self, id: &AtomId, atom_type: AtomType) {
        self.inner.insert(*id, atom_type);
    }

    pub fn add_type(&mut self, pat: impl ToString, type_id: TypeId) {
        self.type_table.add_type(pat, type_id);
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<String> {
        self.type_table.get_type(type_id).cloned()
    }

    pub fn get_type_id(&self, pat: impl ToString) -> Option<TypeId> {
        self.type_table.get_type_id(pat).cloned()
    }
}

#[derive(Eq, PartialEq)]
pub enum ExpressionType {
    Pure,
    Lisp,
    Action,
    Task,
    StateFunction(Option<AtomType>),
}
