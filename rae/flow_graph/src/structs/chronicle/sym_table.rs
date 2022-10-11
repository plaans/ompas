use crate::structs::chronicle::atom::{Atom, Variable};
use crate::structs::chronicle::forest::{Forest, Node};
use crate::structs::chronicle::type_table::{AtomType, TypeId, TypeTable};
use crate::structs::chronicle::{AtomId, FormatWithSymTable};
use ompas_rae_language::*;
use sompas_core::modules::get_scheme_primitives;
use sompas_structs::lnumber::LNumber;
use sompas_structs::lruntimeerror;
use std::collections::{HashMap, HashSet};
use std::fmt::{Display, Formatter};

#[derive(Clone)]
pub struct SymTable {
    symbols: Forest<Atom>,
    ids: im::HashMap<String, AtomId>,
    types: SymbolTypes,
    meta_data: SymTableMetaData,
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

    pub fn get_type_id(&self, symbol: impl ToString) -> Option<&TypeId> {
        self.types.get_type_id(symbol)
    }

    pub fn get_type_string(&self, id: &TypeId) -> Option<&String> {
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
    GETTERS
    */
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<Atom>> {
        self.symbols.get_node(id)
    }

    pub fn get_atom(&self, id: &AtomId, parent: bool) -> Option<&Atom> {
        match parent {
            true => self.symbols.get_value(self.get_parent(id)),
            false => self.symbols.get_value(id),
        }
    }

    pub fn get_type_of(&self, id: &AtomId) -> Option<&AtomType> {
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

    pub fn new_symbol(&mut self, sym: impl Display, a_type: Option<AtomType>) -> AtomId {
        let sym = &sym.to_string();
        if self.it_exists(sym) {
            *self.id(sym).unwrap()
        } else {
            let sym: String = sym.into();
            let id = self.symbols.new_node(Atom::Symbol(sym.clone()));
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

    /*pub fn declare_new_presence(&mut self) -> AtomId {
        let n = self.meta_data.new_presence_index();
        let sym: Sym = format!("p_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(AtomType::Presence),
                kind: AtomKind::Variable(VariableKind::Local),
            },
        );
        id
    }*/

    /*pub fn declare_new_interval(&mut self) -> Interval {
        let n1 = self.meta_data.new_timepoint_index();
        let n2 = self.meta_data.new_timepoint_index();
        let start: Sym = format!("t_{}", n1).into();
        let end: Sym = format!("t_{}", n2).into();
        let id_1 = self.symbols.new_node((&start).into());
        let id_2 = self.symbols.new_node((&end).into());
        self.ids.insert(start, id_1);
        let timepoint_type = AtomType {
            a_type: Some(AtomType::Timepoint),
            kind: AtomKind::Variable(VariableKind::Local),
        };

        self.types.add_new_atom(&id_1, timepoint_type);
        self.ids.insert(end, id_2);
        self.types.add_new_atom(&id_2, timepoint_type);
        Interval::new(&id_1, &id_2)
    }*/

    /*pub fn declare_new_parameter(
        &mut self,
        symbol: impl ToString,
        var_type: Option<AtomType>,
    ) -> AtomId {
        let var_type = AtomType {
            a_type: var_type,
            kind: AtomKind::Variable(VariableKind::Parameter),
        };

        let symbol = symbol.to_string();

        return if if_it_exists_create_new {
            self.unique_to_several(&symbol);
            let vec_similar = self.multiple_def.get_mut(&symbol).unwrap();
            let n = vec_similar.len();
            let pointer_to_ver = self
                .pointer_to_ver
                .last_mut()
                .expect("no hashmap to version of variable");

            if pointer_to_ver.contains_key(&symbol) {
                *pointer_to_ver.get_mut(&symbol).unwrap() = n;
            } else {
                pointer_to_ver.insert(symbol.to_string(), n);
            }
            let sym = Sym::Several(symbol.to_string(), n);

            let id = self.symbols.new_node((&sym).into());
            self.ids.insert(sym, id);
            self.types.add_new_atom(&id, var_type);
            vec_similar.push(id);
            id
        } else {
            //check multiple def
            return match self.pointer_to_ver.last().unwrap().get(&symbol) {
                None => {
                    if self.multiple_def.contains_key(&symbol) {
                        self.multiple_def.get(&symbol).unwrap()[0]
                    } else {
                        *self.ids.get(&symbol.to_string().into()).unwrap()
                    }
                }
                Some(i) => *self.multiple_def.get(&symbol).unwrap().get(*i).unwrap(),
            };
        };
    }*/

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

    pub fn find_parent(&mut self, a: &AtomId) -> &AtomId {
        self.symbols.find(a)
    }

    pub fn get_parent(&self, a: &AtomId) -> &AtomId {
        self.symbols.get_parent(a)
    }

    pub fn flat_bindings(&mut self) {
        self.symbols.flat_bindings()
    }

    pub fn format_forest(&self) -> String {
        self.symbols.to_string()
    }
}

impl Display for SymTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> std::fmt::Result {
        let mut str = "#SYM TABLE: \n\n".to_string();
        let mut constant_typed = vec![];
        let mut constant_untyped = vec![];
        let mut var_typed = vec![];
        let mut var_untyped = vec![];
        for x in self.ids.values() {
            if x == self.get_parent(x) {
                let t = self.get_type_of(x).unwrap();
                let constant = self.symbols.get_node(x).unwrap().get_value().is_constant();
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
                assert_eq!(e, self.get_parent(e));
                str.push_str(
                    format!(
                        "- ({}){}({})\n",
                        e,
                        self.get_atom(e, true).unwrap(),
                        self.get_type_of(e).unwrap().format(self, true)
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

    /*pub fn new_presence_index(&mut self) -> usize {
        let n = self.n_presence;
        self.n_presence += 1;
        n
    }*/
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            symbols: Forest::default(),
            ids: Default::default(),
            types: Default::default(),
            meta_data: Default::default(),
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
    pub fn get_type_of(&self, atom_id: &AtomId) -> Option<&AtomType> {
        self.inner.get(atom_id)
    }

    pub fn add_new_atom(&mut self, id: &AtomId, atom_type: AtomType) {
        self.inner.insert(*id, atom_type);
    }

    pub fn add_type(&mut self, pat: impl ToString, type_id: TypeId) {
        self.type_table.add_type(pat, type_id);
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<&String> {
        self.type_table.get_type(type_id)
    }

    pub fn get_type_id(&self, pat: impl ToString) -> Option<&TypeId> {
        self.type_table.get_type_id(pat)
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
