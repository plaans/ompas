use crate::module::rae_exec::*;
use crate::planning::structs::atom::{Atom, Sym};
use crate::planning::structs::interval::Interval;
use crate::planning::structs::traits::FormatWithSymTable;
use crate::planning::structs::type_table::{
    AtomKind, AtomType, PlanningAtomType, TypeId, TypeTable, VariableKind,
};
use crate::planning::union_find::{Forest, Node, NodeId};
use ompas_lisp::core::root_module::language::get_scheme_primitives;
use ompas_lisp::core::structs::lerror;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use std::collections::{HashMap, HashSet};

pub type AtomId = NodeId;

#[derive(Clone)]
pub struct SymTable {
    symbols: Forest<Atom>,
    ids: im::HashMap<Sym, AtomId>,
    types: TypeTable,
    symbol_types: SymbolTypes,
    meta_data: SymTableMetaData,
    multiple_def: HashMap<String, Vec<AtomId>>,
    pointer_to_ver: Vec<HashMap<String, usize>>,
}

#[derive(Default, Clone)]
pub struct SymTableMetaData {
    n_timepoint: usize,
    n_result: usize,
    n_presence: usize,
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
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            symbols: Forest::default(),
            ids: Default::default(),
            types: Default::default(),
            symbol_types: Default::default(),
            meta_data: Default::default(),
            multiple_def: Default::default(),
            pointer_to_ver: vec![Default::default()],
        };

        st.add_basic_types();

        //Symbols of lisp functions that are useful
        //Not exhaustive
        st.add_list_of_symbols_of_same_type(
            get_scheme_primitives(),
            Some(PlanningAtomType::Function),
        )
        .expect("error while adding symbols of scheme primitives");

        st.add_list_of_symbols_of_same_type(
            vec![RAE_ASSERT, RAE_RETRACT, RAE_INSTANCE],
            Some(PlanningAtomType::Function),
        )
        .expect("error while adding symbols of rae");
        st
    }
}

impl SymTable {
    fn add_basic_types(&mut self) {
        for bt in vec![
            PlanningAtomType::Task,
            PlanningAtomType::Bool,
            PlanningAtomType::Method,
            PlanningAtomType::Action,
            PlanningAtomType::StateFunction,
            PlanningAtomType::Int,
            PlanningAtomType::Float,
            PlanningAtomType::Timepoint,
            PlanningAtomType::Function,
            PlanningAtomType::Symbol,
            PlanningAtomType::Lambda,
        ] {
            let id = self.declare_new_type(&bt.to_string(), None);
            self.types.add_type(&bt, id);
        }
    }

    pub fn get_type_id(&self, sym_type: impl ToString) -> TypeId {
        self.types.get_type_id(sym_type)
    }

    pub fn get_type(&self, type_id: &TypeId) -> Option<&String> {
        self.types.get_type(type_id)
    }

    pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<&str>,
        sym_type: Option<PlanningAtomType>,
    ) -> lerror::Result<()> {
        for element in list {
            if self.it_exists(element) {
                return Err(SpecialError(
                    "add_list_of_symbols_of_same_type",
                    format!("{} already exists", element),
                ));
            }

            let id = self.symbols.new_node(element.into());
            self.ids.insert(element.into(), id);
            self.symbol_types.add_new_atom(
                &id,
                AtomType {
                    a_type: sym_type,
                    kind: AtomKind::Constant,
                },
            );
        }
        Ok(())
    }
}

/*
NEW SIMPLE ATOMS FUNCTIONS
 */
impl SymTable {
    pub fn new_bool(&mut self, b: bool) -> AtomId {
        let id = self.symbols.new_node(b.into());
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(PlanningAtomType::Bool),
                kind: AtomKind::Constant,
            },
        );
        id
    }

    pub fn new_int(&mut self, i: i64) -> AtomId {
        let id = self.symbols.new_node(i.into());
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(PlanningAtomType::Int),
                kind: AtomKind::Constant,
            },
        );
        id
    }

    pub fn new_float(&mut self, f: f64) -> AtomId {
        let id = self.symbols.new_node(f.into());
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(PlanningAtomType::Float),
                kind: AtomKind::Constant,
            },
        );
        id
    }
}

/*
GETTERS
 */
impl SymTable {
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<Atom>> {
        self.symbols.get_node(id)
    }

    pub fn get_atom(&self, id: &AtomId) -> Option<&Atom> {
        self.symbols.get_value(id)
    }

    pub fn get_type_of(&self, id: &AtomId) -> Option<&AtomType> {
        self.symbol_types.get_type(id)
    }

    pub fn id(&self, atom: &str) -> Option<&AtomId> {
        //Look before in the multiple_def table, and then looking in self.ids
        if self.multiple_def.contains_key(atom) {
            let ver = match self.pointer_to_ver.last().unwrap().get(atom) {
                Some(ver) => *ver,
                None => 0,
            };
            let value = self.multiple_def.get(atom).unwrap().get(ver);
            value
        } else {
            self.ids.get(&atom.into())
        }
    }

    pub fn get_symbols_of_type(&self, _symbol_type: &AtomType) -> HashSet<AtomId> {
        todo!()
    }

    pub fn get_sym(&self, id: &AtomId) -> &Atom {
        self.get_atom(&self.get_parent(id)).unwrap()
    }
}
/*
BOOLEAN FUNCTION
 */
impl SymTable {
    pub fn it_exists(&self, sym: &str) -> bool {
        self.ids.keys().any(|k| k.get_string() == sym)
    }
}

/*
SCOPE FUNCTIONS
 */
impl SymTable {
    pub fn new_scope(&mut self) {
        self.pointer_to_ver
            .push(self.pointer_to_ver.last().unwrap().clone())
    }

    pub fn revert_scope(&mut self) {
        self.pointer_to_ver.remove(self.pointer_to_ver.len() - 1);
    }
}

/*
DECLARATION FUNCTION
 */
impl SymTable {
    pub fn declare_new_type(&mut self, sym: &str, a_type: Option<TypeId>) -> TypeId {
        let id = self.symbols.new_node(sym.into());
        let atom_type = AtomType {
            a_type: match a_type {
                Some(t) => Some(PlanningAtomType::SubType(t)),
                None => None,
            },
            kind: AtomKind::Constant,
        };
        self.symbol_types.add_new_atom(&id, atom_type);
        self.types.add_type(sym, id);
        id
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn declare_new_result(&mut self, a_type: Option<PlanningAtomType>) -> AtomId {
        let n = self.meta_data.new_result_index();
        let sym: Sym = format!("r_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type,
                kind: AtomKind::Variable(VariableKind::Local),
            },
        );
        id
    }

    pub fn declare_new_presence(&mut self) -> AtomId {
        let n = self.meta_data.new_presence_index();
        let sym: Sym = format!("p_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(PlanningAtomType::Bool),
                kind: AtomKind::Variable(VariableKind::Local),
            },
        );
        id
    }

    pub fn declare_new_interval(&mut self) -> Interval {
        let n1 = self.meta_data.new_timepoint_index();
        let n2 = self.meta_data.new_timepoint_index();
        let start: Sym = format!("t_{}", n1).into();
        let end: Sym = format!("t_{}", n2).into();
        let id_1 = self.symbols.new_node((&start).into());
        let id_2 = self.symbols.new_node((&end).into());
        self.ids.insert(start, id_1);
        let timepoint_type = AtomType {
            a_type: Some(PlanningAtomType::Timepoint),
            kind: AtomKind::Variable(VariableKind::Local),
        };

        self.symbol_types
            .add_new_atom(&id_1, timepoint_type.clone());
        self.ids.insert(end, id_2);
        self.symbol_types.add_new_atom(&id_2, timepoint_type);
        Interval::new(&id_1, &id_2)
    }

    pub fn declare_new_timepoint(&mut self) -> AtomId {
        let n = self.meta_data.new_timepoint_index();
        let sym: Sym = format!("t_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(
            &id,
            AtomType {
                a_type: Some(PlanningAtomType::Timepoint),
                kind: AtomKind::Variable(VariableKind::Local),
            },
        );
        id
    }

    pub fn declare_new_symbol(
        &mut self,
        sym: &str,
        symbol_type: Option<PlanningAtomType>,
    ) -> AtomId {
        if self.it_exists(sym) {
            //check multiple def
            match self.pointer_to_ver.last().unwrap().get(sym) {
                None => {
                    if self.multiple_def.contains_key(sym) {
                        self.multiple_def.get(sym).unwrap()[0]
                    } else {
                        *self.ids.get(&sym.to_string().into()).unwrap()
                    }
                }
                Some(i) => *self.multiple_def.get(sym).unwrap().get(*i).unwrap(),
            }
        } else {
            let sym: Sym = sym.to_string().into();
            let id = self.symbols.new_node((&sym).into());
            self.ids.insert(sym, id);
            self.symbol_types.add_new_atom(
                &id,
                AtomType {
                    a_type: symbol_type,
                    kind: AtomKind::Constant,
                },
            );
            id
        }
    }

    pub fn declare_new_parameter(
        &mut self,
        symbol: impl ToString,
        if_it_exists_create_new: bool,
        var_type: Option<PlanningAtomType>,
    ) -> AtomId {
        let var_type = AtomType {
            a_type: var_type,
            kind: AtomKind::Variable(VariableKind::Parameter),
        };

        let symbol = symbol.to_string();

        if self.it_exists(&symbol) {
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
                let id = self
                    .symbols
                    .new_node(Sym::Several(symbol.to_string(), n).into());
                self.symbol_types.add_new_atom(&id, var_type);
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
        } else {
            let sym: Sym = symbol.to_string().into();
            let id = self.symbols.new_node((&sym).into());
            self.ids.insert(sym, id);
            self.symbol_types.add_new_atom(&id, var_type);
            id
        }
    }

    fn unique_to_several(&mut self, sym: &str) {
        if !self.multiple_def.contains_key(sym) {
            //println!("transforming {} into several ", sym);
            //change value in vec of symbol
            let id = self.ids.remove(&Sym::Unique(sym.to_string())).unwrap();
            let value = Sym::Several(sym.to_string(), 0);
            self.symbols.set_value(&id, (&value).into());
            //Update key in hashmap
            self.ids.insert(value, id);
            //Create new entry in multiple_def
            self.multiple_def.insert(sym.to_string(), vec![id]);
            self.pointer_to_ver
                .last_mut()
                .unwrap()
                .insert(sym.to_string(), 0);
        }
    }
}

/*
SETTERS
 */

impl SymTable {
    pub fn set_type_of(&mut self, atom_id: &AtomId, atom_type: &Option<PlanningAtomType>) {
        self.symbol_types.inner.get_mut(atom_id).unwrap().a_type = *atom_type;
    }
}

/*
FOREST FUNCTIONS
 */
impl SymTable {
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

#[derive(Default, Clone)]
struct SymbolTypes {
    inner: HashMap<AtomId, AtomType>,
    //types_number: TypesNumber,
    //kind_number: KindNumber,
}

impl SymbolTypes {
    pub fn get_type(&self, atom_id: &AtomId) -> Option<&AtomType> {
        self.inner.get(atom_id)
    }
}
/*
impl SymbolTypes{
    pub fn get_number_of_type(&self, atom_type: &TypeId) -> usize {

        self.types_number.get_number_of_type(atom_type)
    }

    pub fn get_number_of_kind(&self, kind: &AtomKind) -> usize {
        self.kind_number.get_number_of_kind(kind)
    }
}*/

impl SymbolTypes {
    pub fn add_new_atom(&mut self, id: &AtomId, atom_type: AtomType) {
        self.inner.insert(*id, atom_type);
    }
}

impl FormatWithSymTable for AtomId {
    fn format_with_sym_table(&self, st: &SymTable) -> String {
        st.get_sym(self).to_string()
    }
}

#[derive(PartialEq)]
pub enum ExpressionType {
    Pure,
    Lisp,
    Action,
    Task,
    StateFunction,
}

/*


#[derive(Clone, Default)]
struct TypesNumber {
    inner: HashMap<TypeId, usize>,
}

impl TypesNumber {
    pub fn increase_number_of_type(&mut self, atom_type: &TypeId) -> usize {
        let n = self.inner.get_mut(atom_type).unwrap();
        let previous = *n;
        *n += 1;
        previous
    }

    pub fn get_number_of_type(&self, atom_type: &TypeId) -> usize {
        *self.inner.get(atom_type).unwrap()
    }
}

#[derive(Clone)]
struct KindNumber {
    inner: im::HashMap<AtomKind, usize>,
}

impl KindNumber {
    pub fn increase_number_of_kind(&mut self, kind: &AtomKind) -> usize {
        let n = self.inner.get_mut(kind).unwrap();
        let previous = *n;
        *n += 1;
        previous
    }

    pub fn get_number_of_kind(&self, kind: &AtomKind) -> usize {
        *self.inner.get(kind).unwrap()
    }
}

impl Default for KindNumber {
    fn default() -> Self {
        Self {
            inner: hashmap! {
                AtomKind::Variable => 0,
                AtomKind::Constant => 0,
            },
        }
    }
}*/
