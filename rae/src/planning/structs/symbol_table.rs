use crate::module::rae_exec::*;
use crate::planning::structs::atom::{Atom, AtomType, Sym};
use crate::planning::structs::chronicle::ExpressionChronicleResult;
use crate::planning::structs::interval::Interval;
use crate::planning::structs::traits::FormatWithSymTable;
use crate::planning::union_find::{Forest, Node, NodeId};
use ompas_lisp::core::root_module::language::get_scheme_primitives;
use ompas_lisp::core::structs::lerror;
use ompas_lisp::core::structs::lerror::LError::SpecialError;
use ompas_lisp::core::structs::lnumber::LNumber;
use std::collections::HashMap;

pub type AtomId = NodeId;

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

#[derive(Default, Clone)]
pub struct SymbolTypes {
    inner: HashMap<AtomId, AtomType>,
    types_number: TypesNumber,
}

impl SymbolTypes {
    pub fn get_type(&self, atom_id: &AtomId) -> Option<&AtomType> {
        self.inner.get(atom_id)
    }

    pub fn get_number_of_type(&self, atom_type: &AtomType) -> usize {
        self.types_number.get_number_of_type(atom_type)
    }

    pub fn add_new_atom(&mut self, id: AtomId, atom_type: AtomType) {
        self.inner.insert(id, atom_type);
        self.types_number.increase_number_of_type(&atom_type);
    }
}

#[derive(Clone)]
pub struct TypesNumber {
    inner: HashMap<AtomType, usize>,
}

impl TypesNumber {
    pub fn increase_number_of_type(&mut self, atom_type: &AtomType) -> usize {
        let n = self.inner.get_mut(atom_type).unwrap();
        let previous = *n;
        *n += 1;
        previous
    }

    pub fn get_number_of_type(&self, atom_type: &AtomType) -> usize {
        *self.inner.get(atom_type).unwrap()
    }
}

impl Default for TypesNumber {
    fn default() -> Self {
        let mut types_number = HashMap::new();
        types_number.insert(AtomType::Number, 0);
        types_number.insert(AtomType::Boolean, 0);
        types_number.insert(AtomType::Symbol, 0);
        types_number.insert(AtomType::Result, 0);
        types_number.insert(AtomType::Timepoint, 0);
        types_number.insert(AtomType::Object, 0);
        types_number.insert(AtomType::Action, 0);
        types_number.insert(AtomType::StateFunction, 0);
        types_number.insert(AtomType::Method, 0);
        types_number.insert(AtomType::Task, 0);
        types_number.insert(AtomType::Function, 0);
        types_number.insert(AtomType::Lambda, 1);
        Self {
            inner: types_number,
        }
    }
}

#[derive(Clone)]
pub struct SymTable {
    symbols: Forest<Atom>,
    ids: HashMap<Sym, AtomId>,
    symbol_types: SymbolTypes,
    multiple_def: HashMap<String, Vec<AtomId>>,
    pointer_to_ver: Vec<HashMap<String, usize>>,
}

impl SymTable {
    pub fn flat_bindings(&mut self) {
        self.symbols.flat_bindings()
    }

    pub fn format_forest(&self) -> String {
        self.symbols.to_string()
    }
}

impl SymTable {
    pub fn get_sym(&self, id: &AtomId) -> &Atom {
        self.get_atom(&self.get_parent(id)).unwrap()
    }
}

//Forest methods
impl SymTable {
    pub fn union_atom(&mut self, a: &AtomId, b: &AtomId) {
        self.symbols.union(a, b);
    }

    pub fn find_parent(&mut self, a: &AtomId) -> AtomId {
        self.symbols.find(a)
    }

    pub fn get_parent(&self, a: &AtomId) -> AtomId {
        self.symbols.get_parent(a)
    }
}

impl Default for SymTable {
    fn default() -> Self {
        let mut st = Self {
            symbols: Forest::default(),
            ids: Default::default(),
            symbol_types: Default::default(),
            multiple_def: Default::default(),
            pointer_to_ver: vec![Default::default()],
        };

        //Symbols of lisp functions that are useful
        //Not exhaustive
        st.add_list_of_symbols_of_same_type(
            get_scheme_primitives()
                .iter()
                .map(|s| s.to_string())
                .collect(),
            &AtomType::Function,
        )
        .expect("error while adding symbols of scheme primitives");
        st.add_list_of_symbols_of_same_type(
            vec![
                RAE_ASSERT.to_string(),
                RAE_RETRACT.to_string(),
                RAE_INSTANCE.to_string(),
            ],
            &AtomType::Function,
        )
        .expect("error while adding symbols of rae");
        st
    }
}

impl SymTable {
    pub fn add_list_of_symbols_of_same_type(
        &mut self,
        list: Vec<String>,
        sym_type: &AtomType,
    ) -> lerror::Result<()> {
        for element in &list {
            if self.it_exists(element) {
                return Err(SpecialError(
                    "add_list_of_symbols_of_same_type",
                    format!("{} already exists", element),
                ));
            }

            let id = self.symbols.new_node(element.as_str().into());
            self.ids.insert(element.as_str().into(), id);
            self.symbol_types.add_new_atom(id, *sym_type);
        }
        Ok(())
    }
}

impl SymTable {
    pub fn get_node(&self, id: &AtomId) -> Option<&Node<Atom>> {
        self.symbols.get_node(id)
    }

    pub fn get_atom(&self, id: &AtomId) -> Option<&Atom> {
        self.symbols.get_value(id)
    }

    pub fn get_type(&self, id: &AtomId) -> Option<&AtomType> {
        self.symbol_types.get_type(id)
    }

    pub fn id(&self, sym: &str) -> Option<&AtomId> {
        //Look before in the multiple_def table, and then looking in self.ids
        if self.multiple_def.contains_key(sym) {
            let ver = self.pointer_to_ver.last().unwrap().get(sym).unwrap();
            let value = self.multiple_def.get(sym).unwrap().get(*ver);
            value
        } else {
            self.ids.get(&sym.into())
        }
    }

    pub fn new_bool(&mut self, b: bool) -> AtomId {
        let id = self.symbols.new_node(b.into());
        self.symbol_types.add_new_atom(id, AtomType::Boolean);
        id
    }

    pub fn new_number(&mut self, n: LNumber) -> AtomId {
        let id = self.symbols.new_node(n.into());
        self.symbol_types.add_new_atom(id, AtomType::Number);
        id
    }

    //Declare a new return value
    //The name of the return value will be format!("r_{}", last_return_index)
    pub fn declare_new_result(&mut self) -> ExpressionChronicleResult {
        let n = self.symbol_types.get_number_of_type(&AtomType::Result);
        let sym: Sym = format!("r_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(id, AtomType::Result);
        ExpressionChronicleResult::new(id, None)
    }

    pub fn unique_to_several(&mut self, sym: &str) {
        if !self.multiple_def.contains_key(sym) {
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

    pub fn declare_new_interval(&mut self) -> Interval {
        let n = self.symbol_types.get_number_of_type(&AtomType::Timepoint);
        let start: Sym = format!("t_{}", n).into();
        let end: Sym = format!("t_{}", n + 1).into();
        let id_1 = self.symbols.new_node((&start).into());
        let id_2 = self.symbols.new_node((&end).into());
        self.ids.insert(start, id_1);
        self.symbol_types.add_new_atom(id_1, AtomType::Timepoint);
        self.ids.insert(end, id_2);
        self.symbol_types.add_new_atom(id_2, AtomType::Timepoint);
        Interval::new(&id_1, &id_2)
    }

    pub fn new_scope(&mut self) {
        self.pointer_to_ver
            .push(self.pointer_to_ver.last().unwrap().clone())
    }

    pub fn revert_scope(&mut self) {
        self.pointer_to_ver.remove(self.pointer_to_ver.len() - 1);
    }

    pub fn declare_new_timepoint(&mut self) -> AtomId {
        let n = self.symbol_types.get_number_of_type(&AtomType::Timepoint);
        let sym: Sym = format!("t_{}", n).into();
        let id = self.symbols.new_node((&sym).into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(id, AtomType::Timepoint);
        id
    }

    pub fn it_exists(&self, sym: &str) -> bool {
        self.ids.keys().any(|k| k.get_string() == sym)
    }

    pub fn declare_new_symbol(&mut self, symbol: String, if_it_exists_create_new: bool) -> AtomId {
        if self.it_exists(&symbol) {
            return if if_it_exists_create_new {
                self.unique_to_several(&symbol);
                let vec_similar = self.multiple_def.get_mut(&symbol).unwrap();
                let n = vec_similar.len();
                *self
                    .pointer_to_ver
                    .last_mut()
                    .unwrap()
                    .get_mut(&symbol)
                    .unwrap() = n;
                let id = self.symbols.new_node(Sym::Several(symbol, n).into());
                self.symbol_types.add_new_atom(id, AtomType::Symbol);
                vec_similar.push(id);
                id
            } else {
                return *match self.pointer_to_ver.last().unwrap().get(&symbol) {
                    None => self.ids.get(&symbol.into()).unwrap(),
                    Some(i) => self.multiple_def.get(&symbol).unwrap().get(*i).unwrap(),
                };
            };
        } else {
            let sym: Sym = symbol.into();
            let id = self.symbols.new_node((&sym).into());
            self.ids.insert(sym, id);
            self.symbol_types.add_new_atom(id, AtomType::Symbol);
            id
        }
    }

    pub fn declare_new_object(&mut self) -> AtomId {
        let n = self.symbol_types.get_number_of_type(&AtomType::Object);
        let sym: Sym = format!("o_{}", n).into();
        let id = self.symbols.new_node(sym.clone().into());
        self.ids.insert(sym, id);
        self.symbol_types.add_new_atom(id, AtomType::Object);
        id
    }
}
