use crate::deprecated::deprecated_fact_base::language::*;
use crate::deprecated::deprecated_fact_base::{
    FactBaseError, FactBaseOk, FactBaseResult, FactType, EMPTY,
};
use aries_model::symbols::SymId;
use aries_planning::parsing::sexpr::SAtom;
use aries_utils::input::Sym;
use std::collections::HashMap;
use std::fmt::{Display, Formatter};

pub const TYPE_INT_ID: usize = 0;
pub const TYPE_BOOL_ID: usize = 1;
pub const TYPE_OBJECT_ID: usize = 2;

pub struct CustomSymbolTable {
    /// Set of state functions.
    /// A state function is a symbol that match a state function.
    /// A StateFun has a name (Sym) and a set of types, the last type is the return value
    pub state_funs: HashMap<SymId, CustomStateFun>,
    /// Set of types.
    pub types: HashMap<SymId, Sym>,
    ///objects of the problem with a type
    pub objects: HashMap<SymId, Sym>,
    ///Set of symbols that have been defined. We can access them with the SymId.
    pub symbols: Vec<Sym>,
    /// Match a symbol to an idea.
    ids: HashMap<Sym, SymId>,
    /// Match a symbol to a type.
    /// A type has his parent has type.
    pub symbol_types: HashMap<SymId, SymId>,
    /// Each symbol is associated to a FactType.
    pub symbol_fact_type: HashMap<SymId, FactType>,
}

#[derive(Clone)]
pub struct CustomStateFun {
    // Symbol of this state function
    pub sym: SymId,
    /// type of the function. A vec [a, b, c] corresponds
    /// to the type `a -> b -> c` in curried notation.
    /// Hence a and b are the arguments and the last element is the return type
    pub tpe: Vec<SymId>,
}

impl Default for CustomSymbolTable {
    /// Define a SymbolTable with basic types:
    /// - Int
    /// - Boolean
    /// - Object
    fn default() -> Self {
        //define the basic types

        let mut st = CustomSymbolTable {
            state_funs: Default::default(),
            types: Default::default(),
            symbols: vec![],
            objects: Default::default(),
            ids: Default::default(),
            symbol_types: Default::default(),
            symbol_fact_type: Default::default(),
        };
        let int_id = st.add_symbol(SAtom::new(INT), None, FactType::Type);
        let bool_id = st.add_symbol(SAtom::new(BOOLEAN), None, FactType::Type);
        let object_id = st.add_symbol(SAtom::new(OBJECT), None, FactType::Type);
        if int_id != SymId::from(TYPE_INT_ID)
            || bool_id != SymId::from(TYPE_BOOL_ID)
            || object_id != SymId::from(TYPE_OBJECT_ID)
        {
            panic!("Error in CustomSymbolTable")
        }

        st
    }
}

impl Display for CustomSymbolTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n##Symbol Table##\n");
        r.push_str("\n#TYPES: ");
        if !self.types.is_empty() {
            r.push('\n');
            for ty in &self.types {
                r.push_str(format!("-{}", ty.1).as_str());
                match self.get_type_id(ty.0) {
                    Some(ty_id) => {
                        r.push_str(format!("({})\n", self.get_type(&ty_id).to_string()).as_str())
                    }
                    _ => r.push('\n'),
                };
            }
        } else {
            r.push_str(EMPTY);
        }

        r.push_str("\n#OBJECTS: ");
        if !self.objects.is_empty() {
            r.push('\n');
            for obj in &self.objects {
                let type_id = self.get_type_id(obj.0).unwrap();
                let sym_type = self.get_sym(&type_id).unwrap();
                r.push_str(format!("-{}({})\n", obj.1, sym_type).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#STATE-FUNCTION: ");
        if !self.state_funs.is_empty() {
            r.push('\n');
            for k in self.state_funs.values() {
                r.push_str(format!("-{}", self.symbols[k.sym]).as_str());
                r.push('[');
                let max = k.tpe.len() - 2;
                for (counter, param) in k.tpe[0..k.tpe.len() - 1].iter().enumerate() {
                    r.push_str(self.symbols[*param].as_str());
                    if counter < max {
                        r.push(',');
                    }
                }
                r.push_str("] <- ");
                r.push_str(self.symbols[k.tpe[k.tpe.len() - 1]].as_str());
                r.push('\n');
            }
        } else {
            r.push_str(EMPTY);
        }
        write!(f, "{}", r)
    }
}

impl CustomSymbolTable {
    pub fn add_symbol(&mut self, new_symbol: SAtom, _type: Option<SymId>, ft: FactType) -> SymId {
        let sym: Sym = new_symbol;
        let sym_id: SymId = self.symbols.len().into();
        self.symbols.push(sym.clone());
        self.ids.insert(sym.clone(), sym_id);
        self.symbol_fact_type.insert(sym_id, ft);
        match _type {
            None => {}
            Some(t) => {
                self.symbol_types.insert(sym_id, t);
            }
        };
        match ft {
            FactType::Type => {
                self.types.insert(sym_id, sym);
            }
            FactType::Object => {
                self.objects.insert(sym_id, sym);
            }
            _ => {}
        };
        sym_id
    }

    pub fn delete_symbol(&mut self, sym: &Sym) -> FactBaseResult {
        let sym_id = self.ids.remove(sym).unwrap();
        self.symbols.remove(usize::from(sym_id));
        self.symbol_types.remove(&sym_id);
        self.symbol_fact_type.remove(&sym_id);
        Ok(FactBaseOk::Ok)
    }
}

///Getters
impl CustomSymbolTable {
    pub fn get_sym_id(&self, symbol: &Sym) -> Result<SymId, FactBaseError> {
        match self.ids.get(symbol) {
            None => Err(FactBaseError::UndefinedEntry(symbol.to_string())),
            Some(id) => Ok(*id),
        }
    }

    pub fn get_sym(&self, sym_id: &SymId) -> Result<Sym, FactBaseError> {
        Ok(self.symbols[*sym_id].clone())
    }

    pub fn get_type_id(&self, sym_id: &SymId) -> Option<SymId> {
        match self.symbol_types.get(sym_id) {
            None => None,
            Some(s) => Some(*s),
        }
    }

    pub fn get_fact_type(&self, sym_id: &SymId) -> FactType {
        *self.symbol_fact_type.get(sym_id).unwrap()
    }

    pub fn get_type(&self, type_id: &SymId) -> Sym {
        self.types[type_id].clone()
    }

    pub fn get_state_function(&self, sym_id: &SymId) -> CustomStateFun {
        self.state_funs
            .get(sym_id)
            .expect("sym_id does not correspond to a State Function")
            .clone()
    }
}

//Verification
impl CustomSymbolTable {
    pub fn is_symbol_defined(&self, symbol: &Sym) -> bool {
        self.ids.contains_key(symbol)
    }

    pub fn is_defined_type(&self, symbol_id: &SymId) -> Result<bool, FactBaseError> {
        match self.symbol_fact_type.get(symbol_id) {
            None => panic!("corruption in the fact base, missing keys"),
            Some(ft) => Ok(*ft == FactType::Type),
        }
    }

    pub fn is_value_of_type(&self, value: &Sym, type_id: &SymId) -> bool {
        let sym_type = self
            .get_sym(type_id)
            .expect("type_id corresponds to no symbol");
        match sym_type.as_str() {
            INT => {
                //println!("is an int");
                value.clone().as_str().parse::<u64>().is_ok()
            }
            BOOLEAN => {
                //println!("is a boolean");
                matches!(value.clone().as_str(), TRUE | FALSE)
            }
            _ => {
                let sym_id = self.ids[value];
                let sym_type = self
                    .symbol_types
                    .get(&sym_id)
                    .expect("Strong error while getting a symbol type_id");
                sym_type == type_id
            }
        }
    }
}
