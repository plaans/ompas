#[warn(unused_imports)]
use crate::facts::FactBaseError::WrongCommand;
use anyhow::*;
use aries_model::symbols::SymId;
use aries_model::types::TypeId;
use aries_planning::chronicles::StateFun;
use aries_planning::parsing::sexpr::SAtom;
use aries_planning::parsing::sexpr::{ListIter, SExpr};
use aries_utils::input::{ErrLoc, Sym};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::Path;
use aries_model::lang::Type;

static EMPTY: &str = "empty\n";

#[derive(Default)]
pub struct FactBase {
    //TODO : remove facts from FactBase
    facts: HashMap<Vec<SymId>, SymId>,
    ///List of constants and their value
    ///Example: number_room = 3
    constants: HashMap<SymId, Sym>,
    ///List of constants predicates and their value
    ///Example: door(rooma,roomb)= true
    constants_sf: HashMap<Vec<SymId>, Sym>,
    ///List of variables
    /// Example: counter_step = 3
    variables: HashMap<SymId, Sym>,
    /// List of variable state function
    /// loc(robota) = kitchen
    variable_sf: HashMap<Vec<SymId>, Sym>,
    ///Contains all the symbols defined with let. A fact can have as a value an other symbol or an integer (or boolean) value.
    symbol_table: CustomSymbolTable,
}

impl FactBase {
    pub fn from_file(path: &Path) -> Result<Self, FactBaseError> {
        unimplemented!()
    }

    pub fn to_file(path: &Path) -> Result<Self, FactBaseError> {
        unimplemented!()
    }
}

impl Display for FactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n#Constants: ");
        if !self.constants.is_empty() {
            for (id, value) in &self.constants {
                let constant: Sym = self
                    .symbol_table
                    .symbols
                    .get(usize::from(*id))
                    .expect("wrong constant id")
                    .clone();
                r.push_str(format!("-{} = {}", constant, value).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Constants predicates: ");
        if !self.constants_sf.is_empty() {
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Variables: ");
        if !self.variables.is_empty() {
            for (id, value) in &self.variables {
                let var: Sym = self
                    .symbol_table
                    .symbols
                    .get(usize::from(*id))
                    .expect("wrong constant id")
                    .clone();
                r.push_str(format!("-{} = {}", var, value).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Variable predicates: ");
        if !self.variable_sf.is_empty() {
        } else {
            r.push_str(EMPTY);
        }

        r.push_str("\n#Symbol Table: ");
        r.push_str(format!("{}", self.symbol_table).as_str());
        write!(f, "{}", r);
        Ok(())
    }
}

pub struct CustomSymbolTable {
    /// Set of state functions.
    /// A state function is a symbol that match a state function.
    /// A StateFun has a name (Sym) and a set of types, the last type is the return value
    stateFuns: HashMap<SymId, CustomStateFun>,
    /// Set of types.
    types: HashMap<SymId, Sym>,
    ///Set of symbols that have been defined. We can access them with the SymId.
    symbols: Vec<Sym>,
    /// Match a symbol to an idea.
    ids: HashMap<Sym, SymId>,
    /// Match a symbol to a type.
    /// A type has his parent has type.
    symbol_types: HashMap<SymId, SymId>,
    /// Each symbol is associated to a FactType.
    symbol_fact_type: HashMap<SymId, FactType>,
}

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
            stateFuns: Default::default(),
            types: Default::default(),
            symbols: vec![],
            ids: Default::default(),
            symbol_types: Default::default(),
            symbol_fact_type: Default::default(),
        };
        st.add_symbol(SAtom::new("int"), None, &FactType::Type);
        st.add_symbol(SAtom::new("boolean"), None, &FactType::Type);
        st.add_symbol(SAtom::new("object"), None, &FactType::Type);

        st
    }
}

impl Display for CustomSymbolTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n\t#types: ");
        if !self.types.is_empty() {
            for k in self.types.keys() {
                r.push_str(format!("\n\t-{}", self.symbols[*k]).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n\t#State Function: ");
        if !self.stateFuns.is_empty() {
            for k in self.stateFuns.values() {
                r.push_str(format!("\n\t-{}", self.symbols[k.sym]).as_str());
                r.push('[');
                let mut counter = 0;
                let max = k.tpe.len()-2;
                for param in &k.tpe[0..k.tpe.len()-1] {
                    r.push_str(self.symbols[*param].as_str());
                    if counter < max {
                       r.push(',');
                    }
                    counter+=1;
                }
                r.push_str("] <- ");
                r.push_str(self.symbols[k.tpe[k.tpe.len()-1]].as_str());

            }
        } else {
            r.push_str(EMPTY);
        }
        write!(f, "{}", r);
        Ok(())
    }
}

impl CustomSymbolTable {
    pub fn add_symbol(&mut self, new_symbol: SAtom, _type: Option<&SymId>, ft: &FactType) -> SymId {
        let sym: Sym = new_symbol.into();
        let sym_id: SymId = self.symbols.len().into();
        self.symbols.push(sym.clone());
        self.ids.insert(sym.clone(), sym_id);
        self.symbol_fact_type.insert(sym_id, *ft);
        self.symbol_types
            .insert(sym_id, *_type.unwrap_or(&SymId::from(0)));
        if *ft == FactType::Type {self.types.insert(sym_id, sym.clone());}
        sym_id
    }

    pub fn is_symbol_defined(&self, symbol: &Sym) -> bool {
        return self.ids.contains_key(symbol);
    }

    pub fn get_sym_id(&self, symbol: &Sym) -> Option<&SymId> {
        self.ids.get(symbol.into())
    }

    pub fn is_defined_type(&self, symbol: &Sym) -> Result<bool, FactBaseError> {
        match self.get_sym_id(symbol) {
            None => Err(FactBaseError::UndefinedEntry(symbol.clone().to_string())),
            Some(id) => match self.symbol_fact_type.get(&id.clone()) {
                None => panic!("corruption in the fact base, missing keys"),
                Some(ft) => Ok(*ft == FactType::Type),
            },
        }
    }

    pub fn get_type(&self, sym_id: &SymId) -> &SymId {
        self.symbol_types.get(sym_id).unwrap()
    }
}

#[derive(PartialEq,Copy, Clone)]
pub enum FactType {
    Constant,
    ConstantSV,
    Variable,
    VariableSV,
    Type,
    SF,
}

#[derive(Debug)]
pub enum FactBaseError {
    WrongCommand(ErrLoc),
    WrongFact(ErrLoc),
    Other,
    WrongNumberOfArgument(String),
    AlreadyDefined(String),
    UndefinedType(String),
    UndefinedEntry(String),
}

impl From<ErrLoc> for FactBaseError {
    fn from(e: ErrLoc) -> Self {
        WrongCommand(e)
    }
}

impl Display for FactBaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            FactBaseError::WrongCommand(e) => write!(f, "wrong command: {}", e),
            FactBaseError::WrongFact(e) => write!(f, "wrong fact: {}", e),
            FactBaseError::Other => write!(f, "other king of FactBase error"),
            _ => write!(f, "Error"),
        }
    }
}

impl FactBase {
    pub fn new(&mut self) -> Self {
        FactBase {
            facts: Default::default(),
            constants: Default::default(),
            constants_sf: Default::default(),
            variable_sf: Default::default(),
            variables: Default::default(),
            symbol_table: Default::default(),
        }
    }

    pub fn add_new_fact(&mut self, mut fact: ListIter) -> Result<(), FactBaseError> {
        print!("evaluating which king of fact we want to add...");
        let mut fact = fact.pop_list()?.iter();
        let fact_type = fact.pop_atom()?.as_str();
        match fact_type {
            "type" => {
                println!("define a new type");
                self.add_type(fact)
            }
            "predicate" | "pred" => {
                println!("define a new predicate");
                self.add_predicate(fact)
            }
            "constant" | "const" => {
                println!("define a new constant, can be a variable or a state var");
                Ok(())
            }
            "variable" | "var" => {
                println!("define a new variable");
                Ok(())
            }
            "state-var" | "sv" => {
                println!("define a new state var");
                Ok(())
            }
            _ => {
                println!("error");
                Ok(())
            }
        }
        /*let mut key: Vec<_> = vec![];
        for i in 0..len - 1 {
            key.push(command.pop_atom()?.clone());
        }
        let value = command.pop_atom()?.clone();
        self.fact_base.add(key, value);*/
    }

    pub fn set_fact(&mut self, mut fact: ListIter) -> Result<(), FactBaseError> {
        // We can set a state variable or a variable
        // The constants and invariants of a world cannot be change and will raise an error.
        let sym: Sym = fact.pop_atom()?.as_str().into();
        let id = self.symbol_table.ids.get(&sym);
        Ok(())
    }

    pub fn get_fact(&self, fact: ListIter) -> Result<SExpr, FactBaseError> {
        let default = SAtom::new("none".to_string());
        Ok(SExpr::Atom("()".into()))
    }

    //ADDERS

    pub fn add_type(&mut self, mut t_type: ListIter) -> Result<(), FactBaseError> {
        match t_type.len() {
            1 => {
                let atom = t_type.pop_atom()?;
                //TODO: change None to next atom to support parent type.
                self.symbol_table
                    .add_symbol(atom.clone(), None, &FactType::Type);
            }
            _ => println!("wrong definition size"),
        }
        Ok(())
    }

    ///Add a new predicate
    ///
    pub fn add_predicate(&mut self, mut predicate: ListIter) -> Result<(), FactBaseError> {
        if predicate.len() >= 2 {
            let name: Sym = predicate.pop_atom()?.into();
            //check if the symbol is already defined.
            //TODO: create a custom function to verify if a symbol has already been defined.
            match self.symbol_table.ids.contains_key(&name) {
                true => {
                    return Err(FactBaseError::AlreadyDefined(format!(
                        "{} is already defined",
                        name
                    )))
                }
                false => {}
            };
            let mut params_id: Vec<SymId> = Vec::new();
            while predicate.len() > 0 {
                let param: Sym = predicate.pop_atom()?.into();

                //TODO: create a custom function to verify if a type exist.
                match self.symbol_table.is_defined_type(&param){
                    Ok(true) => {},
                    Ok(false) => return Err(FactBaseError::UndefinedType(param.symbol)),
                    Err(e) => return Err(e)
                };
                let param_id = match self.symbol_table.get_sym_id(&param){
                    None => panic!("Critical error in FactBase"),
                    Some(id) => id.clone()
                };
                params_id.push(param_id);
            }
            let sym_id = self.symbol_table.add_symbol(name, None, &FactType::SF);
            let sf : CustomStateFun = CustomStateFun {
                sym: sym_id,
                tpe: params_id
            };
            self.symbol_table.stateFuns.insert(sym_id, sf);
        } else {
            return Err(FactBaseError::Other);
        }
        Ok(())
    }

    pub fn add(&mut self, key: Vec<Sym>, value: SymId) {
        //self.facts.insert(key, value);
    }

    pub fn set(&mut self, key: Vec<SAtom>, new_value: SAtom) -> Result<(), &'static str> {
        /*if self.facts.contains_key(&key) {
            self.facts.insert(key, new_value);
            ()
        }*/
        Err("wrong fact key")
    }
}
