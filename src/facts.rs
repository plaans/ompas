#[warn(unused_imports)]
use aries_planning::parsing::sexpr::{SList, parse, SExpr, ListIter};
use aries_planning::parsing::sexpr::{SAtom};
use std::collections::HashMap;
use std::fmt::{Error, Display, Formatter};
use aries_utils::input::{Sym, Input, ErrLoc};
use aries_model::symbols::{SymbolTable, SymId};
use aries_model::types::{UnreachableFromRoot, TypeId};
use aries_collections::ref_store::{RefPool, RefVec};
use crate::facts::FactBaseError::WrongCommand;
use aries_model::lang::Type;
use aries_planning::chronicles::StateFun;

static EMPTY: &str = "empty\n";

#[derive(Default)]
pub struct FactBase {
    //TODO : remove facts from FactBase
    facts : HashMap<Vec<SymId>, SymId>,
    constants: HashMap<SymId, Sym>,
    constants_sf: HashMap<Vec<SymId>, Sym>,
    variable_sf: HashMap<Vec<SymId>, Sym>,
    variables: HashMap<SymId, Sym>,
    symbol_table: CustomSymbolTable
}

impl Display for FactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n#Constants: ");
        if !self.constants.is_empty() {
            for (id,value) in &self.constants {
                let constant:Sym= self.symbol_table.symbols.get(usize::from(*id)).expect("wrong constant id").clone();
                r.push_str(format!("-{} = {}", constant, value).as_str());
            }
        }
        else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Constants predicates: ");
        if !self.constants_sf.is_empty() {

        }
        else { r.push_str(EMPTY);}
        r.push_str("\n#Variables: ");
        if !self.variables.is_empty() {
            for (id,value) in &self.variables {
                let var:Sym= self.symbol_table.symbols.get(usize::from(*id)).expect("wrong constant id").clone();
                r.push_str(format!("-{} = {}", var, value).as_str());
            }
        }
        else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Variable predicates: ");
        if !self.variable_sf.is_empty() {

        }
        else {
            r.push_str(EMPTY);
        }

        r.push_str("\n#Symbol Table: ");
        r.push_str(format!("{}", self.symbol_table).as_str());
        write!(f, "{}", r);
        Ok(())
    }
}

#[derive(Default)]
pub struct CustomSymbolTable {
    stateFuns: HashMap<SymId, StateFun>,
    types: HashMap<SymId, Sym>,
    symbols: Vec<Sym>,
    ids: HashMap<Sym, SymId>,
    symbol_types: HashMap<SymId, TypeId>,
    symbol_fact_type: RefVec<SymId, FactType>
}

impl Display for CustomSymbolTable {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n\t#types: ");
        if !self.types.is_empty() {
            for k in self.types.keys() {
                r.push_str(format!("\n\t-{}", self.symbols[*k]).as_str());
            }
        }
        else {
            r.push_str(EMPTY);
        }
        r.push_str("\n\t#State Function: ");
        if !self.stateFuns.is_empty() {
            for k in self.stateFuns.keys() {
                r.push_str(format!("\n\t-{}", self.symbols[*k]).as_str());
            }
        }
        else {
            r.push_str(EMPTY);
        }
        write!(f, "{}", r);
        Ok(())
    }
}

impl CustomSymbolTable {
    pub fn add_symbol(&mut self, new_type: SAtom, ft: FactType) -> () {
        match ft {
            FactType::Type => {
                let sym: Sym = new_type.into();
                let sym_id:SymId = self.symbols.len().into();
                self.symbols.push(sym.clone());
                self.ids.insert(sym.clone(), sym_id);
                self.symbol_fact_type.push(FactType::Type);
                self.symbol_types.insert(sym_id,TypeId::from(usize::from(sym_id)));
                self.types.insert(sym_id,sym.clone());
            },
            FactType::SF => {

            },
            FactType::Constant => {

            },
            FactType::ConstantSV => {},
            FactType::variable => {},
            FactType::VariableSV => {},
        };
    }
    pub fn check_type(&self, _type: SAtom) -> Result<TypeId, FactBaseError> {
        unimplemented!()
    }
}

pub enum FactType {
    Constant,
    ConstantSV,
    variable,
    VariableSV,
    Type,
    SF
}

pub enum FactBaseError{
    WrongCommand(ErrLoc),
    WrongFact(ErrLoc),
    Other
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
            FactBaseError::Other => write!(f, "other king of FactBase error")
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
            symbol_table: Default::default()
        }
    }

    pub fn add_new_fact(&mut self, mut fact: ListIter) -> Result<(),FactBaseError> {
        print!("evaluating which king of fact we want to add...");
        let mut fact = fact.pop_list()?.iter();
        let fact_type = fact.pop_atom()?.as_str();
        match fact_type {
            "type" => {
                println!("define a new type");
                self.add_type(fact);
            }
            "predicate" | "pred" => {
                println!("define a new predicate")
            },
            "constant" | "const" => {
                println!("define a new constant, can be a variable or a state var")
            },
            "variable" | "var" => {
                println!("define a new variable")
            }
            "state-var" | "sv" => {
                println!("define a new state var")
            }
            _ => {
                println!("error")
            }
        };
        /*let mut key: Vec<_> = vec![];
        for i in 0..len - 1 {
            key.push(command.pop_atom()?.clone());
        }
        let value = command.pop_atom()?.clone();
        self.fact_base.add(key, value);*/

        Ok(())
    }

    pub fn set_fact(&mut self, mut fact: ListIter) -> Result<(), FactBaseError> {
        /// We can set a state variable or a variable
        /// The constants and invariants of a world cannot be change and will raise an error.
        let sym: Sym = fact.pop_atom()?.as_str().into();
        let id = self.symbol_table.ids.get(&sym);
        Ok(())
    }

    pub fn get_fact(&self, fact : ListIter) -> Result<SExpr, FactBaseError> {
        let default = SAtom::new("none".to_string());
        Ok(SExpr::Atom("()".into()))
    }

    pub fn add_type(&mut self, mut t_type: ListIter) -> Result<(), FactBaseError>{
        match t_type.len() {
            1 => {
                let atom = t_type.pop_atom()?;
                self.symbol_table.add_symbol(atom.clone(), FactType::Type);
            },
            _ => println!("wrong definition size")
        }
        Ok(())
    }

    pub fn add(&mut self, key: Vec<Sym>, value: SymId) {
        //self.facts.insert(key, value);
    }

    pub fn add_new_type(&mut self, mut new_type: (Sym, Option<Sym>)) -> Result<(), FactBaseError>
    {
        //self.symbol_table.types.add_new_type(new_type)
        Ok(())
    }



    pub fn set(&mut self, key: Vec<SAtom>, new_value: SAtom) -> Result<(), &'static str> {
        /*if self.facts.contains_key(&key) {
            self.facts.insert(key, new_value);
            ()
        }*/
        Err("wrong fact key")
    }
}