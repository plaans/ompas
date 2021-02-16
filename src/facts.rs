use crate::facts::language::*;
use crate::facts::FactBaseError::{AlreadyDefined, WrongNumberOfArgument};
use anyhow::*;
use aries_model::symbols::SymId;
use aries_planning::parsing::sexpr::SAtom;
use aries_planning::parsing::sexpr::{ListIter, SExpr};
use aries_utils::input::{ErrLoc, Sym};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::path::Path;

const EMPTY: &str = "empty\n";

pub const FILE_EXTENSION: &str = "fb";
//TODO: define static string for
pub mod language {
    pub const PRED_SHORT: &str = "pred";
    pub const PRED_LONG: &str = "predicate";
    pub const VAR_SHORT: &str = "var";
    pub const VAR_LONG: &str = "variable";
    pub const CONST_SHORT: &str = "const";
    pub const CONST_LONG: &str = "constant";
    pub const STATEVAR_SHORT: &str = "sv";
    pub const STATEVAR_LONG: &str = "state-variable";
    pub const SYM_SHORT: &str = "sym";
    pub const SYM_LONG: &str = "symbol";
    pub const TYPE: &str = "type";

    //
    pub const INT: &str = "int";
    pub const BOOLEAN: &str = "boolean";
    pub const OBJECT: &str = "object";
    pub const TRUE: &str = "true";
    pub const FALSE: &str = "false";
}

pub mod commands {
    pub const COMMAND_HELP: &str = "help";
    pub const COMMAND_PATH: &str = "path";
    pub const COMMAND_DEFINE: &str = "let";
    pub const COMMAND_MODIFY: &str = "set";
    pub const COMMAND_GET: &str = "get";
    pub const COMMAND_PRINT: &str = "print";

    pub const COMMAND_EXIT: &str = "exit";
    pub const COMMAND_CLOSE: &str = "close";
    pub const COMMAND_GET_ALL: &str = "get-all";

    pub const COMMAND_READ: &str = "read";
    pub const COMMAND_WRITE: &str = "write";
    pub const HIST_SHORT: &str = "hist";
    pub const HIST_LONG: &str = "history";

}

#[derive(PartialEq, Copy, Clone)]
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
    ErrLoc(ErrLoc),
    WrongNumberOfArgument(String),
    AlreadyDefined(String),
    UndefinedType(String),
    UndefinedEntry(String),
    MissExpectedSymbol(String),
    Default(String),
}

pub enum FactBaseOk {
    Ok,
    SExpr(SExpr),
    String(String)
}

pub type FactBaseResult = Result<FactBaseOk, FactBaseError>;

impl From<ErrLoc> for FactBaseError {
    fn from(e: ErrLoc) -> Self {
        FactBaseError::ErrLoc(e)
    }
}

impl Display for FactBaseError {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            FactBaseError::ErrLoc(e) => write!(f, "{}", e),
            FactBaseError::Default(s) => write!(f, "Error in factBase: {}", s),
            FactBaseError::AlreadyDefined(e) => write!(f, "Already defined entry : {}", e),
            FactBaseError::UndefinedEntry(e) => write!(f, "Undefined symbol: {}", e),
            _ => write!(f, "Not yet handled error"),
        }
    }
}

impl Into<std::fmt::Error> for FactBaseError {
    fn into(self) -> std::fmt::Error {
        std::fmt::Error::default()
    }
}

#[derive(Default)]
pub struct FactBase {
    ///List of constants and their value
    ///Example: number_room = 3
    constants: HashMap<SymId, Option<Sym>>,
    ///List of constants predicates and their value
    ///Example: door(rooma,roomb)= true
    constants_sf: HashMap<Vec<SymId>, Sym>,
    ///List of variables
    /// Example: counter_step = 3
    variables: HashMap<SymId, Option<Sym>>,
    /// List of variable state function
    /// loc(robota) = kitchen
    variable_sf: HashMap<Vec<SymId>, Sym>,
    ///Contains all the symbols defined with let. A fact can have as a value an other symbol or an integer (or boolean) value.
    symbol_table: CustomSymbolTable,
}

impl FactBase {
    pub fn from_file(_path: &Path) -> Result<Self, FactBaseError> {
        unimplemented!()
    }

    /// Transform the factbase into Lisp commands to reconstruct its states.
    pub fn to_file(&self) -> Result<String, FactBaseError> {
        let mut string = String::new();

        string.push('(');

        //Define all the types
        for types in self.symbol_table.types.clone() {
            string.push_str(format!("(let (type {}))\n",types.1).as_str())
        }
        //Define the predicates
        for pred in self.symbol_table.state_funs.clone() {
            string.push_str("(let (pred ");
            string.push_str(self.get_sym(&pred.0)?.as_str());
            for type_ in pred.1.tpe{
                string.push(' ');
                string.push_str(self.get_sym(&type_)?.as_str())
            }
            string.push_str("))\n");
        }
        //Define all constants variables
        for (constant_id, value) in self.constants.clone() {
            let sym = self.symbol_table.get_sym(&constant_id)?;
            let type_id = self.symbol_table.get_type(&constant_id);
            let sym_type = self.get_sym(&type_id)?;
            string.push_str(format!("(let (const var {} - {}",sym, sym_type).as_str());
            match value {
                Some(s) => string.push_str(format!("= {} ))\n ", s).as_str()),
                None => string.push_str("))\n")
            };

        }
        //Define all the variables
        for (var_id, value) in self.variables.clone() {
            let sym = self.symbol_table.get_sym(&var_id)?;
            let type_id = self.symbol_table.get_type(&var_id);
            let sym_type = self.get_sym(&type_id)?;
            string.push_str(format!("(let (var {} - {}",sym, sym_type).as_str());
            match value {
                Some(s) => string.push_str(format!("= {} )) \n", s).as_str()),
                None => string.push_str("))\n")
            };
        }
        //Define all const predicates
        for (params_id, value) in self.constants_sf.clone() {
            string.push_str("(let (const sv ");
            for param_id in params_id {
                string.push_str(self.get_sym(&param_id)?.as_str());
                string.push(' ');
            }
            string.push_str(value.as_str());
            string.push_str("))\n");
        }
        //Define state variables
        for (params_id, value) in self.variable_sf.clone() {
            string.push_str("(let (sv ");
            for param_id in params_id {
                string.push_str(self.get_sym(&param_id)?.as_str());
                string.push(' ');
            }
            string.push_str(value.as_str());
            string.push_str("))\n");
        }
        //Define all the state variables
        string.push(')');

        Ok(string)
    }
}

impl FactBase {
    pub fn new(&mut self) -> Self {
        FactBase {
            constants: Default::default(),
            constants_sf: Default::default(),
            variable_sf: Default::default(),
            variables: Default::default(),
            symbol_table: Default::default(),
        }
    }

    pub fn add_new_fact(&mut self, mut fact: ListIter) -> FactBaseResult {
        print!("evaluating which king of fact we want to add...");
        let mut fact = fact.pop_list()?.iter();
        let fact_type = fact.pop_atom()?.as_str();
        match fact_type {
            TYPE => {
                println!("define a new type");
                self.add_type(fact)
            }
            PRED_LONG | PRED_SHORT => {
                println!("define a new predicate");
                self.add_predicate(fact)
            }
            CONST_LONG | CONST_SHORT => {
                println!("define a new constant, can be a variable or a state var");
                match fact.pop_atom()?.as_str() {
                    STATEVAR_LONG | STATEVAR_SHORT => self.add_state_var(fact, true),
                    VAR_LONG | VAR_SHORT => self.add_var(fact, true),
                    e => {
                        return Err(FactBaseError::Default(format!(
                            "Expected var or sv, got {}",
                            e
                        )))
                    }
                }
            }
            VAR_LONG | VAR_SHORT => {
                println!("define a new variable");
                self.add_var(fact, false)
            }
            STATEVAR_LONG | STATEVAR_SHORT => {
                println!("define a new state var");
                self.add_state_var(fact, false)
            }
            SYM_LONG | SYM_SHORT => {
                println!("define a new symbol with a type");
                self.add_symbol(fact)
            }
            _ => {
                println!("error");
                Ok(FactBaseOk::Ok)
            }
        }
        /*let mut key: Vec<_> = vec![];
        for i in 0..len - 1 {
            key.push(command.pop_atom()?.clone());
        }
        let value = command.pop_atom()?.clone();
        self.fact_base.add(key, value);*/
    }

    pub fn set_fact(&mut self, mut fact: ListIter) -> FactBaseResult {
        // We can set a state variable or a variable
        // The constants and invariants of a world cannot be change and will raise an error.
        let sym: Sym = fact.pop_atom()?.as_str().into();
        let _id = self.symbol_table.ids.get(&sym);
        Ok(FactBaseOk::Ok)
    }

    pub fn get_fact(&self, _fact: ListIter) -> FactBaseResult {
        let _default: SExpr = SExpr::Atom(SAtom::new("none".to_string()));
        Ok(FactBaseOk::SExpr(_default))
    }

    //ADDERS

    pub fn add_type(&mut self, mut t_type: ListIter) -> FactBaseResult {
        match t_type.len() {
            1 => {
                let sym_type:Sym = t_type.pop_atom()?.into();
                //TODO: change None to next atom to support parent type.
                match self.symbol_table.is_symbol_defined(&sym_type) {
                    false => self.symbol_table.add_symbol(sym_type.clone(), None, FactType::Type),
                    true => return Err(FactBaseError::AlreadyDefined(sym_type.as_str().to_string()))
                };
            }
            _ => println!("wrong definition size"),
        }
        Ok(FactBaseOk::Ok)
    }

    ///Add a new predicate
    ///
    pub fn add_predicate(&mut self, mut predicate: ListIter) -> FactBaseResult {
        if predicate.len() >= 2 {
            let name: Sym = predicate.pop_atom()?.into();
            //check if the symbol is already defined.
            if self.symbol_table.is_symbol_defined(&name) {
                return Err(FactBaseError::AlreadyDefined(name.to_string()));
            }
            let mut params_id: Vec<SymId> = Vec::new();
            while predicate.len() > 0 {
                let param: Sym = predicate.pop_atom()?.into();
                let param_id = self.symbol_table.get_sym_id(&param)?;

                match self.symbol_table.is_defined_type(&param_id) {
                    Ok(true) => {}
                    Ok(false) => return Err(FactBaseError::UndefinedType(param.symbol)),
                    Err(e) => return Err(e),
                };
                let param_id = match self.symbol_table.get_sym_id(&param) {
                    Err(e) => return Err(e),
                    Ok(id) => id.clone(),
                };
                params_id.push(param_id);
            }
            let sym_id = self
                .symbol_table
                .add_symbol(name.clone(), None, FactType::SF);
            let sf: CustomStateFun = CustomStateFun {
                sym: sym_id,
                tpe: params_id,
            };
            self.symbol_table.state_funs.insert(sym_id, sf);
        } else {
            return Err(WrongNumberOfArgument(predicate.pop()?.to_string()));
        }
        Ok(FactBaseOk::Ok)
    }

    ///Add a new constant
    ///can be a symbol, a variable or a state variable
    pub fn add_var(&mut self, mut var: ListIter, is_const: bool) -> FactBaseResult {
        /* To define a const var we need a name for the symbol, a type and a value
          An example to define a variable:
         (let (const var fuel_max - int = 100))
        */
        let mut has_value = false;
        let mut has_type: bool = false;
        let mut option_type_id: Option<SymId> = None;
        let mut value:Option<Sym> =None;
        match var.len() {
            1 => {}
            3 => has_type = true,
            5 => {has_type = true; has_value = true;},
            _ => return Err(FactBaseError::WrongNumberOfArgument(var.pop()?.to_string())),
        };

        let name = var.pop_atom()?.clone();
        //Check if the symbol has already been defined.
        if self
            .symbol_table
            .is_symbol_defined(&Sym::from(name.as_str()))
        {
            return Err(AlreadyDefined(name.to_string()));
        }
        if has_type {
            var.pop_known_atom("-")?;
            //get type_id here or exit function if error
            let sym_type: &Sym = var.pop_atom()?.into();

            option_type_id = Some(self.symbol_table.get_sym_id(sym_type)?.clone());
            if !self
                .symbol_table
                .is_defined_type(&option_type_id.unwrap())?
            {
                return Err(FactBaseError::UndefinedType(sym_type.to_string()));
            }
            if has_value {
                var.pop_known_atom("=")?;
                //get value here.
                let sym_value = var.pop_atom()?.into();
                //check the type of the value
                if !self
                    .symbol_table
                    .is_value_of_type(&sym_value, &option_type_id.unwrap())
                {
                    return Err(FactBaseError::Default("wrong type".to_string()));
                }
                value = Some(sym_value.clone());
                //...
            }
        }

        let sym_id = self
            .symbol_table
            .add_symbol(name, option_type_id, FactType::Constant);
        if is_const {
            self.constants.insert(sym_id, value);
        } else {
            self.variables.insert(sym_id, value);
        }

        Ok(FactBaseOk::Ok)
    }

    pub fn add_state_var(
        &mut self,
        mut state_var: ListIter,
        is_const: bool,
    ) -> FactBaseResult {
        //TODO: Support undefined state var and infer types.

        let predicate_name = state_var.pop_atom()?.into();
        let predicate_id = self.symbol_table.get_sym_id(&predicate_name)?;
        let mut params: Vec<Sym> = Vec::new();
        let mut params_id: Vec<SymId> = Vec::new();
        let mut value: Sym = Sym::from("");
        params_id.push(predicate_id);

        if !(self.symbol_table.get_fact_type(&predicate_id) == FactType::SF) {
            return Err(FactBaseError::Default(format!(
                "{} is not a State Function",
                predicate_name
            )));
        }
        let sf = self.symbol_table.get_state_function(&predicate_id);
        //for each parameter we have to check if the type is good.
        for (index, type_id) in sf.tpe.iter().enumerate() {
            let param: Sym = state_var.pop_atom()?.into();
            let param_id = self.symbol_table.get_sym_id(&param)?;
            let param_type_id = self.symbol_table.get_type(&param_id);
            if param_type_id != *type_id {
                return Err(FactBaseError::Default(format!(
                    "wrong type: expected {} got {}",
                    self.symbol_table.symbols[sf.tpe[index]],
                    self.symbol_table.symbols[param_type_id]
                )));
            };
            if index < sf.tpe.len() - 1 {
                params.push(param);
                params_id.push(param_id);
            } else {
                value = param
            }
        }
        if is_const {
            self.constants_sf.insert(params_id, value);
        } else {
            self.variable_sf.insert(params_id, value);
        }

        Ok(FactBaseOk::Ok)
    }

    pub fn add_symbol(&mut self, mut symbol: ListIter) -> FactBaseResult {
        if symbol.len() != 3 {
            return Err(FactBaseError::WrongNumberOfArgument(
                symbol.pop()?.to_string(),
            ));
        }
        let sym: Sym = symbol.pop_atom()?.into();
        symbol.pop_known_atom("-")?;
        let _type: Sym = symbol.pop_atom()?.into();
        let type_id = self.symbol_table.get_sym_id(&_type)?;
        let sym_id = self
            .symbol_table
            .add_symbol(sym, Some(type_id), FactType::Constant);
        self.constants.insert(sym_id, None);
        Ok(FactBaseOk::Ok)
    }

    pub fn set(&mut self, _key: Vec<SAtom>, _new_value: SAtom) -> Result<(), &'static str> {
        /*if self.facts.contains_key(&key) {
            self.facts.insert(key, new_value);
            ()
        }*/
        Err("wrong fact key")
    }

    fn get_sym(&self, sym_id: &SymId) -> Result<Sym, FactBaseError> {
        self.symbol_table.get_sym(sym_id)
    }
}

impl Display for FactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n#Constants: ");
        if !self.constants.is_empty() {
            r.push('\n');
            for (id, value) in &self.constants {
                let constant= self.get_sym(id).unwrap();
                let type_id =self.symbol_table.get_type(id);
                let sym_type = self.get_sym(&type_id).unwrap();
                r.push_str(format!("-{}({})", constant, sym_type).as_str());
                match value {
                    None => {}
                    Some(s) => r.push_str(format!(" = {}", s).as_str())
                };
                r.push('\n');
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Constants predicates: ");
        if !self.constants_sf.is_empty() {
            r.push('\n');
            for (params, value) in &self.constants_sf {
                let params = params.clone();
                let mut params: Vec<Sym> = params
                    .iter()
                    .map(|&id| self.symbol_table.symbols[usize::from(id)].clone())
                    .collect();
                r.push_str(format!("-{}[", params.remove(0)).as_str());
                for (index, param) in params.iter().enumerate() {
                    if index > 0 {
                        r.push(',');
                    }
                    r.push_str(param.as_str());
                }
                r.push_str(format!("] = {}\n", value).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Variables: ");
        if !self.variables.is_empty() {
            r.push('\n');
            for (id, value) in &self.variables {
                let var = self.get_sym(id).unwrap();
                let type_id = self.symbol_table.get_type(id);
                let sym_type = self.get_sym(&type_id).unwrap();
                r.push_str(format!("-{}({})", var, sym_type).as_str());
                match value {
                    None => {}
                    Some(s) => r.push_str(format!(" = {}", s).as_str())
                };
                r.push('\n');
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#Variable predicates: ");
        if !self.variable_sf.is_empty() {
            r.push('\n');
            for (params, value) in &self.variable_sf {
                let params = params.clone();
                let mut params: Vec<Sym> = params
                    .iter()
                    .map(|&id| self.symbol_table.symbols[usize::from(id)].clone())
                    .collect();
                r.push_str(format!("-{}[", params.remove(0)).as_str());
                for (index, param) in params.iter().enumerate() {
                    if index > 0 {
                        r.push(',');
                    }
                    r.push_str(param.as_str());
                }
                r.push_str(format!("] = {}\n", value).as_str());
            }
        } else {
            r.push_str(EMPTY);
        }

        r.push_str("\n#Symbol Table: ");
        r.push_str(format!("{}", self.symbol_table).as_str());
        write!(f, "{}", r)
    }
}

pub struct CustomSymbolTable {
    /// Set of state functions.
    /// A state function is a symbol that match a state function.
    /// A StateFun has a name (Sym) and a set of types, the last type is the return value
    state_funs: HashMap<SymId, CustomStateFun>,
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
            ids: Default::default(),
            symbol_types: Default::default(),
            symbol_fact_type: Default::default(),
        };
        st.add_symbol(SAtom::new(INT), None, FactType::Type);
        st.add_symbol(SAtom::new(BOOLEAN), None, FactType::Type);
        st.add_symbol(SAtom::new(OBJECT), None, FactType::Type);

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
        if !self.state_funs.is_empty() {
            for k in self.state_funs.values() {
                r.push_str(format!("\n\t-{}", self.symbols[k.sym]).as_str());
                r.push('[');
                let mut counter = 0;
                let max = k.tpe.len() - 2;
                for param in &k.tpe[0..k.tpe.len() - 1] {
                    r.push_str(self.symbols[*param].as_str());
                    if counter < max {
                        r.push(',');
                    }
                    counter += 1;
                }
                r.push_str("] <- ");
                r.push_str(self.symbols[k.tpe[k.tpe.len() - 1]].as_str());
            }
        } else {
            r.push_str(EMPTY);
        }
        write!(f, "{}", r)
    }
}

impl CustomSymbolTable {
    pub fn add_symbol(&mut self, new_symbol: SAtom, _type: Option<SymId>, ft: FactType) -> SymId {
        let sym: Sym = new_symbol.into();
        let sym_id: SymId = self.symbols.len().into();
        self.symbols.push(sym.clone());
        self.ids.insert(sym.clone(), sym_id);
        self.symbol_fact_type.insert(sym_id, ft.clone());
        self.symbol_types
            .insert(sym_id, _type.unwrap_or(SymId::from(0)));
        if ft == FactType::Type {
            self.types.insert(sym_id, sym.clone());
        }
        sym_id
    }

    pub fn is_symbol_defined(&self, symbol: &Sym) -> bool {
        return self.ids.contains_key(symbol);
    }

    pub fn get_sym_id(&self, symbol: &Sym) -> Result<SymId, FactBaseError> {
        match self.ids.get(symbol.into()) {
            None => Err(FactBaseError::UndefinedEntry(symbol.to_string())),
            Some(id) => Ok(*id),
        }
    }

    pub fn get_sym(&self, sym_id: &SymId) -> Result<Sym, FactBaseError> {
        Ok(self.symbols[*sym_id].clone())
    }

    pub fn is_defined_type(&self, symbol_id: &SymId) -> Result<bool, FactBaseError> {
        match self.symbol_fact_type.get(symbol_id) {
            None => panic!("corruption in the fact base, missing keys"),
            Some(ft) => Ok(*ft == FactType::Type),
        }
    }

    pub fn get_type(&self, sym_id: &SymId) -> SymId {
        self.symbol_types.get(sym_id).unwrap().clone()
    }

    pub fn get_fact_type(&self, sym_id: &SymId) -> FactType {
        self.symbol_fact_type.get(sym_id).unwrap().clone()
    }

    pub fn get_state_function(&self, sym_id: &SymId) -> CustomStateFun {
        self.state_funs
            .get(sym_id)
            .expect("sym_id does not correspond to a State Function")
            .clone()
    }

    pub fn is_value_of_type(&self, value: &Sym, type_id: &SymId) -> bool {
        let _type = self.symbols[usize::from(*type_id)].as_str();
        match _type {
            INT => {
                //println!("is an int");
                match value.clone().as_str().parse::<u64>() {
                    Ok(_) => return true,
                    Err(_) => return false,
                }
            }
            BOOLEAN => {
                //println!("is a boolean");
                match value.clone().as_str() {
                    TRUE | FALSE => return true,
                    _ => return false,
                }
            }
            _ => {
                //println!("is an object");
                //TODO: define a way to declare new symbols
                let sym_id = self.ids[value];
                let sym_type = self
                    .symbol_types
                    .get(&sym_id)
                    .expect("Strong error while getting a symbol type_id");
                return sym_type == type_id;
            }
        }
    }
}
