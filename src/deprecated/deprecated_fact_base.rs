use anyhow::*;
use aries_model::symbols::SymId;
use aries_planning::parsing::sexpr::{ListIter, SExpr, parse};
use aries_utils::input::{ErrLoc, Sym};
use std::collections::HashMap;
use std::fmt::{Display, Formatter};
use std::default::Default;
use crate::deprecated::deprecated_fact_base::FactBaseError::*;
use crate::deprecated::deprecated_custom_symbol_table::*;
use crate::deprecated::deprecated_fact_base::language::*;


pub const EMPTY: &str = "empty\n";

pub const FILE_EXTENSION: &str = "fb";

pub mod language {
    pub const PRED_SHORT: &str = "pred";
    pub const PRED_LONG: &str = "predicate";
    pub const VAR_SHORT: &str = "var";
    pub const VAR_LONG: &str = "variable";
    pub const CONST_SHORT: &str = "const";
    pub const CONST_LONG: &str = "constant";
    pub const STATEVAR_SHORT: &str = "sv";
    pub const STATEVAR_LONG: &str = "state-variable";
    pub const OBJ_SHORT: &str = "obj";
    pub const OBJ_LONG: &str = "object";
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
    pub const COMMAND_DEL : &str = "del";
    pub const COMMAND_TEST: &str = "test";

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
    Variable,
    StateVariable,
    Object,
    Type,
    SF,
}

pub enum FactBaseOk {
    Ok,
    SExpr(SExpr),
    String(String),
}

impl Display for FactBaseOk {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            FactBaseOk::Ok => write!(f, "ok"),
            FactBaseOk::SExpr(s) => write!(f, "{}", s),
            FactBaseOk::String(s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub enum FactBaseError {
    ErrLoc(ErrLoc),
    WrongNumberOfArgument(String, u64),
    AlreadyDefined(String),
    ChangingInvariant(String),
    UndefinedType(String),
    UndefinedEntry(String),
    MissExpectedSymbol(String, String),
    WrongType(String, String),
    Default,
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
            FactBaseError::Default => write!(f, "Error in factBase: "),
            AlreadyDefined(e) => write!(f, "Error: Already defined entry : {}", e),
            UndefinedEntry(e) => write!(f, "Error: Undefined symbol: {}", e),
            WrongType(s, t) => write!(f, "Error: {} is not of type {}", s, t),
            WrongNumberOfArgument(s, n) => write!(
                f,
                "Error: {} has not the right number of argument. Expected {}",
                s, n
            ),
            ChangingInvariant(s) => write!(f, "Error: {} is an invariant or a const", s),
            UndefinedType(s) => write!(f, "Error type: {} is not a type", s),
            MissExpectedSymbol(s, s2) => write!(f, "Error: got {} instead of {}", s, s2),
            //_ => write!(f, "Not yet handled error"),
        }
    }
}

impl Into<std::fmt::Error> for FactBaseError {
    fn into(self) -> std::fmt::Error {
        std::fmt::Error::default()
    }
}

#[derive(Clone)]
pub struct FactBaseValue(Option<Sym>, bool);

#[derive(Default)]
pub struct FactBase {
    ///List of variables
    /// Example: counter_step = 3
    variables: HashMap<Sym, FactBaseValue>,
    /// List of variable state function
    /// loc(robot) = kitchen
    /// loc(robot) = kitchen
    state_variables: HashMap<Vec<Sym>, FactBaseValue>,
    ///Contains all the symbols defined with let. A fact can have as a value an other symbol or an integer (or boolean) value.
    symbol_table: CustomSymbolTable,
}

impl FactBase {
    /// Transform the factbase into Lisp commands to reconstruct its states.
    pub fn to_commands(&self) -> Result<String, FactBaseError> {
        let mut string = String::new();

        string.push('(');

        //Define all the types
        let mut vec_types:Vec<_> = self.symbol_table.types.iter().collect();
        vec_types.sort_by(|&x,&y| x.0.cmp(y.0));
        for types in vec_types {
            match usize::from(*types.0) {
                TYPE_INT_ID | TYPE_BOOL_ID | TYPE_OBJECT_ID => {}
                _ => {
                    let sym_type = self.get_sym(&self.get_type_id(types.0).unwrap()).unwrap();
                    string.push_str(format!("(let (type {} - {}))\n", types.1, sym_type).as_str());
                }
            }

        }
        //Define the predicates
        for pred in self.symbol_table.state_funs.clone() {
            string.push_str("(let (pred ");
            string.push_str(self.get_sym(&pred.0)?.as_str());
            for type_ in pred.1.tpe {
                string.push(' ');
                string.push_str(self.get_sym(&type_)?.as_str())
            }
            string.push_str("))\n");
        }

        for object in self.symbol_table.objects.clone() {
            let type_id = self.get_type_id(&object.0).unwrap();
            let sym_type = self.get_sym(&type_id)?;
            string.push_str(format!("(let (object {} - {}))\n", object.1, sym_type).as_str());
        }

        for (sym, value) in self.variables.clone() {
            let sym_id = self.get_sym_id(&sym)?;
            let type_id = self.get_type_id(&sym_id).unwrap();
            let sym_type = self.get_sym(&type_id)?;
            string.push_str("(let (");
            if value.1 {
                string.push_str("const ");
            }
            string.push_str(format!("var {} - {}", sym, sym_type).as_str());
            match value.0 {
                Some(v) => string.push_str(format!(" = {} ))\n ", v).as_str()),
                None => string.push_str("))\n"),
            };
        }
        //Define all const predicates
        //Define state variables
        for (params, value) in self.state_variables.clone() {
            string.push_str("(let (");
            if value.1 {
                string.push_str("const ");
            }
            string.push_str("sv ");
            for param in params {
                string.push_str(param.as_str());
                string.push(' ');
            }
            match value.0 {
                Some(v) => string.push_str(format!("{} ))\n ", v).as_str()),
                None => string.push_str("))\n"),
            };
        }

        //Define all the state variables
        string.push(')');

        Ok(string)
    }
}

///Fact functions
impl FactBase {
    ///Create a new factbase with its default values
    pub fn new(&mut self) -> Self {
        FactBase {
            state_variables: Default::default(),
            variables: Default::default(),
            symbol_table: Default::default(),
        }
    }
    ///Add a fact to the fact base.
    /// A fact can be a type, a symbol, a state-function (state-variable template), a variable or a state-variable
    pub fn add_fact(&mut self, mut fact: ListIter) -> FactBaseResult {
        //print!("evaluating which kind of fact we want to add...");
        let mut fact = fact.pop_list()?.iter();
        let fact_type = fact.pop_atom()?.as_str();
        match fact_type {
            TYPE => {
                //println!("define a new type");
                self.add_type(fact)
            }
            PRED_LONG | PRED_SHORT => {
                //println!("define a new predicate");
                self.add_predicate(fact)
            }
            CONST_LONG | CONST_SHORT => {
                //println!("define a new constant, can be a variable or a state var");
                match fact.pop_atom()?.as_str() {
                    STATEVAR_LONG | STATEVAR_SHORT => self.add_state_var(fact, true),
                    VAR_LONG | VAR_SHORT => self.add_var(fact, true),
                    e => return Err(FactBaseError::UndefinedEntry(e.to_string())),
                }
            }
            VAR_LONG | VAR_SHORT => {
                //println!("define a new variable");
                self.add_var(fact, false)
            }
            STATEVAR_LONG | STATEVAR_SHORT => {
                //println!("define a new state var");
                self.add_state_var(fact, false)
            }
            OBJ_LONG | OBJ_SHORT => {
                //println!("define a new symbol with a type");
                self.add_object(fact)
            }
            s => {
                return Err(FactBaseError::MissExpectedSymbol(s.to_string(),"a FactType (var, sv, const, symbol...)".to_string()))
            }
        }
        /*let mut key: Vec<_> = vec![];
        for i in 0..len - 1 {
            key.push(command.pop_atom()?.clone());
        }
        let value = command.pop_atom()?.clone();
        self.fact_base.add(key, value);*/
    }

    ///Set the value of a previously defined fact. The fact must be mutable (i.e not defined as const)
    pub fn set_fact(&mut self, mut fact: ListIter) -> FactBaseResult {
        // We can set a state variable or a variable
        // The constants and invariants of a world cannot be change and will raise an error.
        let mut fact = fact.pop_list()?.iter();
        let name_sym: Sym = fact.pop_atom()?.as_str().into();

        let sym_id = self.get_sym_id(&name_sym)?;
        let ft = self.get_fact_type(&sym_id);
        match ft {
            FactType::Variable => {
                //println!("setting a variable");
                //Verify if the variable is const
                let type_id = self.get_type_id(&sym_id).unwrap();
                //Verify the type of the value
                let mut value: Sym = fact.pop_atom()?.as_str().into();
                self.check_value_and_type(&mut value, &type_id)?;
                match self.is_const_variable(&name_sym) {
                    true => return Err(ChangingInvariant(name_sym.to_string())),
                    false => {
                        self.set_value_variable(name_sym, FactBaseValue(Some(value.clone()), false))
                    }
                }
            }
            FactType::SF => {
                //println!("Setting a state variable");
                let mut sv: Vec<Sym> = vec![name_sym];
                let sf = self.get_sf(&sym_id);
                self.check_value_and_type_sv(fact, &sf, &mut sv)?;
                let is_const = match self.is_const_state_variable(&sv) {
                    Ok(v) => v,
                    Err(_) => return Err(UndefinedEntry(vec_sym_to_string(sv))),
                };
                let value = sv.pop().unwrap();
                if !is_const {
                    self.set_value_state_variable(sv, FactBaseValue(Some(value), false));
                } else {
                    return Err(ChangingInvariant(vec_sym_to_string(sv)));
                }
            }
            _ => return Err(ChangingInvariant(name_sym.to_string())),
        };
        Ok(FactBaseOk::Ok)
    }

    ///Get the value of a defined fact.
    pub fn get_fact(&self, mut fact: ListIter) -> FactBaseResult {
        let result: Option<Sym>;
        let mut fact = fact.pop_list()?.iter();
        let name_sym = fact.pop_atom()?.as_str().into();
        let sym_id = self.get_sym_id(&name_sym)?;
        let ft = self.get_fact_type(&sym_id);
        match ft {
            FactType::Variable => {
                //println!("getting a variable");
                //Verify if the variable is const
                result = self.get_value_variable(&name_sym).0;
            }
            FactType::SF => {
                //println!("Getting a state variable");
                let mut sv: Vec<Sym> = vec![name_sym];
                let sf = self.get_sf(&sym_id);
                self.check_value_and_type_sv(fact, &sf, &mut sv)?;
                result = self.get_value_state_variable(&sv).0;
            }
            _ => result = Some(name_sym),
        };
        match result {
            None => Ok(FactBaseOk::String("None".to_string())),
            Some(r) => Ok(FactBaseOk::String(r.to_string())),
        }
    }

    ///Delete a fact as a state variable or a variable. Cannot delete a symbol or a state function.
    pub fn del_fact(&mut self, mut fact: ListIter) -> FactBaseResult {
        //println!("del fact");
        //TODO: reconstruct the fact to have an explicit error
        //TODO: Add an error when trying to deleter a const
        let mut fact = fact.pop_list()?.iter();
        let name_sym = fact.pop_atom()?.as_str().into();
        let sym_id = self.get_sym_id(&name_sym)?;
        let ft = self.get_fact_type(&sym_id);
        match ft {
            FactType::Variable => {
                //println!("getting a variable");
                //Verify if the variable is const
                self.variables.remove_entry(&name_sym);
                self.delete_symbol(&name_sym)?;
                return Ok(FactBaseOk::Ok)
            }
            FactType::SF => {
                //println!("Getting a state variable");
                let mut sv: Vec<Sym> = vec![name_sym];
                let sf = self.get_sf(&sym_id);
                self.check_value_and_type_sv(fact, &sf, &mut sv)?;
                return match self.state_variables.remove_entry(&sv) {
                    None => Err(FactBaseError::UndefinedEntry("fact was not defined".to_string())),
                    Some(_) => Ok(FactBaseOk::Ok)
                };
            }

            _ => return Err(FactBaseError::ChangingInvariant(list_iter_to_string(fact)))
        };
    }

    pub fn test_fact(&self, mut fact: ListIter) -> FactBaseResult{
        println!("testing a fact");
        let mut fact = fact.pop_list()?.iter();
        let name_sym: Sym = fact.pop_atom()?.as_str().into();

        let sym_id = self.get_sym_id(&name_sym)?;
        let ft = self.get_fact_type(&sym_id);
        let mut test_value;
        let real_value;
        match ft {
            FactType::Variable => {
                //println!("setting a variable");
                //Verify if the variable is const
                let type_id = self.get_type_id(&sym_id).unwrap();
                //Verify the type of the value
                test_value = fact.pop_atom()?.as_str().into();
                self.check_value_and_type(&mut test_value, &type_id)?;
                real_value = self.get_value_variable(&name_sym).0;
            }
            FactType::SF => {
                //println!("Setting a state variable");
                let mut sv: Vec<Sym> = vec![name_sym];
                let sf = self.get_sf(&sym_id);
                self.check_value_and_type_sv(fact, &sf, &mut sv)?;
                test_value = sv.last().unwrap().clone();
                real_value = self.get_value_state_variable(&sv).0;
            }
            _ => return Err(ChangingInvariant(name_sym.to_string())),
        };
        match real_value {
            None => Ok(FactBaseOk::String(FALSE.to_string())),
            Some(rv) => match rv == test_value {
                true => Ok(FactBaseOk::String(TRUE.to_string())),
                false => Ok(FactBaseOk::String(FALSE.to_string()))
            }
        }

    }

    //ADDERS
    ///Add a new type
    pub fn add_type(&mut self, mut new_type: ListIter) -> FactBaseResult {
        let mut has_parent_type = false;
        match new_type.len() {
            1 => {}
            3 => has_parent_type = true,
            _ => return Err(FactBaseError::WrongNumberOfArgument(list_iter_to_string(new_type), 3)),
        };
        let sym_type: Sym = new_type.pop_atom()?.into();
        match self.symbol_table.is_symbol_defined(&sym_type) {
            false => {
                let mut type_id = SymId::from(TYPE_OBJECT_ID);
                if has_parent_type {
                    new_type.pop_known_atom("-")?;
                    let sym_parent_type: Sym = new_type.pop_atom()?.as_str().into();
                    let sym_parent_type_id = self.get_sym_id(&sym_parent_type)?;
                    if self.symbol_table.is_defined_type(&sym_parent_type_id)?{
                        type_id = sym_parent_type_id;
                    }
                }
                self.symbol_table.add_symbol(sym_type.clone(),
                                             Some(type_id),
                                             FactType::Type);

            }
            true => {
                return Err(FactBaseError::AlreadyDefined(sym_type.as_str().to_string()))
            }
        };
        Ok(FactBaseOk::Ok)
    }

    ///Add a new predicate
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
            let sf = CustomStateFun {
                sym: sym_id,
                tpe: params_id,
            };
            self.symbol_table.state_funs.insert(sym_id, sf);
        } else {
            return Err(WrongNumberOfArgument(list_iter_to_string(predicate), 2));
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
        let mut has_type_or_value = false;
        let mut has_type_and_value = false;
        let mut option_type_id: Option<SymId> = Some(SymId::from(TYPE_OBJECT_ID));
        let mut value: Option<Sym> = None;
        match var.len() {
            1 => {}
            3 => has_type_or_value = true,
            5 => has_type_and_value = true,
            _ => {
                return Err(FactBaseError::WrongNumberOfArgument(
                    list_iter_to_string(var),
                    3,
                ))
            }
        };

        let name: Sym = var.pop_atom()?.clone().into();
        //Check if the symbol has already been defined.
        if self
            .symbol_table
            .is_symbol_defined(&Sym::from(name.as_str()))
        {
            return Err(AlreadyDefined(name.to_string()));
        }
        if has_type_or_value {
            match var.pop_atom()?.as_str() {
                "-" => {
                    //get type_id here or exit function if error
                    let sym_type: &Sym = var.pop_atom()?.into();
                    option_type_id = Some(self.symbol_table.get_sym_id(sym_type)?.clone());
                    if !self
                        .symbol_table
                        .is_defined_type(&option_type_id.unwrap())?
                    {
                        return Err(FactBaseError::UndefinedType(sym_type.to_string()));
                    }
                }
                "=" => {
                    println!("Infer type of the variable from the value");
                    let sym_value = var.pop_atom()?.as_str().into();
                    let type_id = self.infer_type(&sym_value)?;
                    match type_id {
                        Some(t) =>  {
                            println!("infered type is : {}",  self.get_sym(&t)?);
                            value = Some(sym_value);
                            option_type_id = type_id;
                        },
                        None => return Err(FactBaseError::UndefinedEntry(sym_value.to_string()))
                    };

                }
                e => return Err(FactBaseError::MissExpectedSymbol("- or =".to_string(), e.to_string()))
            }
        }

        else if has_type_and_value {
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
            var.pop_known_atom("=")?;
            //get value here.
            let sym_value: Sym = var.pop_atom()?.into();
            //check the type of the value
            if !self
                .symbol_table
                .is_value_of_type(&sym_value, &option_type_id.unwrap())
            {
                return Err(FactBaseError::WrongType(
                    sym_value.to_string(),
                    self.get_sym(&option_type_id.unwrap()).unwrap().to_string(),
                ));
            }
            value = Some(sym_value.clone());

        }

        self.symbol_table
            .add_symbol(name.clone(), option_type_id, FactType::Variable);
        self.variables
            .insert(name.clone(), FactBaseValue(value, is_const));

        Ok(FactBaseOk::Ok)
    }

    pub fn add_state_var(&mut self, mut state_var: ListIter, is_const: bool) -> FactBaseResult {

        let predicate_name: Sym = state_var.pop_atom()?.into();
        if self.symbol_table.is_symbol_defined(&predicate_name) {
            let predicate_id = self.symbol_table.get_sym_id(&predicate_name)?;
            let mut sv: Vec<Sym> = vec![predicate_name.clone()];
            if !(self.symbol_table.get_fact_type(&predicate_id) == FactType::SF) {
                return Err(FactBaseError::WrongType(
                    predicate_name.to_string(),
                    STATEVAR_LONG.to_string(),
                ));
            }
            let sf = self.get_sf(&predicate_id);
            //for each parameter we have to check if the type is good.
            self.check_value_and_type_sv(state_var, &sf, &mut sv)?;
            let value = sv.pop().unwrap();
            self.state_variables
                .insert(sv, FactBaseValue(Some(value), is_const));
        }
        else {
            //println!("Infer types of the state variable to define a new predicate");
            let mut types : Vec<Sym>= Vec::new();
            let mut params: Vec<Sym> = Vec::new();
            while state_var.len()>0 {
                let param : Sym= state_var.pop_atom()?.as_str().into();
                match self.infer_type(&param)? {
                    None => return Err(FactBaseError::UndefinedEntry(param.to_string())),
                    Some(t) => types.push(self.get_sym(&t)?)
                };
                params.push(param);
            }

            //Construct list iter to add new predicate and new state variable
            let mut string = String::new();
            string.push_str(format!("({}", predicate_name).as_str());
            for sym_type in types {
                string.push_str(format!(" {} ", sym_type).as_str());
            }
            string.push(')');
            let sexpr = parse(string.as_str()).unwrap();
            let list_iter = sexpr.as_list_iter().unwrap();
            self.add_predicate(list_iter)?;


            let mut string2 = String::new();
            string2.push_str(format!("({}", predicate_name).as_str());
            for param in params {
                string2.push_str(format!(" {} ", param).as_str());
            }
            string2.push(')');
            let sexpr2 = parse(string2.as_str()).unwrap();
            let list_iter2 = sexpr2.as_list_iter().unwrap();


            self.add_state_var(list_iter2, is_const)?;
            //define a new predicate
            //Add state var with new predicat
        }
        Ok(FactBaseOk::Ok)
    }

    pub fn add_object(&mut self, mut object: ListIter) -> FactBaseResult {
        if object.len() != 3 {
            return Err(FactBaseError::WrongNumberOfArgument(
                list_iter_to_string(object),
                3,
            ));
        }
        let sym: Sym = object.pop_atom()?.into();
        object.pop_known_atom("-")?;
        let sym_type: Sym = object.pop_atom()?.into();
        let type_id = self.get_sym_id(&sym_type)?;
        if !self.symbol_table.is_defined_type(&type_id)?{
            return Err(FactBaseError::UndefinedType(sym_type.to_string()))
        }
        self.symbol_table
            .add_symbol(sym.clone(), Some(type_id), FactType::Object);
        Ok(FactBaseOk::Ok)
    }
}

///Getters
impl FactBase {
    fn get_fact_type(&self, sym_id: &SymId) -> FactType {
        self.symbol_table.get_fact_type(sym_id)
    }

    fn get_sym(&self, sym_id: &SymId) -> std::result::Result<Sym, FactBaseError> {
        self.symbol_table.get_sym(sym_id)
    }

    fn get_sym_id(&self, sym: &Sym) -> std::result::Result<SymId, FactBaseError> {
        self.symbol_table.get_sym_id(sym)
    }

    fn get_sf(&self, sym_id: &SymId) -> CustomStateFun {
        self.symbol_table.get_state_function(sym_id)
    }

    fn get_type_id(&self, sym_id: &SymId) -> Option<SymId> {
        self.symbol_table.get_type_id(sym_id)
    }

    fn get_value_variable(&self, sym: &Sym) -> FactBaseValue {
        match self.variables.get(sym) {
            None => panic!("strong error in get_value_variable: id has no match in FactBase"),
            Some(r) => r.clone(),
        }
    }
    fn get_value_state_variable(&self, params: &Vec<Sym>) -> FactBaseValue {
        match self.state_variables.get(params) {
            None => panic!("strong error in get_value_variable: id has no match in FactBase"),
            Some(r) => r.clone(),
        }
    }
}

///Verificatiob
impl FactBase {
    fn is_const_variable(&self, sym: &Sym) -> bool {
        self.get_value_variable(sym).1
    }

    fn is_const_state_variable(&self, params: &Vec<Sym>) -> Result<bool, FactBaseError> {
        Ok(self.get_value_state_variable(params).1)
    }

    #[allow(dead_code)]
    fn is_value_of_type(&self, value: &Sym, type_id: &SymId) -> bool {
        self.symbol_table.is_value_of_type(value, type_id)
    }

    fn check_value_and_type(&self, value: &mut Sym, type_id: &SymId) -> Result<(), FactBaseError> {
        let good_type = match self.get_sym_id(value) {
            Ok(s) => {
                //println!("{} is a symbol", value);
                if self.get_type_id(&s).unwrap() == *type_id {
                    if self.get_fact_type(&s) == FactType::Variable {
                        //print!("{} is a variable, ", value);
                        *value = match self.get_value_variable(value).0 {
                            None => return Err(FactBaseError::MissExpectedSymbol("value".to_string(),"none".to_string())),
                            Some(v) => v
                        };
                        //println!("the corresponding value is {}.", value);
                    }
                    true
                } else {
                    false
                }
            }
            Err(e) => match usize::from(*type_id) {
                TYPE_INT_ID => {
                    //println!("The symbol {} should be an int", value);
                    match value.clone().as_str().parse::<u64>() {
                        Ok(_) => true,
                        Err(_) => false,
                    }
                }
                TYPE_BOOL_ID => {
                    //println!("The symbol {} should be a boolean", value);
                    match value.clone().as_str() {
                        TRUE | FALSE => true,
                        _ => false,
                    }
                }

                _ => return Err(e),
            },
        };
        match good_type {
            true => Ok(()),
            false => Err(WrongType(
                value.to_string(),
                self.get_sym(type_id).unwrap().to_string(),
            )),
        }
    }

    fn check_value_and_type_sv(&self, mut fact: ListIter, sf: &CustomStateFun, sv: &mut Vec<Sym>) -> Result<(), FactBaseError> {
        let n_type = sf.tpe.len();
        if fact.len() == n_type {
            for type_id in &sf.tpe {
                let mut param_sym: Sym = fact.pop_atom()?.as_str().into();
                self.check_value_and_type(&mut param_sym, type_id)?;
                sv.push(param_sym);
            }
        } else {
            return Err(WrongNumberOfArgument(
                list_iter_to_string(fact),
                sf.tpe.len() as u64,
            ));
        }
        Ok(())
    }

    fn infer_type(&self, sym: &Sym) -> Result<Option<SymId>, FactBaseError> {
        match self.symbol_table.is_symbol_defined(sym){
            true => {
                let sym_id = self.get_sym_id(sym)?;
                Ok(self.get_type_id(&sym_id))
            }
            false => {
                match sym.as_str() {
                    TRUE | FALSE => Ok(Some(SymId::from(TYPE_BOOL_ID))),
                    s => match s.parse::<u64>() {
                        Ok(_) => Ok(Some(SymId::from(TYPE_INT_ID))),
                        Err(_) => Ok(None)
                    }
                }
            }
        }
    }
}
///Setters
impl FactBase {
    fn set_value_variable(&mut self, key: Sym, value: FactBaseValue) {
        self.variables.insert(key, value);
    }

    fn set_value_state_variable(&mut self, key: Vec<Sym>, value: FactBaseValue) {
        self.state_variables.insert(key, value);
    }
}

///Deletters
impl FactBase {
    fn delete_symbol(&mut self, sym: &Sym) -> FactBaseResult {
        self.symbol_table.delete_symbol(sym)
    }
}

fn vec_sym_to_string(sv: Vec<Sym>) -> String {
    let mut string = String::new();
    if sv.len() < 2 {
        panic!("Strong error in vec_sym_to_string")
    }
    string.push_str(format!("{}[", sv[0].to_string()).as_str());
    let mut first = true;
    for param in &sv[1..] {
        string.push_str(param.as_str());
        if first {
            first = false;
        } else {
            string.push(',');
        }
    }
    string.push_str("]");
    string
}

fn list_iter_to_string(mut list : ListIter) -> String {
    let mut string = String::new();
    while !list.is_empty() {
        match list.pop_atom() {
            Ok(a) => string.push_str(a.as_str()),
            Err(e) => panic!("{}", e)
        }
    }
    string
}

impl Display for FactBase {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        let mut r = String::new();
        r.push_str("\n#VARIABLES: ");
        if !self.variables.is_empty() {
            r.push('\n');
            for (var, value) in &self.variables {
                let sym_id = self.get_sym_id(var).unwrap();
                let type_id = self.get_type_id(&sym_id).unwrap();
                let sym_type = self.get_sym(&type_id).unwrap();
                r.push_str(format!("-{}({})", var, sym_type).as_str());
                match &value.0 {
                    None => {}
                    Some(s) => r.push_str(format!(" = {}", s).as_str()),
                };
                if value.1 {
                    r.push_str(" (const)")
                }
                r.push('\n');
            }
        } else {
            r.push_str(EMPTY);
        }
        r.push_str("\n#STATE-VARIABLES: ");
        if !self.state_variables.is_empty() {
            r.push('\n');
            for (params, value) in &self.state_variables {
                let mut params = params.clone();
                r.push_str(format!("-{}[", params.remove(0)).as_str());
                for (index, param) in params.iter().enumerate() {
                    if index > 0 {
                        r.push(',');
                    }
                    r.push_str(param.as_str());
                }
                r.push_str(format!("]={}", value.0.as_ref().unwrap()).as_str());
                if value.1 {
                    r.push_str("  (const)");
                }
                r.push('\n');
            }
        } else {
            r.push_str(EMPTY);
        }

        r.push_str(format!("{}", self.symbol_table).as_str());
        write!(f, "{}", r)
    }
}
