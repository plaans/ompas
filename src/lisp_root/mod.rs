use aries_utils::input::Sym;
use im::HashMap;
use std::borrow::Borrow;
use std::fs::File;
use std::io::Write;
use std::rc::Rc;
use aries_planning::parsing::sexpr::SExpr;
use std::any::Any;
use crate::lisp_root::lisp_struct::*;
use crate::lisp_root::lisp_functions::*;
use crate::lisp_root::lisp_language::*;
use crate::lisp_root::lisp_struct::LError::*;
use crate::lisp_root::lisp_struct::NameTypeLValue::{List, Symbol};
use crate::lisp_root::lisp_struct::LCoreOperator::Quote;

pub mod lisp_functions;
pub mod lisp_language;
pub mod lisp_struct;

pub struct LEnv {
    symbols: HashMap<String, LValue>,
    sym_types: HashMap<Sym, LSymType>,
    macro_table: HashMap<Sym, LLambda>,
    new_entries: Vec<String>,
    outer: Option<Rc<LEnv>>,
    others: HashMap<Sym, Box<dyn Any>>
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Default for LEnv {
    fn default() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut symbols: HashMap<String, LValue> = HashMap::default();
        let mut sym_types: HashMap<Sym, LSymType> = HashMap::default();

        //Core Operators
        symbols.insert(DEFINE.to_string(), LCoreOperator::Define.into());
        symbols.insert(IF.to_string(), LCoreOperator::If.into());
        symbols.insert(LAMBDA.to_string(), LCoreOperator::DefLambda.into());
        symbols.insert(DEF_MACRO.to_string(), LCoreOperator::DefMacro.into());
        symbols.insert(SET.to_string(), LCoreOperator::Set.into());
        symbols.insert(BEGIN.to_string(), LCoreOperator::Begin.into());
        symbols.insert(QUASI_QUOTE.to_string(), LCoreOperator::QuasiQuote.into());
        symbols.insert(QUOTE.to_string(), LCoreOperator::Quote.into());
        symbols.insert(UNQUOTE.to_string(), LCoreOperator::UnQuote.into());

        //Mathematical functions
        symbols.insert(ADD.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(add),
            label: ADD.to_string(),
        }));
        symbols.insert(SUB.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(sub),
            label: SUB.to_string(),
        }));
        symbols.insert(MUL.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(mul),
            label: MUL.to_string(),
        }));
        symbols.insert(DIV.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(div),
            label: DIV.to_string(),
        }));
        //Comparison
        symbols.insert(GT.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(gt),
            label: GT.to_string(),
        }));
        symbols.insert(LT.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(lt),
            label: LT.to_string(),
        }));
        symbols.insert(GE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(ge),
            label: GE.to_string(),
        }));
        symbols.insert(LE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(le),
            label: LE.to_string(),
        }));
        symbols.insert(EQ.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(eq),
            label: ADD.to_string(),
        }));

        //Type verification
        symbols.insert(IS_NONE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_none),
            label: IS_NONE.to_string(),
        }));
        symbols.insert(IS_NUMBER.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_number),
            label: IS_NUMBER.to_string(),
        }));
        symbols.insert(IS_BOOL.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_bool),
            label: IS_BOOL.to_string(),
        }));
        symbols.insert(IS_FN.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_fn),
            label: IS_FN.to_string(),
        }));
        symbols.insert(
            IS_STATE_FUNCTION.to_string(),
            LValue::LFn(LFn {
                pointer: Rc::new(is_state_function),
                label: IS_STATE_FUNCTION.to_string(),
            }));

        symbols.insert(IS_OBJECT.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_object),
            label: IS_OBJECT.to_string(),
        }));
        symbols.insert(IS_TYPE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_type),
            label: IS_TYPE.to_string(),
        }));
        symbols.insert(IS_MAP.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_map),
            label: IS_MAP.to_string(),
        }));
        symbols.insert(IS_LIST.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_list),
            label: IS_LIST.to_string(),
        }));
        symbols.insert(IS_LAMBDA.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_lambda),
            label: IS_LAMBDA.to_string(),
        }));
        symbols.insert(IS_QUOTE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(is_quote),
            label: IS_QUOTE.to_string(),
        }));

        //Special entry
        symbols.insert(GET.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(get),
            label: GET.to_string(),
        }));
        symbols.insert(GET_TYPE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(get_type),
            label: GET_TYPE.to_string(),
        }));


        //Logical functions : will be added as macros
        //symbols.insert(AND.to_string(), LValue::LFn(Rc::new(and)));
        //symbols.insert(OR.to_string(), LValue::LFn(Rc::new(or)));
        //symbols.insert(NOT.to_string(), LValue::LFn(Rc::new(not)));



        //Basic types

        //Functions for the factbase
        symbols.insert(
            STATE_FUNCTION.to_string(),
            LValue::LFn(LFn {
                pointer: Rc::new(state_function),
                label: STATE_FUNCTION.to_string(),
            }));
        symbols.insert(SUBTYPE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(subtype),
            label: SUBTYPE.to_string(),
        }));
        symbols.insert(MAP.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(map),
            label: MAP.to_string(),
        }));
        symbols.insert(LIST.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(list),
            label: LIST.to_string(),
        }));
        //State is an alias for map
        symbols.insert(STATE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(map),
            label: STATE.to_string(),
        }));
        symbols.insert(TYPEOF.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(type_of),
            label: TYPEOF.to_string(),
        }));
        symbols.insert(READ.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(read),
            label: READ.to_string(),
        }));
        symbols.insert(WRITE.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(write),
            label: WRITE.to_string(),
        }));
        //symbols.insert(QUOTE.to_string(), LValue::LFn(Rc::new(quote)));
        symbols.insert(PRINT.to_string(), LValue::LFn(LFn {
            pointer: Rc::new(print),
            label: PRINT.to_string(),
        }));
        //Sym_types

        /*
        * LIST FUNCTIONS
        */
        symbols.insert(CAR.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(car),
            label: CAR.to_string(),
        }));

        symbols.insert(CDR.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(cdr),
            label: CDR.to_string(),
        }));

        symbols.insert(CONS.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(cons),
            label: CONS.to_string(),
        }));

        symbols.insert(APPEND.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(append),
            label: APPEND.to_string(),
        }));

        symbols.insert(MEMBER.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(member),
            label: MEMBER.to_string(),
        }));

        symbols.insert(REVERSE.to_string(), LValue::LFn( LFn{
            pointer: Rc::new(reverse),
            label: REVERSE.to_string(),
        }));



        sym_types.insert(
            TYPE_INT.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_FLOAT.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_BOOL.into(),
            LSymType::Type(None),
        );
        sym_types.insert(
            TYPE_OBJECT.into(),
            LSymType::Type(None),
        );

        symbols.insert(
            PI.to_string(),
            LValue::Number(LNumber::Float(std::f64::consts::PI)),
        );

        //TODO: add the macros defined in a predefined file

        Self {
            symbols,
            sym_types,
            macro_table: Default::default(),
            new_entries: vec![],
            outer: None,
            others: Default::default(),
        }
    }
}

impl LEnv {
    pub fn new_ref_counter() -> Rc<Self> {
        Rc::new(Self::default())
    }

    pub fn new_ref_counter_from_outer(outer: &Rc<LEnv>) -> Rc<Self> {
        Rc::new(LEnv {
            symbols: Default::default(),
            sym_types: Default::default(),
            macro_table: Default::default(),
            new_entries: vec![],
            outer: Some(outer.clone()),
            others: Default::default()
        })
    }
    
    pub fn new_empty_ref_counter() -> Rc<Self> {
        Rc::new(LEnv {
            symbols: Default::default(),
            sym_types: Default::default(),
            macro_table: Default::default(),
            new_entries: vec![],
            outer: None,
            others: Default::default()
        })
    }


    pub fn find(&self, var: &Sym) -> Option<&Self> {
        match self.symbols.get(var.as_str()) {
            None => match self.outer.borrow() {
                None => None,
                Some(env) => env.find(var),
            },
            Some(_) => Some(self),
        }
    }

    pub fn get_symbol(&self, s: &str) -> Option<LValue> {
        match self.symbols.get(s) {
            None => match self.outer.borrow() {
                None => None,
                Some(env) => env.get_symbol(s),
            },
            Some(v) => Some(v.clone()),
        }
    }

    pub fn get_sym_type(&self, sym: &Sym) -> Option<&LSymType> {
        self.sym_types.get(sym)
    }

    pub fn add_entry(&mut self, key: String, exp: LValue) {
        self.symbols.insert(key.clone(), exp);
        self.new_entries.push(key);
    }

    pub fn add_sym_type(&mut self, sym: Sym, sym_type: LSymType) {
        self.sym_types.insert(sym, sym_type);
    }

    pub fn add_macro(&mut self, key: Sym, _macro: LLambda) {
        self.macro_table.insert(key, _macro);
    }

    pub fn get_macro(&self, key: &Sym) -> Option<&LLambda>  {
        self.macro_table.get(key)
    }

    pub fn get_new_entries(&self) -> Vec<String> {
        self.new_entries.clone()
    }

    pub fn to_file(&self, name_file: String) {
        let mut file = File::create(name_file).unwrap();
        let mut string = String::new();
        string.push_str("(begin \n");
        for key in &self.new_entries {
            let value = self.get_symbol(key).unwrap_or(LValue::None);
            match value {
                LValue::Symbol(s) => string.push_str(
                    format!("(typeof {} {})",
                            s,
                            self.get_sym_type(&s).unwrap_or(&LSymType::Object(LType::Object)).as_command())
                        .as_str()),
                lv => string.push_str(format!("(define {} {})", key, lv.as_command()).as_str()),
            }
        }

        string.push(')');
        match file.write_all(string.as_bytes()) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        }
        //eprintln!("write fact base to file");
    }

    pub fn load_module(&mut self, module: Module) {
        /*for (sym, nl) in module.prelude {
            self.symbols.insert(sym.to_string(), LValue::(nl));
        }*/
        unimplemented!()
    }

    pub fn get_context(&self) -> &dyn NativeContext {
        unimplemented!()
    }

    pub fn get_mut_context(&self) -> &mut dyn NativeContext {
        unimplemented!()
    }
}

pub fn parse(str : &str, env: &mut Rc<LEnv>) -> Result<LValue, LError> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => {
            expand(&parse_into_lvalue(&se, env)?, true, env)
        }
        Err(e) => Err(SpecialError(format!("Error in command: {}", e.to_string()))),
    }
}

pub fn parse_into_lvalue(se: &SExpr, env: &mut Rc<LEnv>) -> Result<LValue,LError> {
    match se {
        SExpr::Atom(atom) => {
            //println!("expression is an atom: {}", atom);
            //Test if its an int
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValue::Number(LNumber::Int(int))),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValue::Number(LNumber::Float(float))),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            Ok(LValue::Bool(true))
                        }
                        FALSE => {
                            //println!("atom is boolean false");
                            Ok(LValue::Bool(false))
                        }

                        s => match env.get_symbol(s){
                            None => Ok(LValue::Symbol(s.into())),
                            Some(s) => Ok(s)
                        }
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let mut vec_lvalue = Vec::new();

            for element in list.iter() {
                vec_lvalue.push(parse_into_lvalue(element, env)?);
            }
            Ok(LValue::List(vec_lvalue))
        }
    }
}

pub fn expand(x: &LValue, top_level: bool, env: &mut Rc<LEnv>) -> Result<LValue, LError> {
    match x {
        LValue::List(list) => {
            match list.first().unwrap() {
                LValue::CoreOperator(co) => match co {
                    LCoreOperator::Define | LCoreOperator::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(WrongNumberOfArgument(x.clone(), list.len(), 3..std::usize::MAX));
                        }
                        let def = list.get(0).unwrap().as_core_operator()?;
                        let v = list.get(1).unwrap();
                        let body = &list[1..];
                        match v {
                            LValue::List(v_list) => {
                                if v_list.len() >= 2 {
                                    let f = v_list.get(0).unwrap();
                                    let args = &v_list[1..];
                                    let mut new_body = Vec::new();
                                    new_body.push(LCoreOperator::DefLambda.into());
                                    new_body.append(&mut args.to_vec());
                                    new_body.append(&mut body.to_vec());
                                    return expand(&vec![def.into(), f.clone(), new_body.into()].into(), top_level, env);
                                }
                            },
                            LValue::Symbol(sym) => {
                                if list.len() != 3 {
                                    return Err(WrongNumberOfArgument(x.clone(), list.len(), 3..3))
                                }
                                let exp = expand(list.get(2).unwrap(), top_level, env)?;
                                if def == LCoreOperator::DefMacro {
                                    if !top_level {
                                        return Err(SpecialError(format!("{}: defmacro only allowed at top level", x)))
                                    }
                                    let proc = eval(&exp, &mut LEnv::new_empty_ref_counter())?;
                                    if !matches!(proc, LValue::Lambda(_)) {
                                        return Err(SpecialError(format!("{}: macro must be a procedure", proc)))
                                    } else {
                                        env.add_macro(sym.clone(), proc.as_lambda()?);
                                    }
                                    //Add to macro_table
                                    return Ok(LValue::None)
                                }
                                //We add to the list the expanded body
                                return Ok(vec![LCoreOperator::Define.into(), v.clone(), exp.clone()].into())
                            }
                            lv => return Err(WrongType(x.clone(), x.into(), NameTypeLValue::Symbol))
                        }
                    },
                    LCoreOperator::DefLambda => {
                        if list.len() < 3 {
                            return Err(WrongNumberOfArgument(x.clone(), list.len(), 3..std::usize::MAX))
                        }
                        let vars = list.get(1).unwrap();
                        let body = &list[2..];
                        //Verification of the types of the arguments
                        match vars {
                            LValue::List(vars_list) => {
                                for v in vars_list {
                                    if !matches!(v, LValue::Symbol(_)) {
                                        return Err(SpecialError("illegal lambda argument list".to_string()))
                                    }
                                }
                            },
                            LValue::Symbol(_) => {}
                            lv => return Err(NotInListOfExpectedTypes(lv.clone(), lv.into(), vec![List, Symbol]))
                        }
                        let exp = if body.len() == 1 {
                            body.get(0).unwrap().clone()
                        } else {
                            let mut vec = vec![LCoreOperator::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(vec).to_owned()
                        };
                        return Ok(vec![LCoreOperator::DefLambda.into(), vars.clone(), expand(&exp, top_level, env)?].into())
                    },
                    LCoreOperator::If => {
                        let mut list = list.clone();
                        if list.len() == 3 {
                            list.push(LValue::None);
                        }
                        if list.len() != 4 {
                            return Err(WrongNumberOfArgument((&list).into(), list.len(), 4..4))
                        }
                        //return map(expand, x)
                        let mut list_expanded = Vec::new();
                        for element in list {
                            list_expanded.push(expand(&element, false, env)?)
                        }
                        return Ok(list_expanded.into())
                    },
                    LCoreOperator::Quote => {
                        eprintln!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(WrongNumberOfArgument(list.into(), list.len(), 2..2))
                        }
                        return Ok(x.clone())
                    },
                    LCoreOperator::Set => {
                        if list.len() != 3 {
                            return Err(WrongNumberOfArgument(list.into(), list.len(), 3..3))
                        }
                        let var = list.get(1).unwrap();
                        //Can only set a symbol
                        if !matches!(var, LValue::Symbol(_s)) {
                            return Err(WrongType(var.clone(), var.into(), NameTypeLValue::Symbol))
                        }
                        let mut return_list = list.clone();
                        //We expand only the last element
                        return_list[2] = expand(return_list.get(2).unwrap(), false, env)?;
                        return Ok(return_list.into())
                    },
                    LCoreOperator::Begin => {
                        return if list.len() == 1 {
                            Ok(LValue::None)
                        } else {
                            let mut expanded_list = Vec::new();
                            for xi in list {
                                expanded_list.push(expand(xi, top_level, env)?);
                            }
                            Ok(expanded_list.into())
                        }
                    },
                    LCoreOperator::QuasiQuote => {
                        return if list.len() != 2 {
                            Err(WrongNumberOfArgument(list.into(), list.len(), 2..2))
                        } else {
                            expand_quasi_quote(list.get(1).unwrap(), env)
                        }
                    },
                    LCoreOperator::UnQuote => {}
                }
                LValue::Symbol(sym) => {
                    match env.get_macro(sym) {
                        None => {}
                        Some(m) => {
                            return expand(&m.call(&list[1..], env)?, top_level, env)
                        }
                    }
                    /*elif isa(x[0], Symbol) and x[0] in macro_table:
                        return expand(macro_table[x[0]](*x[1:]), toplevel) # (m arg...)
                        */
                }
                _ => {}
            }
            let mut expanded_list = Vec::new();
            for val in list {
                expanded_list.push(expand(val, false, env)?)
            }
            Ok(expanded_list.into())
        },
        LValue::None => Err(SpecialError(format!("Not expecting a none value"))),
        lv => Ok(lv.clone())
    }
}

pub fn expand_quasi_quote(x: &LValue, env: &mut Rc<LEnv>) -> Result<LValue, LError> {
    /*"""Expand `x => 'x; `,x => x; `(,@x y) => (append x y) """
    if not is_pair(x):
    return [_quote, x]
    require(x, x[0] is not _unquotesplicing, "can't splice here")
    if x[0] is _unquote:
        require(x, len(x)==2)
    return x[1]

    return [_append, x[0][1], expand_quasiquote(x[1:])]
    else:
    return [_cons, expand_quasiquote(x[0]), expand_quasiquote(x[1:])]*/

    match x {
        LValue::List(list) => {
            if list.is_empty() {
                Ok(vec![Quote.into(), x.clone()].into())
            }else {
                let first = list.first().unwrap();
                return if matches!(first, LValue::CoreOperator(LCoreOperator::UnQuote)) {
                    if list.len() != 2 {
                        return Err(WrongNumberOfArgument(x.clone(),list.len(),2..2))
                    }
                    Ok(list.get(1).unwrap().clone())
                }/*elif is_pair(x[0]) and x[0][0] is _unquotesplicing:
                    require(x[0], len(x[0])==2)*/
                else {
                    Ok(vec![env.get_symbol(CONS).unwrap(), expand_quasi_quote(&first, env)?,
                                   expand_quasi_quote(&list[1..].to_vec().into(),env)?].into())
                }
            }
        }
        _ => Ok(vec![Quote.into(), x.clone()].into())
    }
    //Verify if has unquotesplicing here

}


///Signal a syntax error if predicate is false."
pub fn require(x: &LValue, predicate: bool, msg: String) -> Result<(), LError> {
    return if !predicate {
        Err(SpecialError(format!("{}: {}", x.to_string(), msg)))
    }
    else {
        Ok(())
    }
}

pub fn eval(lv: &LValue, env: &mut Rc<LEnv>) -> Result<LValue, LError> {
    match lv {
        LValue::List(list) => {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = list.get(0).unwrap();
            let args = &list[1..];
            match proc {
                LValue::CoreOperator(co) => match co {
                    LCoreOperator::Define => {
                        match args.get(0).unwrap() {
                            LValue::Symbol(s) =>  {
                                let exp = eval(args.get(1).unwrap(), env)?;
                                env.add_entry(s.to_string(), exp);
                            }
                            lv => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
                        }
                        Ok(LValue::None)
                    }
                    LCoreOperator::DefLambda => {
                        let params = match args.get(0).unwrap() {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv  => return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Symbol))
                                    }
                                }
                                vec_sym
                            }
                            lv => return Err(WrongType(lv.clone(),lv.into(), NameTypeLValue::List))
                        };
                        let body = args.get(1).unwrap();
                        Ok(LValue::Lambda(LLambda::new(
                            params,
                            body.clone(),
                            env,
                        )))
                    }
                    LCoreOperator::If => {
                        let test = args.get(0).unwrap();
                        let conseq = args.get(1).unwrap();
                        let alt = args.get(2).unwrap();
                        match eval(test, env) {
                            Ok(LValue::Bool(true)) => eval(conseq, env),
                            Ok(LValue::Bool(false)) => eval(alt, env),
                            Ok(lv) => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Bool)),
                            Err(e) => Err(e),
                        }
                    }
                    LCoreOperator::Quote => {
                        return Ok(args.first().unwrap().clone())
                    }
                    LCoreOperator::Set => {
                        //TODO: implement set
                        Ok(LValue::None)
                    }
                    LCoreOperator::Begin => {
                        for (k,exp) in args[1..].iter().enumerate() {
                            let result = eval(exp, env)?;
                            if k == args.len() {
                                return Ok(result)
                            }
                        }
                        Ok(LValue::None)
                    }
                    LCoreOperator::QuasiQuote | LCoreOperator::UnQuote | LCoreOperator::DefMacro => {
                        Ok(LValue::None)
                    }
                }
                LValue::LFn(f) => {
                    let mut arg_evaluated = Vec::new();
                    for arg in args {
                        arg_evaluated.push(eval(arg, env)?)
                    }
                    return (f.pointer)(arg_evaluated.as_slice(), env)
                },
                LValue::Lambda(l) => {
                    l.call(args, env)
                    /*let mut new_env = l.get_new_env(args, env)?;
                    eval(&l.get_body(), &mut new_env)*/
                }
                LValue::NativeLambda(fun) => {
                    let args :Vec<LValue> = args.iter().map(|a| eval(a, env)).collect::<Result<_,_>>()?;
                    fun.call(&args, env.get_context())
                }
                lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::LFn))
            }
        }
        LValue::Symbol(s) => {
            match env.get_symbol(s.as_str()) {
                None => Ok(lv.clone()),
                Some(lv) => Ok(lv)
            }
        }
        lv => Ok(lv.clone())
    }
}

//(begin (define ?v (var (:type object
//                        :value bob))
