use crate::core::functions::*;
use crate::core::language::*;
use crate::core::lisp_as_literal::AsLiteral;
use crate::core::structs::LCoreOperator::Quote;
use crate::core::structs::LError::*;
use crate::core::structs::NameTypeLValue::{List, Symbol};
use crate::core::structs::*;
use aries_planning::parsing::sexpr::SExpr;
use aries_utils::input::Sym;
use im::HashMap;
use std::any::Any;
use std::borrow::Borrow;
use std::fs::File;
use std::io::Write;
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub mod functions;
pub mod language;
pub mod lisp_as_literal;
pub mod structs;

pub struct LEnv {
    symbols: HashMap<String, LValue>,
    macro_table: HashMap<Sym, LLambda>,
    new_entries: Vec<String>,
    outer: Option<RefLEnv>,
}

pub struct ContextCollection(Vec<Box<dyn Any>>);

impl Default for ContextCollection {
    fn default() -> Self {
        Self(vec![])
    }
}

type CtxCollec = ContextCollection;

impl ContextCollection {
    pub fn insert(&mut self, ctx: Box<dyn Any>) -> usize {
        self.0.push(ctx);
        self.0.len() - 1
    }

    pub fn get_context(&self, id: usize) -> &dyn Any {
        self.0.get(id).unwrap().deref()
    }

    pub fn get_mut_context(&mut self, id: usize) -> &mut dyn Any {
        self.0.get_mut(id).unwrap().deref_mut()
    }
}

#[derive(Clone)]
pub struct RefLEnv(Rc<LEnv>);

impl Default for RefLEnv {
    fn default() -> Self {
        RefLEnv(Rc::new(LEnv::default()))
    }
}

impl RefLEnv {
    pub fn root() -> Self {
        RefLEnv(Rc::new(LEnv::root()))
    }

    pub fn new(env: LEnv) -> Self {
        RefLEnv(Rc::new(env))
    }

    pub fn new_from_outer(outer: RefLEnv) -> Self {
        RefLEnv(Rc::new(LEnv {
            symbols: Default::default(),
            macro_table: Default::default(),
            new_entries: vec![],
            outer: Some(outer),
        }))
    }

    pub fn empty() -> Self {
        RefLEnv(Rc::new(LEnv::empty()))
    }
}

impl Deref for RefLEnv {
    type Target = LEnv;

    fn deref(&self) -> &Self::Target {
        &(self.0)
    }
}

impl DerefMut for RefLEnv {
    fn deref_mut(&mut self) -> &mut Self::Target {
        Rc::get_mut(&mut self.0).unwrap()
    }
}

impl PartialEq for LEnv {
    fn eq(&self, other: &Self) -> bool {
        self == other
    }
}

impl Default for LEnv {
    fn default() -> Self {
        Self::empty()
    }
}

impl LEnv {
    pub fn root() -> Self {
        // let map = im::hashmap::HashMap::new();
        // map.ins
        let mut symbols: HashMap<String, LValue> = HashMap::default();

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

        //Special entry
        symbols.insert(
            GET.to_string(),
            LValue::Fn(LFn::new(Box::new(get), GET.to_string())),
        );

        symbols.insert(
            MAP.to_string(),
            LValue::Fn(LFn::new(Box::new(map), MAP.to_string())),
        );
        symbols.insert(
            LIST.to_string(),
            LValue::Fn(LFn::new(Box::new(list), LIST.to_string())),
        );
        //State is an alias for map

        /*
         * LIST FUNCTIONS
         */
        symbols.insert(
            CAR.to_string(),
            LValue::Fn(LFn::new(Box::new(car), CAR.to_string())),
        );

        symbols.insert(
            CDR.to_string(),
            LValue::Fn(LFn::new(Box::new(cdr), CDR.to_string())),
        );

        symbols.insert(
            CONS.to_string(),
            LValue::Fn(LFn::new(Box::new(cons), CONS.to_string())),
        );

        symbols.insert(
            APPEND.to_string(),
            LValue::Fn(LFn::new(Box::new(append), APPEND.to_string())),
        );

        symbols.insert(
            MEMBER.to_string(),
            LValue::Fn(LFn::new(Box::new(member), MEMBER.to_string())),
        );

        symbols.insert(
            REVERSE.to_string(),
            LValue::Fn(LFn::new(Box::new(reverse), REVERSE.to_string())),
        );

        //TODO: Add a function to import files in a predefined file
        Self {
            symbols,
            macro_table: Default::default(),
            new_entries: vec![],
            outer: None,
        }
    }

    pub fn empty() -> Self {
        LEnv {
            symbols: Default::default(),
            macro_table: Default::default(),
            new_entries: vec![],
            outer: None,
        }
    }

    pub fn new_with_outer(outer: Option<RefLEnv>) -> Self {
        LEnv {
            symbols: Default::default(),
            macro_table: Default::default(),
            new_entries: vec![],
            outer,
        }
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

    pub fn add_entry(&mut self, key: String, exp: LValue) {
        self.symbols.insert(key.clone(), exp);
        self.new_entries.push(key);
    }

    pub fn add_macro(&mut self, key: Sym, _macro: LLambda) {
        self.macro_table.insert(key, _macro);
    }

    pub fn get_macro(&self, key: &Sym) -> Option<&LLambda> {
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
            string.push_str(format!("(define {} {})", key, value.as_command()).as_str());
        }

        string.push(')');
        match file.write_all(string.as_bytes()) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        }
        //eprintln!("write fact base to file");
    }
}

pub fn load_module(env: &mut RefLEnv, ctxs: &mut ContextCollection, mut module: Module) -> usize {
    let id = ctxs.insert(module.ctx);
    for (sym, lv) in &mut module.prelude {
        match lv {
            LValue::Fn(nl) => nl.set_index_mod(id),
            LValue::MutFn(nml) => nml.set_index_mod(id),
            _ => {}
        }
        env.symbols.insert(sym.to_string(), lv.clone());
    }
    id
}

pub fn parse(str: &str, env: &mut RefLEnv, ctxs: &mut CtxCollec) -> Result<LValue, LError> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => expand(&parse_into_lvalue(&se, env, ctxs)?, true, env, ctxs),
        Err(e) => Err(SpecialError(format!("Error in command: {}", e.to_string()))),
    }
}

pub fn parse_into_lvalue(
    se: &SExpr,
    env: &mut RefLEnv,
    ctxs: &mut CtxCollec,
) -> Result<LValue, LError> {
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

                        s => match env.get_symbol(s) {
                            None => Ok(LValue::Symbol(s.into())),
                            Some(s) => Ok(s),
                        },
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let mut vec_lvalue = Vec::new();

            for element in list.iter() {
                vec_lvalue.push(parse_into_lvalue(element, env, ctxs)?);
            }
            Ok(LValue::List(vec_lvalue))
        }
    }
}

pub fn expand(
    x: &LValue,
    top_level: bool,
    env: &mut RefLEnv,
    ctxs: &mut CtxCollec,
) -> Result<LValue, LError> {
    match x {
        LValue::List(list) => {
            match list.first().unwrap() {
                LValue::CoreOperator(co) => match co {
                    LCoreOperator::Define | LCoreOperator::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(WrongNumberOfArgument(
                                x.clone(),
                                list.len(),
                                3..std::usize::MAX,
                            ));
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
                                    return expand(
                                        &vec![def.into(), f.clone(), new_body.into()].into(),
                                        top_level,
                                        env,
                                        ctxs,
                                    );
                                }
                            }
                            LValue::Symbol(sym) => {
                                if list.len() != 3 {
                                    return Err(WrongNumberOfArgument(x.clone(), list.len(), 3..3));
                                }
                                let exp = expand(list.get(2).unwrap(), top_level, env, ctxs)?;
                                if def == LCoreOperator::DefMacro {
                                    if !top_level {
                                        return Err(SpecialError(format!(
                                            "{}: defmacro only allowed at top level",
                                            x
                                        )));
                                    }
                                    let proc = eval(
                                        &exp,
                                        &mut RefLEnv::new_from_outer(env.clone()),
                                        ctxs,
                                    )?;
                                    if !matches!(proc, LValue::Lambda(_)) {
                                        return Err(SpecialError(format!(
                                            "{}: macro must be a procedure",
                                            proc
                                        )));
                                    } else {
                                        env.add_macro(sym.clone(), proc.as_lambda()?);
                                    }
                                    //Add to macro_table
                                    return Ok(LValue::None);
                                }
                                //We add to the list the expanded body
                                return Ok(
                                    vec![LCoreOperator::Define.into(), v.clone(), exp].into()
                                );
                            }
                            _ => {
                                return Err(WrongType(x.clone(), x.into(), NameTypeLValue::Symbol))
                            }
                        }
                    }
                    LCoreOperator::DefLambda => {
                        if list.len() < 3 {
                            return Err(WrongNumberOfArgument(
                                x.clone(),
                                list.len(),
                                3..std::usize::MAX,
                            ));
                        }
                        let vars = list.get(1).unwrap();
                        let body = &list[2..];
                        //Verification of the types of the arguments
                        match vars {
                            LValue::List(vars_list) => {
                                for v in vars_list {
                                    if !matches!(v, LValue::Symbol(_)) {
                                        return Err(SpecialError(
                                            "illegal lambda argument list".to_string(),
                                        ));
                                    }
                                }
                            }
                            LValue::Symbol(_) => {}
                            lv => {
                                return Err(NotInListOfExpectedTypes(
                                    lv.clone(),
                                    lv.into(),
                                    vec![List, Symbol],
                                ))
                            }
                        }
                        let exp = if body.len() == 1 {
                            body.get(0).unwrap().clone()
                        } else {
                            let mut vec = vec![LCoreOperator::Begin.into()];
                            vec.append(&mut body.to_vec());
                            LValue::List(vec)
                        };
                        return Ok(vec![
                            LCoreOperator::DefLambda.into(),
                            vars.clone(),
                            expand(&exp, top_level, env, ctxs)?,
                        ]
                        .into());
                    }
                    LCoreOperator::If => {
                        let mut list = list.clone();
                        if list.len() == 3 {
                            list.push(LValue::None);
                        }
                        if list.len() != 4 {
                            return Err(WrongNumberOfArgument((&list).into(), list.len(), 4..4));
                        }
                        //return map(expand, x)
                        let mut list_expanded = Vec::new();
                        for element in list {
                            list_expanded.push(expand(&element, false, env, ctxs)?)
                        }
                        return Ok(list_expanded.into());
                    }
                    LCoreOperator::Quote => {
                        //eprintln!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(WrongNumberOfArgument(list.into(), list.len(), 2..2));
                        }
                        return Ok(x.clone());
                    }
                    LCoreOperator::Set => {
                        if list.len() != 3 {
                            return Err(WrongNumberOfArgument(list.into(), list.len(), 3..3));
                        }
                        let var = list.get(1).unwrap();
                        //Can only set a symbol
                        if !matches!(var, LValue::Symbol(_s)) {
                            return Err(WrongType(var.clone(), var.into(), NameTypeLValue::Symbol));
                        }
                        let mut return_list = list.clone();
                        //We expand only the last element
                        return_list[2] = expand(return_list.get(2).unwrap(), false, env, ctxs)?;
                        return Ok(return_list.into());
                    }
                    LCoreOperator::Begin => {
                        return if list.len() == 1 {
                            Ok(LValue::None)
                        } else {
                            let mut expanded_list = Vec::new();
                            for xi in list {
                                expanded_list.push(expand(xi, top_level, env, ctxs)?);
                            }
                            Ok(expanded_list.into())
                        }
                    }
                    LCoreOperator::QuasiQuote => {
                        return if list.len() != 2 {
                            Err(WrongNumberOfArgument(list.into(), list.len(), 2..2))
                        } else {
                            expand_quasi_quote(list.get(1).unwrap(), env)
                        }
                    }
                    LCoreOperator::UnQuote => {
                        //TODO: Implémenter msg d'erreur
                        panic!("unquote not at right place")
                    }
                },
                LValue::Symbol(sym) => {
                    match env.get_macro(sym) {
                        None => {}
                        Some(m) => {
                            return expand(&m.call(&list[1..], env, ctxs)?, top_level, env, ctxs)
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
                expanded_list.push(expand(val, false, env, ctxs)?)
            }
            Ok(expanded_list.into())
        }
        LValue::None => Err(SpecialError("Not expecting a none value".to_string())),
        lv => Ok(lv.clone()),
    }
}

pub fn expand_quasi_quote(x: &LValue, env: &mut RefLEnv) -> Result<LValue, LError> {
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
            } else {
                let first = list.first().unwrap();
                return if matches!(first, LValue::CoreOperator(LCoreOperator::UnQuote)) {
                    if list.len() != 2 {
                        return Err(WrongNumberOfArgument(x.clone(), list.len(), 2..2));
                    }
                    Ok(list.get(1).unwrap().clone())
                }
                /*elif is_pair(x[0]) and x[0][0] is _unquotesplicing:
                require(x[0], len(x[0])==2)*/
                else {
                    Ok(vec![
                        env.get_symbol(CONS).unwrap(),
                        expand_quasi_quote(&first, env)?,
                        expand_quasi_quote(&list[1..].to_vec().into(), env)?,
                    ]
                    .into())
                };
            }
        }
        _ => Ok(vec![Quote.into(), x.clone()].into()),
    }
    //Verify if has unquotesplicing here
}

pub fn eval(lv: &LValue, env: &mut RefLEnv, ctxs: &mut CtxCollec) -> Result<LValue, LError> {
    match lv {
        LValue::List(list) => {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            match proc {
                LValue::CoreOperator(co) => match co {
                    LCoreOperator::Define => {
                        match args.get(0).unwrap() {
                            LValue::Symbol(s) => {
                                let exp = eval(&args[1], env, ctxs)?;
                                env.add_entry(s.to_string(), exp);
                            }
                            lv => {
                                return Err(WrongType(
                                    lv.clone(),
                                    lv.into(),
                                    NameTypeLValue::Symbol,
                                ))
                            }
                        };
                        Ok(LValue::None)
                    }
                    LCoreOperator::DefLambda => {
                        let params = match args.get(0).unwrap() {
                            LValue::List(list) => {
                                let mut vec_sym = Vec::new();
                                for val in list {
                                    match val {
                                        LValue::Symbol(s) => vec_sym.push(s.clone()),
                                        lv => {
                                            return Err(WrongType(
                                                lv.clone(),
                                                lv.into(),
                                                NameTypeLValue::Symbol,
                                            ))
                                        }
                                    }
                                }
                                vec_sym
                            }
                            lv => {
                                return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::List))
                            }
                        };
                        let body = args.get(1).unwrap();
                        Ok(LValue::Lambda(LLambda::new(params, body.clone())))
                    }
                    LCoreOperator::If => {
                        let test = args.get(0).unwrap();
                        let conseq = args.get(1).unwrap();
                        let alt = args.get(2).unwrap();
                        match eval(test, env, ctxs) {
                            Ok(LValue::Bool(true)) => eval(conseq, env, ctxs),
                            Ok(LValue::Bool(false)) => eval(alt, env, ctxs),
                            Ok(lv) => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Bool)),
                            Err(e) => Err(e),
                        }
                    }
                    LCoreOperator::Quote => return Ok(args.first().unwrap().clone()),
                    LCoreOperator::Set => {
                        //TODO: implement set
                        Ok(LValue::None)
                    }
                    LCoreOperator::Begin => {
                        for (k, exp) in args[1..].iter().enumerate() {
                            let result = eval(exp, env, ctxs)?;
                            if k == args.len() {
                                return Ok(result);
                            }
                        }
                        Ok(LValue::None)
                    }
                    LCoreOperator::QuasiQuote
                    | LCoreOperator::UnQuote
                    | LCoreOperator::DefMacro => Ok(LValue::None),
                },
                LValue::Lambda(l) => {
                    l.call(args, env, ctxs)
                    /*let mut new_env = l.get_new_env(args, env)?;
                    eval(&l.get_body(), &mut new_env)*/
                }
                LValue::Fn(fun) => {
                    let args: Vec<LValue> = args
                        .iter()
                        .map(|a| eval(a, env, ctxs))
                        .collect::<Result<_, _>>()?;
                    let ctx: &dyn Any = match fun.get_index_mod() {
                        None => &(),
                        Some(u) => ctxs.get_context(u),
                    };
                    fun.call(&args, env, ctx)
                }
                LValue::MutFn(fun) => {
                    let args: Vec<LValue> = args
                        .iter()
                        .map(|a| eval(a, env, ctxs))
                        .collect::<Result<_, _>>()?;
                    match fun.get_index_mod() {
                        None => fun.call(&args, env, &mut ()),
                        Some(u) => fun.call(&args, env, ctxs.get_mut_context(u)),
                    }
                }
                lv => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Fn)),
            }
        }
        LValue::Symbol(s) => match env.get_symbol(s.as_str()) {
            None => Ok(lv.clone()),
            Some(lv) => Ok(lv),
        },
        lv => Ok(lv.clone()),
    }
}

//(begin (define ?v (var (:type object
//                        :value bob))
