use crate::functions::*;
use crate::language::*;
use crate::structs::LCoreOperator::Quote;
use crate::structs::LError::*;
use crate::structs::NameTypeLValue::{List, Symbol};
use crate::structs::*;
use aries_planning::parsing::sexpr::SExpr;
use im::HashMap;
use std::any::Any;
use std::borrow::Borrow;
use std::convert::{TryFrom, TryInto};
use std::ops::{Deref, DerefMut};
use std::rc::Rc;

pub struct LEnv {
    pub(crate) symbols: HashMap<String, LValue>,
    pub(crate) macro_table: HashMap<String, LLambda>,
    pub(crate) new_entries: Vec<String>,
    pub(crate) outer: Option<RefLEnv>,
}

pub struct ContextCollection {
    inner: Vec<Box<dyn Any>>,
    map_label_usize: HashMap<&'static str, usize>,
}

impl Default for ContextCollection {
    fn default() -> Self {
        Self {
            inner: vec![],
            map_label_usize: Default::default(),
        }
    }
}

impl ContextCollection {
    pub fn insert(&mut self, ctx: Box<dyn Any>) -> usize {
        self.inner.push(ctx);
        self.inner.len() - 1
    }

    pub fn get_context(&self, id: usize) -> &dyn Any {
        self.inner.get(id).unwrap().deref()
    }
    pub fn get_context_with_label(&self, label: &str) -> &dyn Any {
        let id = match self.map_label_usize.get(label) {
            None => panic!("no context with such label"),
            Some(s) => *s,
        };

        self.get_context(id)
    }

    pub fn get_mut_context(&mut self, id: usize) -> &mut dyn Any {
        self.inner.get_mut(id).unwrap().deref_mut()
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
    pub fn keys(&self) -> Vec<String> {
        let mut keys: Vec<String> = self.symbols.keys().cloned().collect();
        keys.append(&mut self.macro_table.keys().cloned().collect());
        if let Some(outer) = self.outer.clone() {
            keys.append(&mut outer.keys())
        }
        keys
    }

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

        symbols.insert(NIL.to_string(), LValue::Nil);

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

        symbols.insert(ENV.to_string(), LValue::Fn(LFn::new(Box::new(env), ENV)));

        //Special entry
        symbols.insert(GET.to_string(), LValue::Fn(LFn::new(Box::new(get), GET)));
        symbols.insert(MAP.to_string(), LValue::Fn(LFn::new(Box::new(map), MAP)));
        symbols.insert(LIST.to_string(), LValue::Fn(LFn::new(Box::new(list), LIST)));
        //State is an alias for map

        /*
         * LIST FUNCTIONS
         */
        symbols.insert(CAR.to_string(), LValue::Fn(LFn::new(Box::new(car), CAR)));
        symbols.insert(CDR.to_string(), LValue::Fn(LFn::new(Box::new(cdr), CDR)));
        symbols.insert(LAST.to_string(), LValue::Fn(LFn::new(Box::new(last), LAST)));
        symbols.insert(CONS.to_string(), LValue::Fn(LFn::new(Box::new(cons), CONS)));
        symbols.insert(LEN.to_string(), LValue::Fn(LFn::new(Box::new(length), LEN)));
        symbols.insert(
            EMPTY.to_string(),
            LValue::Fn(LFn::new(Box::new(empty), EMPTY)),
        );

        //Map functions
        symbols.insert(
            GET_MAP.to_string(),
            LValue::Fn(LFn::new(Box::new(get_map), GET_MAP)),
        );
        symbols.insert(
            SET_MAP.to_string(),
            LValue::Fn(LFn::new(Box::new(set_map), SET_MAP)),
        );

        symbols.insert(NOT.to_string(), LValue::Fn(LFn::new(Box::new(not), NOT)));
        symbols.insert(
            NOT_SHORT.to_string(),
            LValue::Fn(LFn::new(Box::new(not), NOT_SHORT)),
        );

        symbols.insert(
            APPEND.to_string(),
            LValue::Fn(LFn::new(Box::new(append), APPEND)),
        );

        symbols.insert(
            MEMBER.to_string(),
            LValue::Fn(LFn::new(Box::new(member), MEMBER)),
        );

        symbols.insert(
            REVERSE.to_string(),
            LValue::Fn(LFn::new(Box::new(reverse), REVERSE)),
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

    pub fn find(&self, var: &str) -> Option<&Self> {
        match self.symbols.get(var) {
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

    pub fn add_macro(&mut self, key: String, _macro: LLambda) {
        self.macro_table.insert(key, _macro);
    }

    pub fn get_macro(&self, key: &str) -> Option<&LLambda> {
        self.macro_table.get(key)
    }

    pub fn get_new_entries(&self) -> Vec<String> {
        self.new_entries.clone()
    }

    //TODO: remove this function
    /*
    pub fn to_file(&self, name_file: String) {
        let mut file = File::create(name_file).unwrap();
        let mut string = String::new();
        string.push_str("(begin \n");
        for key in &self.new_entries {
            let value = self.get_symbol(key).unwrap_or(LValue::Nil);
            string.push_str(format!("(define {} {})", key, value.as_command()).as_str());
        }

        string.push(')');
        match file.write_all(string.as_bytes()) {
            Ok(_) => {}
            Err(e) => panic!("{}", e),
        }
        //eprintln!("write fact base to file");
    }*/
}

pub fn load_module(
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
    ctx: impl GetModule,
    lisp_init: &mut InitLisp,
) -> usize {
    let mut module = ctx.get_module();
    let id = ctxs.insert(module.ctx);
    lisp_init.append(&mut module.raw_lisp);
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

pub fn parse(str: &str, env: &mut RefLEnv, ctxs: &mut ContextCollection) -> Result<LValue, LError> {
    match aries_planning::parsing::sexpr::parse(str) {
        Ok(se) => expand(&parse_into_lvalue(&se, env, ctxs)?, true, env, ctxs),
        Err(e) => Err(SpecialError(format!("Error in command: {}", e.to_string()))),
    }
}

pub fn parse_into_lvalue(
    se: &SExpr,
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
) -> Result<LValue, LError> {
    match se {
        SExpr::Atom(atom) => {
            return match atom.as_str().parse::<i64>() {
                Ok(int) => Ok(LValue::Number(LNumber::Int(int))),
                Err(_) => match atom.as_str().parse::<f64>() {
                    //Test if its a float
                    Ok(float) => Ok(LValue::Number(LNumber::Float(float))),
                    Err(_) => match atom.as_str() {
                        //Test if its a Boolean
                        TRUE => {
                            //println!("atom is boolean true");
                            Ok(LValue::True)
                        }
                        FALSE | NIL => {
                            //println!("atom is boolean false");
                            Ok(LValue::Nil)
                        }
                        s => Ok(s.into()),
                    },
                },
            };
        }
        SExpr::List(list) => {
            //println!("expression is a list");
            let list_iter = list.iter();
            if list_iter.is_empty() {
                Ok(LValue::Nil)
            } else {
                let vec: Vec<LValue> = list_iter
                    .map(|x| parse_into_lvalue(x, env, ctxs))
                    .collect::<Result<_, _>>()?;
                Ok(LValue::List(vec))
            }
        }
    }
}

/*pub fn parse_into_lvalue(
    se: &SExpr,
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
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
                            Ok(LValue::True)
                        }
                        FALSE | NIL => {
                            //println!("atom is boolean false");
                            Ok(LValue::Nil)
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
            let list_iter = list.iter();
            if list_iter.is_empty() {
                Ok(LValue::Nil)
            } else {
                let vec: Vec<LValue> = list_iter
                    .map(|x| parse_into_lvalue(x, env, ctxs))
                    .collect::<Result<_, _>>()?;
                Ok(LValue::List(vec))
            }
        }
    }
}*/

pub fn expand(
    x: &LValue,
    top_level: bool,
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
) -> Result<LValue, LError> {
    match x {
        LValue::List(list) => {
            if let Ok(co) = LCoreOperator::try_from(&list[0]) {
                match co {
                    LCoreOperator::Define | LCoreOperator::DefMacro => {
                        //eprintln!("expand: define: Ok!");
                        if list.len() < 3 {
                            return Err(WrongNumberOfArgument(
                                x.clone(),
                                list.len(),
                                3..std::usize::MAX,
                            ));
                        }
                        let def = LCoreOperator::try_from(&list[0])?;
                        let v = &list[1];
                        let body = &list[2..];
                        match v {
                            LValue::List(v_list) => {
                                if v_list.len() >= 2 {
                                    let f = &v_list[0];
                                    let args = &v_list[1..];
                                    let mut new_body = vec![LCoreOperator::DefLambda.into()];
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
                                let exp = expand(&list[2], top_level, env, ctxs)?;
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
                                        env.add_macro(sym.clone(), proc.try_into()?);
                                    }
                                    //Add to macro_table
                                    return Ok(LValue::Nil);
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
                        let vars = &list[1];
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
                            list.push(LValue::Nil);
                        }
                        if list.len() != 4 {
                            return Err(WrongNumberOfArgument((&list).into(), list.len(), 4..4));
                        }
                        //return map(expand, x)
                        let mut expanded_list = vec![LCoreOperator::If.into()];
                        for x in &list[1..] {
                            expanded_list.push(expand(x, false, env, ctxs)?)
                        }
                        return Ok(expanded_list.into());
                    }
                    LCoreOperator::Quote => {
                        //println!("expand: quote: Ok!");
                        if list.len() != 2 {
                            return Err(WrongNumberOfArgument(list.into(), list.len(), 2..2));
                        }
                        return Ok(vec![LCoreOperator::Quote.into(), list[1].clone()].into());
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
                            Ok(LValue::Nil)
                        } else {
                            let mut expanded_list = vec![LCoreOperator::Begin.into()];
                            for x in &list[1..] {
                                expanded_list.push(expand(x, top_level, env, ctxs)?)
                            }
                            Ok(expanded_list.into())
                        }
                    }
                    LCoreOperator::QuasiQuote => {
                        return if list.len() != 2 {
                            Err(WrongNumberOfArgument(list.into(), list.len(), 2..2))
                        } else {
                            expand_quasi_quote(&list[1], env)
                        }
                    }
                    LCoreOperator::UnQuote => {
                        //TODO: Implémenter msg d'erreur
                        panic!("unquote not at right place")
                    }
                }
            } else if let LValue::Symbol(sym) = &list[0] {
                match env.get_macro(sym) {
                    None => {}
                    Some(m) => {
                        return expand(&m.call(&list[1..], env, ctxs)?, top_level, env, ctxs)
                    }
                }
            }

            let expanded_list: Vec<LValue> = list
                .iter()
                .map(|x| expand(x, false, env, ctxs))
                .collect::<Result<_, _>>()?;
            Ok(expanded_list.into())
        }
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
                let first = &list[0];
                if let LValue::Symbol(s) = first {
                    if let Ok(co) = LCoreOperator::try_from(s.as_str()) {
                        if co == LCoreOperator::UnQuote {
                            if list.len() != 2 {
                                return Err(WrongNumberOfArgument(x.clone(), list.len(), 2..2));
                            }
                            return Ok(list[1].clone());
                        }
                    }
                }
                Ok(vec![
                    env.get_symbol(CONS).unwrap(),
                    expand_quasi_quote(&first, env)?,
                    expand_quasi_quote(&list[1..].to_vec().into(), env)?,
                ]
                .into())
                /*elif is_pair(x[0]) and x[0][0] is _unquotesplicing:
                require(x[0], len(x[0])==2)*/
            }
        }
        _ => Ok(vec![Quote.into(), x.clone()].into()),
    }
    //Verify if has unquotesplicing here
}

/*pub fn eval(
    lv: &LValue,
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
) -> Result<LValue, LError> {
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
                        match &args[0] {
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
                        Ok(LValue::Nil)
                    }
                    LCoreOperator::DefLambda => {
                        let params = match &args[0] {
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
                                vec_sym.into()
                            }
                            LValue::Symbol(s) => s.clone().into(),
                            lv => {
                                return Err(NotInListOfExpectedTypes(
                                    lv.clone(),
                                    lv.into(),
                                    vec![NameTypeLValue::List, NameTypeLValue::Symbol],
                                ))
                            }
                        };
                        let body = &args[1];
                        Ok(LValue::Lambda(LLambda::new(params, body.clone())))
                    }
                    LCoreOperator::If => {
                        let test = args.get(0).unwrap();
                        let conseq = args.get(1).unwrap();
                        let alt = args.get(2).unwrap();
                        match eval(test, env, ctxs) {
                            Ok(LValue::True) => eval(conseq, env, ctxs),
                            Ok(LValue::Nil) => eval(alt, env, ctxs),
                            Ok(lv) => Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Bool)),
                            Err(e) => Err(e),
                        }
                    }
                    LCoreOperator::Quote => return Ok(args.first().unwrap().clone()),
                    LCoreOperator::Set => {
                        //TODO: implement set
                        Ok(LValue::Nil)
                    }
                    LCoreOperator::Begin => {
                        let results: Vec<LValue> = args
                            .iter()
                            .map(|x| eval(x, env, ctxs))
                            .collect::<Result<_, _>>()?;
                        Ok(results.last().unwrap_or(&LValue::Nil).clone())
                    }
                    LCoreOperator::QuasiQuote
                    | LCoreOperator::UnQuote
                    | LCoreOperator::DefMacro => Ok(LValue::Nil),
                },
                LValue::Lambda(l) => {
                    let args: Vec<LValue> = args
                        .iter()
                        .map(|x| eval(x, env, ctxs))
                        .collect::<Result<_, _>>()?;
                    l.call(args.as_slice(), env, ctxs)
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
                lv => Ok(list.into()),
            }
        }
        LValue::Symbol(s) => match env.get_symbol(s.as_str()) {
            None => Ok(lv.clone()),
            Some(lv) => Ok(lv),
        },
        lv => Ok(lv.clone()),
    }
}*/

//Better version of eval
pub fn eval(
    lv: &LValue,
    env: &mut RefLEnv,
    ctxs: &mut ContextCollection,
) -> Result<LValue, LError> {
    let mut lv = lv.clone();
    //TODO: Voir avec arthur une manière plus élégante de faire
    let mut temp_env: RefLEnv;
    let mut env = env;

    loop {
        //println!("lv: {}", lv);
        if let LValue::Symbol(s) = &lv {
            return match env.get_symbol(s.as_str()) {
                None => Ok(lv.clone()),
                Some(lv) => Ok(lv),
            };
        } else if let LValue::List(list) = &lv {
            //println!("expression is a list");
            let list = list.as_slice();
            let proc = &list[0];
            let args = &list[1..];
            //assert!(args.len() >= 2, "Checked in expansion");
            if let LValue::CoreOperator(co) = proc {
                match co {
                    LCoreOperator::Define => {
                        match &args[0] {
                            LValue::Symbol(s) => {
                                let exp = eval(&args[1], &mut env, ctxs)?;
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
                        return Ok(LValue::Nil);
                    }
                    LCoreOperator::DefLambda => {
                        let params = match &args[0] {
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
                                vec_sym.into()
                            }
                            LValue::Symbol(s) => s.clone().into(),
                            lv => {
                                return Err(NotInListOfExpectedTypes(
                                    lv.clone(),
                                    lv.into(),
                                    vec![NameTypeLValue::List, NameTypeLValue::Symbol],
                                ))
                            }
                        };
                        let body = &args[1];
                        return Ok(LValue::Lambda(LLambda::new(params, body.clone())));
                    }
                    LCoreOperator::If => {
                        let test = args.get(0).unwrap();
                        let conseq = args.get(1).unwrap();
                        let alt = args.get(2).unwrap();
                        lv = match eval(test, &mut env, ctxs) {
                            Ok(LValue::True) => eval(conseq, &mut env, ctxs)?,
                            Ok(LValue::Nil) => eval(alt, &mut env, ctxs)?,
                            Ok(lv) => {
                                return Err(WrongType(lv.clone(), lv.into(), NameTypeLValue::Bool))
                            }
                            Err(e) => return Err(e),
                        };
                    }
                    LCoreOperator::Quote => return Ok(args[0].clone()),
                    LCoreOperator::Set => {
                        //TODO: implement set
                        return Ok(LValue::Nil);
                    }
                    LCoreOperator::Begin => {
                        let results: Vec<LValue> = args
                            .iter()
                            .map(|x| eval(x, &mut env, ctxs))
                            .collect::<Result<_, _>>()?;
                        lv = results.last().unwrap_or(&LValue::Nil).clone();
                    }
                    LCoreOperator::QuasiQuote
                    | LCoreOperator::UnQuote
                    | LCoreOperator::DefMacro => return Ok(LValue::Nil),
                }
            } else {
                let exps = list
                    .iter()
                    .map(|x| eval(x, &mut env, ctxs))
                    .collect::<Result<Vec<LValue>, _>>()?;
                let proc = &exps[0];
                let args = &exps[1..];
                match proc {
                    LValue::Lambda(l) => {
                        lv = l.get_body();
                        //TODO: A voir avec Arthur
                        temp_env = l.get_new_env(args, &env)?;
                        env = &mut temp_env;

                        //l.call(args, env, ctxs)

                        //Change the way of doing lambda
                        //Python example
                        /*
                        if isa(proc, Procedure):
                            x = proc.exp
                            env = Env(proc.parms, exps, proc.env)
                         */
                    }
                    LValue::Fn(fun) => {
                        let ctx: &dyn Any = match fun.get_index_mod() {
                            None => &(),
                            Some(u) => ctxs.get_context(u),
                        };
                        return fun.call(args, &env, ctx);
                    }
                    LValue::MutFn(fun) => {
                        return match fun.get_index_mod() {
                            None => fun.call(&args, &mut env, &mut ()),
                            Some(u) => fun.call(&args, &mut env, ctxs.get_mut_context(u)),
                        }
                    }
                    _ => return Ok(exps.into()), //Cas particulier lorsqu'il n'y a aucune procédure à faire de renvoyer une liste
                }
            }
        } else {
            return Ok(lv);
        }
    }
}

pub fn core_macros_and_lambda() -> InitLisp {
    vec![
        MACRO_AND2,
        MACRO_OR2,
        MACRO_NEQ,
        MACRO_NEQ_SHORT,
        LAMBDA_AND,
        LAMBDA_OR,
    ]
    .into()
}

//(begin (define ?v (var (:type object
//                        :value bob))
