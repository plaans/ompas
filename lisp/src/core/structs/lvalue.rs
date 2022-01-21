use crate::core::language::{FUTURE, NIL, TRUE};
use crate::core::structs::lcoreoperator::language::*;
use crate::core::structs::lcoreoperator::LCoreOperator;
use crate::core::structs::lerror::LError;
use crate::core::structs::lerror::LError::ConversionError;
use crate::core::structs::lfuture::*;
use crate::core::structs::llambda::*;
use crate::core::structs::lnumber::LNumber;
use crate::core::structs::new_function::*;
use crate::core::structs::typelvalue::TypeLValue;
use crate::modules::utils::language::*;
use im::HashMap;
use serde::*;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Div, Mul, Sub};

#[derive(Clone, Serialize, Deserialize, Debug)]
#[serde(untagged, rename_all = "lowercase")]
pub enum LValue {
    // symbol
    Symbol(Sym),
    // literaux
    String(String),
    Character(char),
    Number(LNumber),
    #[serde(skip)]
    Fn(LFn),
    //#[serde(skip)]
    //MutFn(LMutFn),
    #[serde(skip)]
    AsyncFn(LAsyncFn),
    //#[serde(skip)]
    //AsyncMutFn(LAsyncMutFn),
    #[serde(skip)]
    Lambda(LLambda),
    #[serde(skip)]
    CoreOperator(LCoreOperator),
    #[serde(skip)]
    Future(LFuture),

    // data structure
    #[serde(skip)]
    Map(im::HashMap<LValue, LValue>),
    List(Vec<LValue>),
    //Quote(Box<LValue>),
    //Refers to boolean 'false and empty list in lisp
    True,
    Nil,
}

type Sym = String;

const TAB_SIZE: usize = 3;
const MAX_LENGTH: usize = 80;

impl LValue {
    fn pretty_print_list_aligned(keyword: &str, list: &[LValue], mut indent: usize) -> String {
        let mut string: String = format!("({}", keyword);
        indent += string.len() + 1;
        let mut first = true;
        for element in list {
            if first {
                first = false;
                string.push(' ');
                string.push_str(element.pretty_print(indent).as_str());
            } else {
                string.push_str(
                    format!("\n{}{}", " ".repeat(indent), element.pretty_print(indent)).as_str(),
                );
            }
        }
        string.push(')');
        string
    }

    fn pretty_print_list(list: &[LValue], indent: usize) -> String {
        let mut string = '('.to_string();

        let mut global_size = 0;
        let mut vec_pretty_printed = vec![];
        for element in list {
            let pretty_printed = element.pretty_print(indent + TAB_SIZE);
            global_size += pretty_printed.len();
            vec_pretty_printed.push(element.pretty_print(indent + TAB_SIZE));
        }

        if global_size < MAX_LENGTH + indent {
            for (i, element) in vec_pretty_printed.iter().enumerate() {
                if i > 0 {
                    string.push(' ');
                }
                string.push_str(element.as_str());
            }
        } else {
            for (i, element) in vec_pretty_printed.iter().enumerate() {
                match i {
                    0 => {
                        string.push_str(element.as_str());
                    }
                    1 => {
                        string.push(' ');
                        string.push_str(element.as_str());
                    }
                    _ => string.push_str(
                        format!("\n{}{}", " ".repeat(indent + TAB_SIZE), element).as_str(),
                    ),
                }
            }
        }

        string.push(')');
        string
    }

    pub fn pretty_print(&self, indent: usize) -> String {
        match self {
            LValue::Lambda(l) => {
                format!(
                    "(lambda {}\n{}{})",
                    l.get_params(),
                    " ".repeat(indent + TAB_SIZE),
                    l.get_body().pretty_print(indent + TAB_SIZE)
                )
            }
            LValue::List(list) => {
                if !list.is_empty() {
                    match &list[0] {
                        LValue::CoreOperator(LCoreOperator::Begin) => {
                            let indent = indent + TAB_SIZE;
                            let mut string = "(begin".to_string();
                            for element in &list[1..] {
                                string.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        element.pretty_print(indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                );
                            }
                            string.push(')');
                            string
                        }
                        LValue::CoreOperator(LCoreOperator::If) => {
                            LValue::pretty_print_list_aligned(IF, &list[1..], indent)
                        }
                        LValue::Symbol(s) => match s.as_str() {
                            LET | LET_STAR => {
                                let (mut string, indent) = match s.as_str() {
                                    LET =>  ("(let ".to_string(), indent + 5),
                                    LET_STAR => ("(let* ".to_string(), indent + 6),
                                    _ => unreachable!("The value of the atom has been checked before and should be let or let*.")
                                };
                                let bindings = &list[1];
                                let body = &list[2];

                                if let LValue::List(bindings) = bindings {
                                    string.push('(');
                                    for (i, binding) in bindings.iter().enumerate() {
                                        if i == 0 {
                                            string.push_str(
                                                binding
                                                    .pretty_print(indent + 1 + TAB_SIZE)
                                                    .as_str(),
                                            );
                                        } else {
                                            string.push_str(
                                                format!(
                                                    "\n{}{}",
                                                    " ".repeat(indent + 1),
                                                    binding.pretty_print(indent + 1 + TAB_SIZE)
                                                )
                                                .as_str(),
                                            );
                                        }
                                    }
                                    string.push(')')
                                } else {
                                    panic!("should be a list")
                                }
                                string.push_str(
                                    format!(
                                        "\n{}{}",
                                        " ".repeat(indent),
                                        body.pretty_print(indent + TAB_SIZE)
                                    )
                                    .as_str(),
                                );
                                string.push(')');
                                string
                            }
                            LAMBDA => {
                                let args = &list[1];
                                let body = &list[2];

                                format!(
                                    "(lambda {}\n{}{}",
                                    args.pretty_print(indent + TAB_SIZE),
                                    " ".repeat(indent + TAB_SIZE),
                                    body.pretty_print(indent + TAB_SIZE)
                                )
                            }
                            COND => LValue::pretty_print_list_aligned(COND, &list[1..], indent),
                            _ => LValue::pretty_print_list(list.as_slice(), indent),
                        },
                        _ => LValue::pretty_print_list(list.as_slice(), indent),
                    }
                } else {
                    NIL.to_string()
                }
            }
            //LValue::Quote(lv) => format!("'{}", lv.pretty_print(indent)),
            lv => lv.to_string(),
        }
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::Fn(fun) => write!(f, "{}", fun.debug_label),
            LValue::MutFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::Nil => write!(f, "{}", NIL),
            LValue::Symbol(s) | LValue::String(s) => write!(f, "{}", s),
            LValue::Number(n) => write!(f, "{}", n),
            LValue::True => write!(f, "{}", TRUE),
            LValue::List(list) => {
                let mut result = String::new();
                result.push('(');
                for (i, element) in list.iter().enumerate() {
                    result.push_str(element.to_string().as_str());
                    if i != list.len() - 1 {
                        result.push(' ');
                    }
                }
                result.push(')');
                write!(f, "{}", result)
            }
            LValue::Lambda(l) => write!(f, "{}", l),
            LValue::Map(m) => {
                let mut result = String::new();
                for (key, value) in m.iter() {
                    result.push_str(format!("{}: {}\n", key, value).as_str());
                }
                write!(f, "{}", result)
            }
            //LValue::Quote(q) => write!(f, "{}", q),
            LValue::CoreOperator(co) => {
                write!(f, "{}", co)
            }
            LValue::Character(c) => write!(f, "{}", c),
            LValue::AsyncFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::AsyncMutFn(fun) => write!(f, "{}", fun.debug_label),
            LValue::Future(_) => write!(f, "{}", FUTURE),
        }
    }
}

impl Hash for LValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValue::Symbol(s) => (*s).hash(state),
            LValue::Number(n) => (*n).hash(state),
            LValue::True => true.hash(state),
            LValue::Map(m) => (*m).hash(state),
            LValue::List(l) => {
                (*l).hash(state);
            }
            /*LValue::Quote(s) => {
                s.to_string().hash(state);
            }*/
            LValue::Nil => false.hash(state),
            _ => {}
        };
    }
}

impl Eq for LValue {}

impl TryFrom<&LValue> for im::HashMap<LValue, LValue> {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Map(m) => Ok(m.clone()),
            _ => Err(ConversionError(
                "hashmap::tryfrom<&LValue>",
                value.into(),
                TypeLValue::Map,
            )),
        }
    }
}

impl TryFrom<LValue> for im::HashMap<LValue, LValue> {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for String {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Symbol(s) => Ok(s.clone()),
            LValue::True => Ok(TRUE.into()),
            LValue::Nil => Ok(NIL.into()),
            LValue::Number(n) => Ok(n.to_string()),
            LValue::Fn(f) => Ok(f.debug_label.to_string()),
            LValue::MutFn(f) => Ok(f.debug_label.to_string()),
            lv => Err(ConversionError(
                "String::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::Symbol,
            )),
        }
    }
}

impl TryFrom<LValue> for String {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for Vec<LValue> {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::List(l) => Ok(l.clone()),
            lv => Err(ConversionError(
                "Vec<LValue>::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::List,
            )),
        }
    }
}

impl TryFrom<LValue> for Vec<LValue> {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for i64 {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(ConversionError(
                "i64::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::Number,
            )),
        }
    }
}

impl TryFrom<LValue> for i64 {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for f64 {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(ConversionError(
                "f64::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::Number,
            )),
        }
    }
}

impl TryFrom<LValue> for f64 {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for bool {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::True => Ok(true),
            LValue::Nil => Ok(false),
            lv => Err(ConversionError(
                "bool::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::Bool,
            )),
        }
    }
}

impl TryFrom<LValue> for bool {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LCoreOperator {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::CoreOperator(co) => Ok(co.clone()),
            LValue::Symbol(s) => Ok(s.as_str().try_into()?),
            lv => Err(ConversionError(
                "LCoreOperator::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::CoreOperator,
            )),
        }
    }
}

impl TryFrom<LValue> for LCoreOperator {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LLambda {
    type Error = LError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Lambda(l) => Ok(l.clone()),
            lv => Err(ConversionError(
                "LLambda::tryfrom<&LValue>",
                lv.into(),
                TypeLValue::Lambda,
            )),
        }
    }
}

impl TryFrom<LValue> for LLambda {
    type Error = LError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl PartialEq for LValue {
    fn eq(&self, other: &Self) -> bool {
        match (self, other) {
            //bool comparison
            //Number comparison
            (LValue::Number(n1), LValue::Number(n2)) => *n1 == *n2,
            (LValue::Symbol(s1), LValue::Symbol(s2)) => *s1 == *s2,
            (LValue::String(s1), LValue::String(s2)) => s1 == s2,
            (LValue::Character(c1), LValue::Character(c2)) => c1 == c2,
            (LValue::True, LValue::True) => true,
            (LValue::Nil, LValue::Nil) => true,
            //Text comparison
            (LValue::List(l1), LValue::List(l2)) => *l1 == *l2,
            (LValue::Map(m1), LValue::Map(m2)) => *m1 == *m2,
            (LValue::Lambda(l1), LValue::Lambda(l2)) => *l1 == *l2,
            (LValue::Fn(f1), LValue::Fn(f2)) => f1.debug_label == f2.debug_label,
            (LValue::MutFn(mf1), LValue::MutFn(mf2)) => mf1.debug_label == mf2.debug_label,
            (LValue::CoreOperator(c1), LValue::CoreOperator(c2)) => c1 == c2,
            (LValue::AsyncFn(af1), LValue::AsyncFn(af2)) => af1.get_label() == af2.get_label(),
            (LValue::AsyncMutFn(amf1), LValue::AsyncMutFn(amf2)) => {
                amf1.get_label() == amf2.get_label()
            }
            (_, _) => false,
        }
    }
}

impl PartialOrd for LValue {
    fn partial_cmp(&self, _other: &Self) -> Option<Ordering> {
        unimplemented!()
    }

    fn lt(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 < *n2,
            _ => false,
        }
    }

    fn le(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 <= *n2,
            _ => false,
        }
    }

    fn gt(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 > *n2,
            _ => false,
        }
    }

    fn ge(&self, other: &Self) -> bool {
        match (self, other) {
            (LValue::Number(n1), LValue::Number(n2)) => *n1 >= *n2,
            _ => false,
        }
    }
}

impl Add for &LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 + n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::Add",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::Add",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::Add",
                format!(
                    "{} and {} cannot be add",
                    TypeLValue::from(l1),
                    TypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Sub for &LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 - n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::sub",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::sub",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::sub",
                format!(
                    "{} and {} cannot be add",
                    TypeLValue::from(l1),
                    TypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Mul for &LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 * n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::mul",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::mul",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::mul",
                format!(
                    "{} and {} cannot be add",
                    TypeLValue::from(l1),
                    TypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Div for &LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 / n2)),
            (LValue::Number(_), l) => Err(LError::WrongType(
                "LValue::div",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),
            (l, LValue::Number(_)) => Err(LError::WrongType(
                "LValue::div",
                l.clone(),
                l.into(),
                TypeLValue::Number,
            )),

            (l1, l2) => Err(LError::SpecialError(
                "LValue::div",
                format!(
                    "{} and {} cannot be add",
                    TypeLValue::from(l1),
                    TypeLValue::from(l2)
                ),
            )),
        }
    }
}

impl Add for LValue {
    type Output = Result<LValue, LError>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl Sub for LValue {
    type Output = Result<LValue, LError>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl Mul for LValue {
    type Output = Result<LValue, LError>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Div for LValue {
    type Output = Result<LValue, LError>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

impl From<&LNumber> for LValue {
    fn from(n: &LNumber) -> Self {
        LValue::Number(n.clone())
    }
}

impl From<LNumber> for LValue {
    fn from(n: LNumber) -> Self {
        (&n).into()
    }
}

impl From<u32> for LValue {
    fn from(u: u32) -> Self {
        LValue::Number(LNumber::Int(u as i64))
    }
}

impl From<usize> for LValue {
    fn from(u: usize) -> Self {
        LValue::Number(LNumber::Usize(u))
    }
}

impl From<&str> for LValue {
    fn from(s: &str) -> Self {
        LValue::Symbol(s.to_string())
    }
}

impl From<String> for LValue {
    fn from(s: String) -> Self {
        (&s).into()
    }
}

impl From<&String> for LValue {
    fn from(s: &String) -> Self {
        LValue::Symbol(s.clone())
    }
}

impl From<&[LValue]> for LValue {
    fn from(lv: &[LValue]) -> Self {
        if lv.is_empty() {
            LValue::Nil
        } else {
            LValue::List(lv.to_vec())
        }
    }
}

impl<T: Clone + Into<LValue>> From<&Vec<T>> for LValue {
    fn from(vec: &Vec<T>) -> Self {
        LValue::List(vec.iter().map(|x| x.clone().into()).collect())
    }
}

impl<T: Clone + Into<LValue>> From<Vec<T>> for LValue {
    fn from(vec: Vec<T>) -> Self {
        (&vec).into()
    }
}

impl From<&LCoreOperator> for LValue {
    fn from(co: &LCoreOperator) -> Self {
        LValue::CoreOperator(co.clone())
    }
}

impl From<LCoreOperator> for LValue {
    fn from(co: LCoreOperator) -> Self {
        (&co).into()
    }
}

impl From<f64> for LValue {
    fn from(f: f64) -> Self {
        LValue::Number(LNumber::Float(f))
    }
}

impl From<i64> for LValue {
    fn from(i: i64) -> Self {
        LValue::Number(LNumber::Int(i))
    }
}

impl From<f32> for LValue {
    fn from(f: f32) -> Self {
        LValue::Number(LNumber::Float(f as f64))
    }
}

impl From<i32> for LValue {
    fn from(i: i32) -> Self {
        LValue::Number(LNumber::Int(i as i64))
    }
}

impl From<bool> for LValue {
    fn from(b: bool) -> Self {
        match b {
            true => LValue::True,
            false => LValue::Nil,
        }
    }
}

impl<K: Clone + Into<LValue>, V: Clone + Into<LValue>> From<&im::HashMap<K, V>> for LValue {
    fn from(map: &im::HashMap<K, V>) -> Self {
        let mut new_map: HashMap<LValue, LValue> = im::HashMap::new();

        for (k, v) in map.iter() {
            new_map.insert(k.clone().into(), v.clone().into());
        }

        LValue::Map(new_map)
    }
}

impl<K: Clone + Into<LValue>, V: Clone + Into<LValue>> From<im::HashMap<K, V>> for LValue {
    fn from(map: im::HashMap<K, V>) -> Self {
        (&map).into()
    }
}

#[cfg(test)]
mod test {
    use super::*;
    use crate::core::structs::lnumber::LNumber;
    fn test_add() {
        let i1: LValue = 3.into();
        let i2: LValue = 5.into();
        let f1: LValue = 3.0.into();
        let f2: LValue = 5.0.into();
        assert_eq!(LValue::Number(LNumber::Int(8)), (&i1 + &i2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(8.0)), (&i1 + &f2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(8.0)), (&f1 + &f2).unwrap());
    }
    fn test_sub() {
        let i1: LValue = 3.into();
        let i2: LValue = 5.into();
        let f1: LValue = 3.0.into();
        let f2: LValue = 5.0.into();
        assert_eq!(LValue::Number(LNumber::Int(-2)), (&i1 - &i2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&i1 - &f2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(-2.0)), (&f1 - &f2).unwrap());
    }

    fn test_mul() {
        let i1: LValue = 3.into();
        let i2: LValue = 5.into();
        let f1: LValue = 3.0.into();
        let f2: LValue = 5.0.into();
        assert_eq!(LValue::Number(LNumber::Int(15)), (&i1 * &i2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(15.0)), (&i1 * &f2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(15.0)), (&f1 * &f2).unwrap());
    }

    fn test_div() {
        let i1: LValue = 3.into();
        let i2: LValue = 5.into();
        let f1: LValue = 3.0.into();
        let f2: LValue = 5.0.into();
        assert_eq!(LValue::Number(LNumber::Int(0)), (&i1 / &i2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(0.6)), (&i1 / &f2).unwrap());
        assert_eq!(LValue::Number(LNumber::Float(0.6)), (&f1 / &f2).unwrap());
    }

    #[test]
    fn test_math() {
        test_add();
        test_sub();
        test_mul();
        test_div();
    }

    fn test_gt() {
        let i1: LValue = 3.into();
        let f2: LValue = 5.0.into();
        assert!(!(&i1 > &f2));
        assert!(&f2 > &i1);
        assert!(!(&f2 > &f2));
    }

    fn test_ge() {
        let i1: LValue = 3.into();
        let f2: LValue = 5.0.into();
        assert!(!(&i1 >= &f2));
        assert!(&f2 >= &i1);
        assert!(&f2 >= &f2);
    }

    fn test_lt() {
        let i1: LValue = 3.into();
        let f2: LValue = 5.0.into();
        assert!(&i1 < &f2);
        assert!(!(&f2 < &i1));
        assert!(!(&f2 < &f2));
    }

    fn test_le() {
        let i1: LValue = 3.into();
        let f2: LValue = 5.0.into();
        assert!(&i1 <= &f2);
        assert!(!(&f2 <= &i1));
        assert!(&f2 <= &f2);
    }

    #[test]
    fn test_ord() {
        test_gt();
        test_ge();
        test_lt();
        test_le();
    }
}
