use crate::function::{LAsyncFn, LFn};
use crate::kindlvalue::KindLValue;
use crate::lasynchandler::LAsyncHandler;
use crate::lcoreoperator::LCoreOperator;
use crate::llambda::LLambda;
use crate::lnumber::LNumber;
use crate::lruntimeerror::LRuntimeError;
use crate::{lruntimeerror, string, symbol, wrong_type};
use function_name::named;
use im::HashMap;
use sompas_language::*;
use std::cmp::Ordering;
use std::convert::{TryFrom, TryInto};
use std::fmt::{Display, Formatter};
use std::hash::{Hash, Hasher};
use std::ops::{Add, Deref, Div, Mul, Sub};
use std::sync::Arc;

pub type Sym = String;

pub type RefLValue = Arc<LValue>;

#[derive(Clone, Debug)] //Serialize, Deserialize, Debug)]
                        //#[serde(untagged, rename_all = "lowercase")]
pub enum LValue {
    // symbol
    Symbol(Arc<Sym>),
    // literaux
    String(Arc<String>),
    Number(LNumber),
    //#[serde(skip)]
    Fn(LFn),
    //#[serde(skip)]
    AsyncFn(LAsyncFn),
    //#[serde(skip)]
    Lambda(LLambda),
    //#[serde(skip)]
    CoreOperator(LCoreOperator),
    //#[serde(skip)]
    Handler(LAsyncHandler),
    //Future(LFuture),
    Err(RefLValue),
    // data structure
    //#[serde(skip)]
    Map(im::HashMap<LValue, LValue>),
    List(Arc<Vec<LValue>>),
    True,
    //Refers to boolean 'false and empty list in lisp
    Nil,
}

impl From<()> for LValue {
    fn from(_: ()) -> Self {
        LValue::Nil
    }
}

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
                string.push_str(element.format(indent).as_str());
            } else {
                string.push_str(
                    format!("\n{}{}", " ".repeat(indent), element.format(indent)).as_str(),
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
            let pretty_printed = element.format(indent + TAB_SIZE);
            global_size += pretty_printed.len();
            vec_pretty_printed.push(element.format(indent + TAB_SIZE));
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

    pub fn format(&self, indent: usize) -> String {
        match self {
            LValue::Lambda(l) => {
                format!(
                    "(lambda {}\n{}{})",
                    l.get_params(),
                    " ".repeat(indent + TAB_SIZE),
                    l.get_body().format(indent + TAB_SIZE)
                )
            }
            LValue::List(list) => {
                if !list.is_empty() {
                    match &list[0] {
                        LValue::CoreOperator(LCoreOperator::Begin)
                        | LValue::CoreOperator(LCoreOperator::Do) => {
                            let indent = indent + TAB_SIZE;
                            let mut string = format!("({}", list[0]);
                            for (i, element) in list.iter().enumerate() {
                                if i != 0 {
                                    string.push_str(
                                        format!(
                                            "\n{}{}",
                                            " ".repeat(indent),
                                            element.format(indent + TAB_SIZE)
                                        )
                                        .as_str(),
                                    );
                                }
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
                                                binding.format(indent + 1 + TAB_SIZE).as_str(),
                                            );
                                        } else {
                                            string.push_str(
                                                format!(
                                                    "\n{}{}",
                                                    " ".repeat(indent + 1),
                                                    binding.format(indent + 1 + TAB_SIZE)
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
                                        body.format(indent + TAB_SIZE)
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
                                    args.format(indent + TAB_SIZE),
                                    " ".repeat(indent + TAB_SIZE),
                                    body.format(indent + TAB_SIZE)
                                )
                            }
                            COND => LValue::pretty_print_list_aligned(COND, &list[1..], indent),
                            _ => LValue::pretty_print_list(&list, indent),
                        },
                        _ => LValue::pretty_print_list(&list, indent),
                    }
                } else {
                    NIL.to_string()
                }
            }
            //LValue::Quote(lv) => format!("'{}", lv.pretty_print(indent)),
            lv => lv.to_string(),
        }
    }

    pub fn get_kind(&self) -> KindLValue {
        self.into()
    }

    pub fn into_ref(self) -> RefLValue {
        Arc::new(self)
    }
}

impl Display for LValue {
    fn fmt(&self, f: &mut Formatter<'_>) -> Result<(), std::fmt::Error> {
        match self {
            LValue::Fn(fun) => write!(f, "{}", fun.get_label()),
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
            LValue::AsyncFn(fun) => write!(f, "{}", fun.get_label()),
            LValue::Handler(_) => write!(f, "{}", HANDLER),
            LValue::Err(e) => write!(f, "err: {}", e),
        }
    }
}

impl Hash for LValue {
    fn hash<H: Hasher>(&self, state: &mut H) {
        match self {
            LValue::String(s) => s.as_ref().hash(state),
            LValue::Symbol(s) => s.as_ref().hash(state),
            LValue::Number(n) => (*n).hash(state),
            LValue::True => true.hash(state),
            LValue::Map(m) => (*m).hash(state),
            LValue::List(l) => l.as_ref().hash(state),
            LValue::CoreOperator(co) => co.hash(state),
            LValue::Nil => false.hash(state),
            lv => panic!("cannot hash {}", lv.get_kind()),
        };
    }
}

impl Eq for LValue {}

impl TryFrom<&LValue> for im::HashMap<LValue, LValue> {
    type Error = LRuntimeError;

    #[function_name::named]
    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Map(m) => Ok(m.clone()),
            _ => Err(LRuntimeError::conversion_error(
                function_name!(),
                value,
                KindLValue::Map,
            )),
        }
    }
}

impl TryFrom<&LValue> for Arc<Sym> {
    type Error = LRuntimeError;

    #[named]
    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Symbol(a) => Ok(a.clone()),
            LValue::String(s) => Ok(s.clone()),
            lv => Err(LRuntimeError::not_in_list_of_expected_types(
                function_name!(),
                lv,
                vec![KindLValue::String, KindLValue::Symbol],
            )),
        }
    }
}

impl TryFrom<LValue> for im::HashMap<LValue, LValue> {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for String {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Symbol(s) => Ok(s.deref().clone()),
            LValue::True => Ok(TRUE.into()),
            LValue::Nil => Ok(NIL.into()),
            LValue::Number(n) => Ok(n.to_string()),
            LValue::Fn(f) => Ok(f.get_label().to_string()),
            lv => Err(LRuntimeError::conversion_error(
                "String::tryfrom<&LValue>",
                lv,
                KindLValue::Symbol,
            )),
        }
    }
}

impl TryFrom<LValue> for String {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for Vec<LValue> {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::List(l) => Ok(l.deref().clone()),
            LValue::Nil => Ok(vec![]),
            lv => Err(LRuntimeError::conversion_error(
                "Vec<LValue>::tryfrom<&LValue>",
                lv.into(),
                KindLValue::List,
            )),
        }
    }
}

impl TryFrom<LValue> for Vec<LValue> {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for i64 {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(LRuntimeError::conversion_error(
                "i64::tryfrom<&LValue>",
                lv,
                KindLValue::Number,
            )),
        }
    }
}

impl TryFrom<LValue> for i64 {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for f64 {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Number(n) => Ok(n.into()),
            lv => Err(LRuntimeError::conversion_error(
                "f64::tryfrom<&LValue>",
                lv.into(),
                KindLValue::Number,
            )),
        }
    }
}

impl TryFrom<LValue> for f64 {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for bool {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::True => Ok(true),
            LValue::Nil => Ok(false),
            lv => Err(LRuntimeError::conversion_error(
                "bool::tryfrom<&LValue>",
                lv.into(),
                KindLValue::Bool,
            )),
        }
    }
}

//impl TryFrom<&LValue> for LValue {}

impl TryFrom<LValue> for bool {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LCoreOperator {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::CoreOperator(co) => Ok(co.clone()),
            LValue::Symbol(s) => Ok(s.as_str().try_into()?),
            lv => Err(LRuntimeError::conversion_error(
                "LCoreOperator::tryfrom<&LValue>",
                lv.into(),
                KindLValue::CoreOperator,
            )),
        }
    }
}

impl TryFrom<LValue> for LCoreOperator {
    type Error = LRuntimeError;

    fn try_from(value: LValue) -> Result<Self, Self::Error> {
        (&value).try_into()
    }
}

impl TryFrom<&LValue> for LLambda {
    type Error = LRuntimeError;

    fn try_from(value: &LValue) -> Result<Self, Self::Error> {
        match value {
            LValue::Lambda(l) => Ok(l.clone()),
            lv => Err(LRuntimeError::conversion_error(
                "LLambda::tryfrom<&LValue>",
                lv.into(),
                KindLValue::Lambda,
            )),
        }
    }
}

impl TryFrom<LValue> for LLambda {
    type Error = LRuntimeError;

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
            (LValue::True, LValue::True) => true,
            (LValue::Nil, LValue::Nil) => true,
            //Text comparison
            (LValue::List(l1), LValue::List(l2)) => *l1 == *l2,
            (LValue::Map(m1), LValue::Map(m2)) => *m1 == *m2,
            (LValue::Lambda(l1), LValue::Lambda(l2)) => *l1 == *l2,
            (LValue::Fn(f1), LValue::Fn(f2)) => f1.get_label() == f2.get_label(),
            (LValue::CoreOperator(c1), LValue::CoreOperator(c2)) => c1 == c2,
            (LValue::AsyncFn(af1), LValue::AsyncFn(af2)) => af1.get_label() == af2.get_label(),
            (LValue::Err(e1), LValue::Err(e2)) => *e1 == *e2,
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
    type Output = Result<LValue, LRuntimeError>;

    #[named]
    fn add(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 + n2)),
            (LValue::Number(_), l) => Err(wrong_type!(l, KindLValue::Number)),
            (l, LValue::Number(_)) => Err(wrong_type!(l, KindLValue::Number)),

            (l1, l2) => Err(lruntimeerror!(format!(
                "{} and {} cannot be add",
                KindLValue::from(l1),
                KindLValue::from(l2)
            ))),
        }
    }
}

impl Sub for &LValue {
    type Output = Result<LValue, LRuntimeError>;

    #[named]
    fn sub(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 - n2)),
            (LValue::Number(_), l) => Err(wrong_type!(l, KindLValue::Number)),
            (l, LValue::Number(_)) => Err(wrong_type!(l, KindLValue::Number)),

            (l1, l2) => Err(lruntimeerror!(format!(
                "{} and {} cannot be add",
                KindLValue::from(l1),
                KindLValue::from(l2)
            ))),
        }
    }
}

impl Mul for &LValue {
    type Output = Result<LValue, LRuntimeError>;

    #[named]
    fn mul(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 * n2)),
            (LValue::Number(_), l) => Err(wrong_type!(l, KindLValue::Number)),
            (l, LValue::Number(_)) => Err(wrong_type!(l.into(), KindLValue::Number)),

            (l1, l2) => Err(lruntimeerror!(format!(
                "{} and {} cannot be add",
                KindLValue::from(l1),
                KindLValue::from(l2)
            ))),
        }
    }
}

impl Div for &LValue {
    type Output = Result<LValue, LRuntimeError>;

    #[named]
    fn div(self, rhs: Self) -> Self::Output {
        match (self, rhs) {
            (LValue::Number(n1), LValue::Number(n2)) => Ok(LValue::Number(n1 / n2)),
            (LValue::Number(_), l) => Err(wrong_type!(l, KindLValue::Number)),
            (l, LValue::Number(_)) => Err(wrong_type!(l, KindLValue::Number)),

            (l1, l2) => Err(lruntimeerror!(format!(
                "{} and {} cannot be add",
                KindLValue::from(l1),
                KindLValue::from(l2)
            ))),
        }
    }
}

impl Add for LValue {
    type Output = Result<LValue, LRuntimeError>;

    fn add(self, rhs: Self) -> Self::Output {
        &self + &rhs
    }
}

impl Sub for LValue {
    type Output = Result<LValue, LRuntimeError>;

    fn sub(self, rhs: Self) -> Self::Output {
        &self - &rhs
    }
}

impl Mul for LValue {
    type Output = Result<LValue, LRuntimeError>;

    fn mul(self, rhs: Self) -> Self::Output {
        &self * &rhs
    }
}

impl Div for LValue {
    type Output = Result<LValue, LRuntimeError>;

    fn div(self, rhs: Self) -> Self::Output {
        &self / &rhs
    }
}

impl From<&LNumber> for LValue {
    fn from(n: &LNumber) -> Self {
        (*n).into()
    }
}

impl From<LNumber> for LValue {
    fn from(n: LNumber) -> Self {
        LValue::Number(n)
    }
}

impl From<u32> for LValue {
    fn from(u: u32) -> Self {
        LValue::Number(LNumber::Int(u as i64))
    }
}

impl From<usize> for LValue {
    fn from(u: usize) -> Self {
        LValue::Number(LNumber::Int(u as i64))
    }
}

impl From<&str> for LValue {
    fn from(s: &str) -> Self {
        let bytes = s.as_bytes();
        let len = bytes.len();
        if char::from(bytes[0]) == '"' && char::from(bytes[len - 1]) == '"'.into() {
            string!(s[1..len - 1].to_string())
        } else {
            symbol!(s.to_string())
        }
    }
}

impl From<String> for LValue {
    fn from(s: String) -> Self {
        (&s).into()
    }
}

impl From<&String> for LValue {
    fn from(s: &String) -> Self {
        s.as_str().into()
    }
}

impl From<&[LValue]> for LValue {
    fn from(lv: &[LValue]) -> Self {
        if lv.is_empty() {
            LValue::Nil
        } else {
            LValue::List(Arc::new(lv.to_vec()))
        }
    }
}

impl From<&LValue> for LValue {
    fn from(lv: &LValue) -> Self {
        lv.clone()
    }
}

impl From<Arc<Sym>> for LValue {
    fn from(a: Arc<Sym>) -> Self {
        LValue::Symbol(a)
    }
}

impl From<&Arc<Sym>> for LValue {
    fn from(a: &Arc<Sym>) -> Self {
        let a = a.deref();
        a.into()
    }
}

impl<T: Clone + Into<LValue>> From<&Vec<T>> for LValue {
    fn from(vec: &Vec<T>) -> Self {
        vec.clone().into()
    }
}

impl<T: Clone + Into<LValue>> From<Vec<T>> for LValue {
    fn from(vec: Vec<T>) -> Self {
        if vec.is_empty() {
            LValue::Nil
        } else {
            LValue::List(Arc::new(vec.iter().map(|x| x.clone().into()).collect()))
        }
    }
}

impl From<&LCoreOperator> for LValue {
    fn from(co: &LCoreOperator) -> Self {
        (*co).into()
    }
}

impl From<LCoreOperator> for LValue {
    fn from(co: LCoreOperator) -> Self {
        LValue::CoreOperator(co)
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

impl From<LFn> for LValue {
    fn from(lfn: LFn) -> Self {
        Self::Fn(lfn)
    }
}

impl From<LAsyncFn> for LValue {
    fn from(alfn: LAsyncFn) -> Self {
        Self::AsyncFn(alfn)
    }
}

impl<K: Clone + Into<LValue>, V: Clone + Into<LValue>> From<&im::HashMap<K, V>> for LValue {
    fn from(map: &im::HashMap<K, V>) -> Self {
        map.clone().into()
    }
}

impl<K: Clone + Into<LValue>, V: Clone + Into<LValue>> From<im::HashMap<K, V>> for LValue {
    fn from(map: im::HashMap<K, V>) -> Self {
        let mut new_map: HashMap<LValue, LValue> = im::HashMap::new();

        for (k, v) in map.iter() {
            new_map.insert(k.clone().into(), v.clone().into());
        }

        LValue::Map(new_map)
    }
}

#[cfg(test)]
mod test {
    use super::*;
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
