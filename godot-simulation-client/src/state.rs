#![allow(dead_code)]
#![allow(unused_imports)]
use im::ordmap::DiffItem::Update;
use im::HashMap;
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::map;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use serde_json::ser::State;
use std::borrow::Borrow;
use std::convert::TryFrom;
use std::mem;
use tokio::sync::mpsc::Sender;
use crate::serde::LValueSerde;


pub const KEY_DYNAMIC: &str = "dynamic";
pub const KEY_STATIC: &str = "static";

#[derive(Default)]
pub struct GodotState {
    static_state: LState,
    dynamic_state: LState,
}
#[derive(Clone)]
pub enum StateType {
    Static,
    Dynamic,
}

impl GodotState {

    pub fn set_state(&mut self, s: LState) {
        match s._type {
            Some(StateType::Static) => self.static_state = s,
            Some(StateType::Dynamic) => self.dynamic_state = s,
            _ => {}
        };
    }

    pub fn update_state(&mut self, s: LState) {
        let _old = mem::replace(
            match s._type {
                Some(StateType::Static) => &mut self.static_state,
                Some(StateType::Dynamic) => &mut self.dynamic_state,
                None => panic!("there should be a state type")
            },
            s,
        );
    }

    pub fn get_state(&self, st: Option<StateType>) -> LState {
        match st {
            None => self.static_state.union(&self.dynamic_state),
            Some(st) => match st {
                StateType::Static => self.static_state.clone(),
                StateType::Dynamic => self.dynamic_state.clone(),
            },
        }
    }
}

#[derive(Clone, Default)]
pub struct LState {
    inner: im::HashMap<LValueSerde, LValueSerde>,
    _type: Option<StateType>,
}

impl LState {
    pub fn set_type(&mut self, _type: StateType) {
        self._type = Some(_type)
    }

    pub fn get_type(&self) -> &Option<StateType> {
        &self._type
    }

    pub fn insert(&mut self, key: LValueSerde, value: LValueSerde) {
        self.inner.insert(key, value);
    }

    pub fn union(&self, other: &Self) -> Self {
        self.inner.clone().union(other.inner.clone()).into()
    }

    pub fn append(&mut self, other: &LState) {
        let _ = self.inner.clone().union(other.inner.clone());
    }

    pub fn into_map(&self) -> LValue {
        let mut map: im::HashMap<LValue, LValue>;
        for e in self.inner {
            map.insert(e.0.into(), e.1.into());
        }
        map.into()
    }
}

impl From<&LState> for im::HashMap<LValueSerde, LValueSerde> {
    fn from(ls: &LState) -> Self {
        ls.inner.clone()
    }
}
impl From<LState> for im::HashMap<LValueSerde, LValueSerde> {
    fn from(ls: LState) -> Self {
        (&ls).into()
    }
}
impl From<&im::HashMap<LValueSerde, LValueSerde>> for LState {
    fn from(m: &HashMap<LValueSerde, LValueSerde>) -> Self {
        Self { inner: m.clone(), _type: None }
    }
}
impl From<im::HashMap<LValueSerde, LValueSerde>> for LState {
    fn from(m: HashMap<LValueSerde, LValueSerde>) -> Self {
        (&m).into()
    }
}

/*impl From<&LState> for LValueSerde {
    fn from(ls: &LState) -> Self {
        LValueSerde::Map(ls.inner.clone())
    }
}*/

/*impl From<LState> for LValueSerde {
    fn from(ls: LState) -> Self {
        (&ls).into()
    }
}*/