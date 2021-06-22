#![allow(dead_code)]
#![allow(unused_imports)]
use im::ordmap::DiffItem::Update;
use im::HashMap;
use ompas_acting::rae::state::{LState, StateType};
use ompas_lisp::core::LEnv;
use ompas_lisp::functions::map;
use ompas_lisp::structs::LError::{SpecialError, WrongNumberOfArgument, WrongType};
use ompas_lisp::structs::{GetModule, LError, LValue, Module, NameTypeLValue};
use ompas_modules::doc::{Documentation, LHelp};
use std::borrow::Borrow;
use std::convert::TryFrom;
use std::mem;
use tokio::sync::mpsc::Sender;

pub const KEY_DYNAMIC: &str = "dynamic";
pub const KEY_STATIC: &str = "static";

#[derive(Default, Debug)]
pub struct GodotState {
    static_state: LState,
    dynamic_state: LState,
}

pub type ActionId = usize;

impl GodotState {
    pub fn set_state(&mut self, s: LState) {
        match s._type {
            Some(StateType::Static) => self.static_state = s,
            Some(StateType::Dynamic) => self.dynamic_state = s,
            _ => {}
        };
    }

    pub fn update_state(&mut self, s: LState) {
        match s._type {
            Some(StateType::Static) => self.static_state = s,
            Some(StateType::Dynamic) => self.dynamic_state = s,
            None => panic!("there should be a state type"),
            _ => {}
        };
    }

    pub fn get_state(&self, st: Option<StateType>) -> LState {
        match st {
            None => {
                //println!("get whole state");
                self.static_state.union(&self.dynamic_state)
            }
            Some(st) => match st {
                StateType::Static => {
                    //println!("get static state");
                    self.static_state.clone()
                }
                StateType::Dynamic => {
                    //println!("get dynamic state");
                    self.dynamic_state.clone()
                }
                _ => todo!(),
            },
        }
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
