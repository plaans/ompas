use aries_planning::parsing::sexpr::{SList, parse, SExpr};
use aries_planning::parsing::sexpr::{SAtom};
use std::collections::HashMap;
use std::fmt::Error;
use aries_utils::input::{Sym, Input};

#[derive(Default, Debug)]
pub struct FactBase {
    facts : HashMap<Vec<SAtom>, SAtom>
}

impl FactBase {
    pub fn add(&mut self, key: Vec<SAtom>, value: SAtom) {
        self.facts.insert(key, value);
    }

    pub fn get(&self, key: Vec<SAtom>) -> SAtom {
        let default = SAtom::new("none".to_string());
        match self.facts.get(&key) {
            None => default,
            Some(v) => v.clone()
        }
    }

    pub fn set(&mut self, key: Vec<SAtom>, new_value: SAtom) -> Result<(), &'static str> {
        if self.facts.contains_key(&key) {
            self.facts.insert(key, new_value);
            ()
        }
        Err("wrong fact key")
    }
}