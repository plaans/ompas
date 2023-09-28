pub mod config;
pub mod domain;

use std::fmt::Write;

use crate::config::Recipe;
use ompas_language::monitor::control::EXEC_TASK;
use ompas_language::monitor::model::{DEF_FACTS, DEF_OBJECTS, DEF_STATIC_FACTS};
use sompas_structs::list;
use sompas_structs::lvalue::LValue;

pub type Task = Vec<String>;

pub struct Report {
    content: String,
    extension: String,
}

impl Default for Report {
    fn default() -> Self {
        Self {
            content: "".to_string(),
            extension: ".txt".to_string(),
        }
    }
}

pub trait Problem {
    fn generate(recipe: &Recipe) -> Result<Self, String>
    where
        Self: Sized;
    fn get_objects(&self) -> Vec<(String, Vec<String>)>;
    fn get_tasks(&self) -> Vec<Task>;
    fn get_dynamic_facts(&self) -> Vec<(LValue, LValue)>;
    fn get_static_facts(&self) -> Vec<(LValue, LValue)>;
    fn to_sompas(&self) -> String {
        let mut str = "(begin\n".to_string();

        write!(str, "({}\n", DEF_OBJECTS).unwrap();
        for (t, objects) in self.get_objects() {
            str.push_str("\t(");
            objects.iter().for_each(|o| write!(str, "{o} ").unwrap());
            write!(str, "{t})\n").unwrap();
        }
        str.push_str(")\n");

        write!(str, "({}\n", DEF_FACTS).unwrap();
        for (sv, v) in self.get_dynamic_facts() {
            write!(str, "\t{}\n", list!(sv, v)).unwrap();
        }
        str.push_str(")\n");

        write!(str, "({}\n", DEF_STATIC_FACTS).unwrap();
        for (sv, v) in self.get_static_facts() {
            write!(str, "{}\n", list!(sv, v)).unwrap();
        }
        str.push_str(")\n");

        for task in self.get_tasks() {
            write!(str, "({}", EXEC_TASK).unwrap();
            for p in task {
                str.push(' ');
                str.push_str(p.as_str());
            }
            str.push_str(")\n");
        }
        write!(str, ")\n").unwrap();
        str
    }
    fn report(&self) -> Report {
        Report::default()
    }
}

pub trait Generator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String>;
}
