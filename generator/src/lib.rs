pub mod config;
pub mod generator;

use std::fmt::Write;
use std::fs::File;
use std::io::Write as OtherWrite;
use std::path::PathBuf;

use crate::config::Recipe;
use ompas_language::monitor::control::EXEC_TASK;
use ompas_language::monitor::model::{DEF_FACTS, DEF_OBJECTS, DEF_STATIC_FACTS};
use sompas_structs::list;
use sompas_structs::lvalue::LValue;

pub type Task = Vec<String>;

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

        writeln!(str, "\t({}", DEF_OBJECTS).unwrap();
        for (t, objects) in self.get_objects() {
            str.push_str("\t\t(");
            objects.iter().for_each(|o| write!(str, "{o} ").unwrap());
            writeln!(str, "{t})").unwrap();
        }
        str.push_str("\t)\n");

        writeln!(str, "\t({}", DEF_FACTS).unwrap();
        for (sv, v) in self.get_dynamic_facts() {
            writeln!(str, "\t\t{}", list!(sv, v)).unwrap();
        }
        str.push_str("\t)\n\n");

        writeln!(str, "\t({}", DEF_STATIC_FACTS).unwrap();
        for (sv, v) in self.get_static_facts() {
            writeln!(str, "\t\t{}", list!(sv, v)).unwrap();
        }
        str.push_str("\t)\n\n");

        for task in self.get_tasks() {
            write!(str, "\t({}", EXEC_TASK).unwrap();
            for p in task {
                str.push(' ');
                str.push_str(p.as_str());
            }
            str.push_str(")\n");
        }
        writeln!(str, "\n)").unwrap();
        str
    }

    fn store(&mut self, path: &PathBuf) {
        let mut file = File::create(path).unwrap();
        file.write_all(self.to_sompas().as_bytes()).unwrap();
    }

    fn report(&self, _: PathBuf) -> PathBuf {
        todo!()
    }
}

pub trait Generator {
    fn new_problem(&self, recipe: &Recipe) -> Result<Box<dyn Problem>, String>;
}
