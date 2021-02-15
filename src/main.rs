use aries_planning::parsing::sexpr::parse;
use fact_base::repl::Repl;
use std::collections::HashMap;
use std::path::{Path, PathBuf};
#[warn(unused_imports)]
use structopt::StructOpt;

#[derive(Debug, StructOpt)]
#[structopt(
    name = "FactBase",
    about = "A fact and belief database inside an acting and planning engine"
)]
struct Opt {
    #[structopt(short, long)]
    repl: bool,

    #[structopt(short = "f", long = "file")]
    input: Option<PathBuf>,

    #[structopt(short = "t", long = "test")]
    test: bool,
}

fn main() {
    println!("uname fact base v1.0");

    let opt: Opt = Opt::from_args();
    println!("{:?}", opt);

    //test_lib_model(&opt);

    if opt.repl == true {
        println!("Fact Base REPL");
        let mut repl = Repl::default();
        repl.run();
    }
}

#[derive(Default, Debug)]
struct Datas {
    pub vars: HashMap<Vec<u32>, u32>,
}

#[allow(dead_code)]
fn test() {
    let mut datas: Datas = Datas::default();
    let key = vec![1, 2];
    datas.vars.insert(key, 2);
    datas.vars.insert(vec![1, 2, 3], 3);
    println!("{:?}", datas);

    let key_find: Vec<u32> = vec![1, 2, 3];
    let _v = match datas.vars.get(&key_find) {
        None => panic!("wrong key"),
        Some(v) => println!("The value is : {}", *v),
    };
}

#[allow(dead_code)]
fn test_lib_model(opt: &Opt) {
    let path: &Path = opt.input.as_ref().unwrap().as_path();
    let sexpr = parse(path);
    println!("{:?}", sexpr);
}
