use structopt::StructOpt;
use std::path::PathBuf;
use fact_base::repl::Repl;


#[derive(Debug, StructOpt)]
#[structopt(name = "FactBase", about = "A fact and belief database inside an acting and planning engine")]
struct Opt {

    #[structopt(short, long)]
    repl: bool,

    #[structopt(short = "f", long = "file")]
    input: Option<PathBuf>
}

fn main() {
    println!("uname fact base v1.0");

    let opt : Opt = Opt::from_args();
    println!("{:?}", opt);



    if opt.repl == true {
        println!("Fact Base REPL");
        let repl = Repl::default();
        repl.run();
    }
}
