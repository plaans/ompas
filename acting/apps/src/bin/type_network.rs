use ompas_core::model::sym_domain::domain_test::DomainTest;
use ompas_core::model::sym_domain::domain_test::DomainTest::*;
use ompas_core::model::sym_domain::type_lattice::TypeLattice;
use ompas_language::sym_table::TYPE_TIMEPOINT;
use ompas_middleware::Master;
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

#[tokio::main]
async fn main() {
    println!("Hello, world!");
    let tn = TypeLattice::new();
    output_markdown("/tmp".into(), &tn, true);
    let tn = &tn;
    test_meet(tn, New(TYPE_TIMEPOINT.to_string()), Float);
    test_meet(tn, New(TYPE_TIMEPOINT.to_string()), 0.0.into());

    // test_meet(tn, List(None), Map);
    // test_meet(tn, Map, Any);
    // test_meet(tn, Any, Map);
    // test_meet(tn, Any, Any);
    // test_meet(tn, Int, Number);
    // test_meet(tn, Any, Err(None));
    // test_meet(tn, Err(Some(Box::new(Int))), Err(None));
    // test_meet(tn, Handle(Some(Box::new(Number))), Handle(None));
    // test_meet(tn, Boolean, Handle(None));
    // test_meet(tn, Boolean, Union(vec![True, Symbol]));
    // test_meet(tn, Union(vec![True, Number]), Union(vec![Boolean, Int]));
    // //test_meet(tn, "Timepoint".into(), Number);
    // //test_meet(tn, Int, "Timepoint".into());
    // test_union(tn, True, Boolean);
    // test_union(tn, Boolean, False);
    // test_union(tn, False, List(None));
    // //test_union(tn, EmptyList, List(None));
    // test_union(
    //     tn,
    //     Union(vec![False, Map]),
    //     Union(vec![Boolean, List(None)]),
    // );
    // test_union(tn, Union(vec![Map, Boolean]), False);
    // test_union(tn, False, True);
    // test_union(tn, Union(vec![Symbol, Float, Int]), Boolean);
    // test_union(
    //     tn,
    //     Union(vec![Symbol, Float, Int, Map]),
    //     Union(vec![Boolean, List(None), Handle(None), Err(None), Map]),
    // );

    output_markdown("/tmp".into(), tn, true);
}

fn test_meet(tn: &TypeLattice, ta: DomainTest, tb: DomainTest) {
    let r = tn.meet(&ta.into_domain(tn), &tb.into_domain(tn));
    let r2 = DomainTest::from_domain(tn, &r);
    println!("{} ^ {} = {}", ta, tb, r2)
}

fn test_union(tn: &TypeLattice, ta: DomainTest, tb: DomainTest) {
    let r = tn.union(&ta.into_domain(tn), &tb.into_domain(tn));
    let r = DomainTest::from_domain(tn, &r);
    println!("{} | {} = {}", ta, tb, r)
}

fn output_markdown(path: PathBuf, network: &TypeLattice, view: bool) {
    let mut path = path;
    path.push(format!("type_network_{}", Master::get_string_date()));
    fs::create_dir_all(&path).unwrap();
    let mut path_dot = path.clone();
    let dot_file_name = "type_network.dot";
    path_dot.push(dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = network.export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let graph_file_name = "type_network.png";
    Command::new("dot")
        .args(["-Tpng", dot_file_name, "-o", graph_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    let mut md_path = path.clone();
    let md_file_name = "type_network.md";
    md_path.push(md_file_name);
    let mut md_file = File::create(&md_path).unwrap();
    let md: String = format!(
        "# Type Network : \n
![]({})
    ",
        graph_file_name,
    );

    md_file.write_all(md.as_bytes()).unwrap();

    if view {
        println!("open");
        Command::new("google-chrome").arg(md_path).spawn().unwrap();
    }
}
