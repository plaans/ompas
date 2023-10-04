use chrono::{DateTime, Utc};
use ompas_core::model::sym_domain::cst;
use ompas_core::model::sym_domain::domain_test::DomainTest;
use ompas_core::model::sym_domain::domain_test::DomainTest::*;
use ompas_core::model::sym_domain::type_lattice::TypeLattice;
use ompas_language::sym_table::TYPE_OBJECT;
use ompas_middleware::Master;
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

fn meet(dc: &TypeLattice, ta: impl Into<DomainTest>, tb: impl Into<DomainTest>) -> DomainTest {
    let ta = ta.into();
    let tb = tb.into();
    let r = DomainTest::meet(dc, &ta, &tb);
    println!("{} ^ {} = {}", ta, tb, r);
    r
}

#[allow(unused)]
fn union(dc: &TypeLattice, ta: impl Into<DomainTest>, tb: impl Into<DomainTest>) -> DomainTest {
    let ta = ta.into();
    let tb = tb.into();
    let r = DomainTest::union(dc, &ta, &tb);
    println!("{} | {} = {}", ta, tb, r);
    r
}

#[allow(unused)]
fn sub(dc: &TypeLattice, ta: impl Into<DomainTest>, tb: impl Into<DomainTest>) -> DomainTest {
    let ta = ta.into();
    let tb = tb.into();
    let r = DomainTest::substract(dc, &ta, &tb);
    println!("{} / {} = {}", ta, tb, r);
    r
}

fn main() {
    println!("Hello, world!");
    //let tn = &TypeNetwork::default();
    let mut dc = TypeLattice::new();
    let symbol = dc.get_type_id(TYPE_OBJECT).unwrap();
    let location = dc.add_type("location", vec![*symbol]);
    let _ = dc.add_type("robot", vec![location]);
    let _ = dc.add_type("parking_area", vec![location]);
    let _ = dc.add_type("belt", vec![location]);

    meet(
        &dc,
        Cst(
            Box::new(New("robot".to_string())),
            cst::Cst::Symbol("robot0".to_string()),
        ),
        New("location".to_string()),
    );

    /*meet(
        dc,
        sub(dc, Any, Err(None)),
        union(dc, "a".to_string(), "b".to_string()),
    );*/

    //meet(dc, union(dc, True, Nil), union(dc, True, Nil));
    //meet(dc, sub(dc, Any, Err(None)), sub(dc, Any, Err(None)));
    meet(
        &dc,
        Cst(Box::new(Symbol), cst::Cst::Symbol("r1".to_string())),
        New(TYPE_OBJECT.to_string()),
    );

    //meet(dc, Symbol, New(TYPE_OBJECT.to_string()));

    output_domain_collection("/tmp/domain".into(), &dc, true);
}

fn output_domain_collection(path: PathBuf, dc: &TypeLattice, view: bool) {
    let mut path = path;
    path.push(format!("type_network_{}", Master::get_string_date()));
    fs::create_dir_all(&path).unwrap();
    let mut path_dot = path.clone();
    let dot_file_name = "type_network.dot";
    path_dot.push(dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = dc.export_dot();
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
        Command::new("google-chrome")
            .arg(md_file_name)
            .spawn()
            .unwrap();
    }
}

/*fn test_meet(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} ^ {} = {}", ta, tb, tn.meet(&ta, &tb))
}

fn test_union(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} | {} = {}", ta, tb, tn.union(&ta, &tb))
}*/
