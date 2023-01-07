use chrono::{DateTime, Utc};
use flow_graph::structs::domain::cst;
use flow_graph::structs::domain::domain_test::DomainTest;
use flow_graph::structs::domain::domain_test::DomainTest::*;
use flow_graph::structs::domain::domain_test::DomainTest::{Boolean, Int};
use flow_graph::structs::domain::type_lattice::TypeLattice;
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

fn union(dc: &TypeLattice, ta: impl Into<DomainTest>, tb: impl Into<DomainTest>) -> DomainTest {
    let ta = ta.into();
    let tb = tb.into();
    let r = DomainTest::union(dc, &ta, &tb);
    println!("{} | {} = {}", ta, tb, r);
    r
}

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
    let dc = &TypeLattice::default();
    meet(
        dc,
        sub(dc, Any, Err(None)),
        union(dc, "a".to_string(), "b".to_string()),
    );

    /*let union = (: &TypeNetwork, ta: Type, tb: Type) {
        println!("{} | {} = {}", ta, tb, dc.union(&ta, &tb))
    };*/
    /*meet(dc, Boolean, Boolean);
    meet(dc, Any, Boolean);
    meet(dc, True, Boolean);
    meet(dc, Boolean, Nil);
    meet(dc, List, Nil);
    meet(dc, Boolean, List);
    meet(dc, List, Map);
    meet(dc, Map, Any);
    meet(dc, Any, Map);
    meet(dc, Any, Any);
    meet(dc, Int, Number);*/
    /*meet(
        dc,
        Err(Some(Box::new(Handle(None)))),
        Err(Some(Box::new(Handle(Some(Box::new(Int)))))),
    );*/
    /*union(dc, Boolean, Union(vec![True, Symbol]));
    union(dc, Union(vec![True, Union(vec![Nil, Number])]), List);
    union(
        dc,
        Union(vec![Boolean, List, Map, Err(None)]),
        Union(vec![List, Number, Symbol, Handle(None)]),
    );*/
    /*sub(dc, Any, Err(None));
    sub(dc, Err(None), Any);
    sub(dc, Err(None), Nil);*/
    //sub(dc, Union(vec![Err(None), Nil]), Union(vec![Nil, Int]));
    //meet(dc, Substract(Box::new(Any), Box::new(Err(None))), Err(None));
    //meet(dc, dc.substract(&Any, &Err(None)), Int);
    //sub(dc, Err(None), Err(Some(Box::new(Int))));
    /*union(dc, Boolean, false);
    union(dc, Boolean, true);
    meet(dc, Boolean, false);
    meet(dc, true, false);
    meet(dc, 1, 1);
    union(dc, 1, 1);
    union(dc, 1, 2);
    sub(dc, 1, 2);
    sub(dc, 1, 1);
    sub(dc, union(dc, 1, 2), 1);
    sub(dc, Boolean, false);
    union(dc, false, sub(dc, Boolean, false));
    sub(dc, 1, Int);
    meet(dc, Err(None), Err(Some(Box::new(1.into()))));*/
    /*union(dc, Err(Some(Box::new(Int))), Err(None));
    sub(dc, sub(dc, Any, Err(None)), Err(Some(Box::new(Int))));
    sub(dc, sub(dc, Any, Err(Some(Box::new(Int)))), Err(None));
    meet(dc, sub(dc, Any, Err(None)), Err(Some(Box::new(Int))));
    meet(dc, sub(dc, Any, Err(Some(Box::new(Int)))), Err(None));
    sub(dc, sub(dc, Any, Int), sub(dc, Number, Float));*/
    /*meet(
        dc,
        dc.substract(&dc.substract(&Any, &Int), &dc.substract(&Number, &Float)),
        Float,
    );
    meet(
        dc,
        dc.substract(&dc.substract(&Any, &Int), &dc.substract(&Number, &Float)),
        Int,
    );*/

    //test_meet(tn, Union(vec![True, Number]), Union(vec![Boolean, Int]));
    //test_meet(tn, "Timepoint".into(), Number);
    //test_meet(tn, Int, "Timepoint".into());
    //test_union(tn, True, Boolean);
    //test_union(tn, Boolean, False);
    //test_union(tn, False, List);
    //test_union(tn, EmptyList, List);
    //test_union(tn, Union(vec![False, Map]), Union(vec![Boolean, List]));
    //test_union(tn, Union(vec![Map, Boolean]), False);
    /*test_union(tn, False, True);
    test_union(tn, Union(vec![Symbol, Float, Int]), Boolean);
    test_union(
        tn,
        Union(vec![Symbol, Float, Int, Map]),
        Union(vec![
            Boolean,
            List,
            Handle(Box::new(Any)),
            Err(Box::new(Any)),
            Map,
        ]),
    );*/

    //output_domain_collection("/home/jeremy/Bureau/domain".into(), dc, true);
    //output_markdown("/home/jeremy/Bureau".into(), tn, true);
}

fn output_domain_collection(path: PathBuf, dc: &TypeLattice, view: bool) {
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("type_network_{}", string_date));
    fs::create_dir_all(&path).unwrap();
    let mut path_dot = path.clone();
    let dot_file_name = "type_network.dot";
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = dc.export_dot();
    file.write_all(dot.as_bytes()).unwrap();
    set_current_dir(&path).unwrap();
    let graph_file_name = "type_network.png";
    Command::new("dot")
        .args(["-Tpng", &dot_file_name, "-o", &graph_file_name])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();
    let mut md_path = path.clone();
    let md_file_name = "type_network.md";
    md_path.push(&md_file_name);
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
            .arg(&md_file_name)
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
