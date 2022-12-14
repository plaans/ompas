use chrono::{DateTime, Utc};
use flow_graph::structs::r#type::Type::*;
use flow_graph::structs::r#type::{Type, TypeNetwork};
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    println!("Hello, world!");
    let tn = &TypeNetwork::default();
    /*test_meet(tn, Literal, Literal);
    test_meet(tn, Boolean, Boolean);
    test_meet(tn, Literal, Boolean);
    test_meet(tn, True, Boolean);
    test_meet(tn, Boolean, Nil);
    test_meet(tn, List, Nil);
    test_meet(tn, List, Map);
    test_meet(tn, Map, Any);
    test_meet(tn, Any, Map);
    test_meet(tn, Any, Any);
    test_meet(tn, Literal, Int);
    test_meet(tn, Any, Err(Box::new(Any)));
    test_meet(tn, Err(Box::new(Int)), Err(Box::new(Any)));
    test_meet(tn, Handle(Box::new(Number)), Handle(Box::new(Literal)));
    test_meet(tn, Literal, Handle(Box::new(Any)));*/

    //test_union(tn, Literal, Boolean);
    //test_union(tn, Literal, List);
    //test_union(tn, Nil, List);
    //test_union(tn, Union(vec![Nil, Map]), Union(vec![Literal, List]));
    //test_union(tn, Union(vec![Map, Boolean]), Nil);
    test_union(tn, Nil, True);
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
    );

    //output_markdown("/home/jeremy/Bureau".into(), tn, true);
}

fn test_meet(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} ^ {} = {}", ta, tb, tn.meet(&ta, &tb))
}

fn test_union(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} | {} = {}", ta, tb, tn.union(&ta, &tb))
}

fn output_markdown(path: PathBuf, network: &TypeNetwork, view: bool) {
    let mut path = path;
    let date: DateTime<Utc> = Utc::now() + chrono::Duration::hours(2);
    let string_date = date.format("%Y-%m-%d_%H-%M-%S").to_string();
    path.push(format!("type_network_{}", string_date));
    fs::create_dir_all(&path).unwrap();
    let mut path_dot = path.clone();
    let dot_file_name = "type_network.dot";
    path_dot.push(&dot_file_name);
    let mut file = File::create(&path_dot).unwrap();
    let dot = network.export_dot();
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