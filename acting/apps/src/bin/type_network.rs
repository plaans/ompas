use chrono::{DateTime, Utc};
use ompas_core::model::sym_domain::type_lattice::TypeLattice;
use std::env::set_current_dir;
use std::fs;
use std::fs::File;
use std::io::Write;
use std::path::PathBuf;
use std::process::Command;

fn main() {
    println!("Hello, world!");
    let tn = TypeLattice::new();
    output_markdown("/tmp".into(), &tn, true);
    //let tn = &TypeNetwork::default();

    /*test_meet(tn, List, Map);
    test_meet(tn, Map, Any);
    test_meet(tn, Any, Map);
    test_meet(tn, Any, Any);
    test_meet(tn, Int, Number);
    test_meet(tn, Any, Err(Box::new(Any)));
    test_meet(tn, Err(Box::new(Int)), Err(Box::new(Any)));
    test_meet(tn, Handle(Box::new(Number)), Handle(Box::new(Any)));
    test_meet(tn, Boolean, Handle(Box::new(Any)));*/
    //test_meet(tn, Boolean, Union(vec![True, Symbol]));
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

    //output_domain_collection("/temp".into(), dc, true);
    //output_markdown("/home/jeremy/Bureau".into(), tn, true);
}

/*fn test_meet(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} ^ {} = {}", ta, tb, tn.meet(&ta, &tb))
}

fn test_union(tn: &TypeNetwork, ta: Type, tb: Type) {
    println!("{} | {} = {}", ta, tb, tn.union(&ta, &tb))
}*/

/*fn output_domain_collection(path: PathBuf, dc: &DomainCollection, view: bool) {
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
}*/

fn output_markdown(path: PathBuf, network: &TypeLattice, view: bool) {
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
