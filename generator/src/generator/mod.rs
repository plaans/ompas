use crate::Problem;
use petgraph::prelude::NodeIndex;
use petgraph::Graph;
use rand::prelude::{IteratorRandom, ThreadRng};
use std::fmt::Write;
use std::fs::File;
use std::io::Write as OtherWrite;
use std::path::PathBuf;
use std::process::Command;

pub mod continuous_shop;
pub mod gobot;
pub mod gripper;
pub mod gripper_build;
pub mod gripper_door;
pub mod gripper_multi;
pub mod jobshop;

pub fn populate_topology<N, E>(
    graph: &mut Graph<N, E>,
    _max_distance: u32,
    _max_edge: u32,
    f: &dyn Fn(u32) -> E,
) {
    populate_greedy(graph, f)
}

pub fn populate_greedy<N, E>(graph: &mut Graph<N, E>, f: &dyn Fn(u32) -> E) {
    let rg = &mut ThreadRng::default();

    let indices: Vec<NodeIndex> = graph.node_indices().collect();

    for (id, r) in graph.node_indices().enumerate() {
        let other = indices.iter().filter(|&&i| i != r).choose(rg).unwrap();
        graph.add_edge(r, *other, f(id as u32));
    }
}

pub fn write_dot_to_file(pb: &dyn Problem, path: PathBuf, dot_string: String) -> PathBuf {
    let mut dot_file_name = path.clone();
    dot_file_name.push("topology.dot");
    let mut dot_file = File::create(dot_file_name.clone()).unwrap();

    dot_file.write_all(dot_string.as_bytes()).unwrap();

    let mut img_file_name = path.clone();
    let name = "topology.png";
    img_file_name.push(name);

    let mut content = "## Tasks\n".to_string();
    for t in pb.get_tasks() {
        write!(content, "-{}", t.join(" ")).unwrap();
    }

    write!(content, "\n## TOPOLOGY\n ![]({})", name).unwrap();
    write!(
        content,
        "\n## PROBLEM FILE \n ```lisp\n\
        {}\n\
        ```",
        pb.to_sompas()
    )
    .unwrap();

    Command::new("dot")
        .args([
            "-Tpng",
            dot_file_name.to_str().unwrap(),
            "-o",
            img_file_name.to_str().unwrap(),
        ])
        .spawn()
        .unwrap()
        .wait()
        .unwrap();

    let mut report_file_name = path.clone();
    report_file_name.push("report.md");
    let mut md_file = File::create(&report_file_name).unwrap();

    md_file.write_all(content.as_bytes()).unwrap();

    report_file_name
}
