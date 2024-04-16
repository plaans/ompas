use crate::Problem;
use petgraph::algo::dijkstra;
use petgraph::prelude::NodeIndex;
use petgraph::{Graph, Undirected};
use rand::prelude::{IteratorRandom, ThreadRng};
use rand::thread_rng;
use std::collections::HashSet;
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
    graph: &mut Graph<N, E, Undirected>,
    _max_distance: u32,
    _max_edge: u32,
    f: &dyn Fn(u32) -> E,
) {
    //populate_greedy(graph, f)
    populate_with_max_distance(graph, _max_distance, f)
}

pub fn populate_with_max_distance<N, E>(
    graph: &mut Graph<N, E, Undirected>,
    max_distance: u32,
    f: &dyn Fn(u32) -> E,
) {
    let all_nodes: HashSet<_> = graph.node_indices().collect();
    let mut to_address: Vec<_> = graph.node_indices().collect();
    //let mut too_much_edges: Vec<NodeIndex> = Default::default();
    let mut next_id = 0;

    let mut rnd = thread_rng();
    while !to_address.is_empty() {
        let node = *to_address.iter().choose(&mut rnd).unwrap();
        let distances = dijkstra(&*graph, node, None, |_| 1);
        let mut not_connected = all_nodes.clone();
        not_connected.remove(&node);
        let mut not_connected: Vec<_> = not_connected
            .difference(&distances.keys().cloned().collect())
            .cloned()
            .collect();
        let mut too_far: Vec<_> = distances
            .iter()
            .filter_map(|(node, d)| {
                if *d > max_distance as i32 {
                    Some(*node)
                } else {
                    None
                }
            })
            .collect();
        too_far.append(&mut not_connected);
        if too_far.is_empty() {
            to_address.retain(|n| *n != node)
        } else {
            let other = too_far.iter().choose(&mut rnd).unwrap();
            graph.add_edge(node, *other, f(next_id as u32));
            next_id += 1;
        }
    }
}

pub fn populate_greedy<N, E>(graph: &mut Graph<N, E, Undirected>, f: &dyn Fn(u32) -> E) {
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
