use std::collections::{HashMap, HashSet, VecDeque};
use std::sync::Arc;
use tokio::sync::RwLock;

fn main() {
    println!("hello, world!")
}

/*pub struct Node(String);

pub type NodeId = usize;

pub struct Edge {
    node_a: NodeId,
    node_b: NodeId,
    weight: f64,
}

pub struct Graph {
    nodes_ids: HashMap<Node, NodeId>,
    edges: Vec<Edge>,
    n_nodes: usize,
}

struct CraftBotsModule {
    graph: Arc<RwLock<Graph>>,
}

/// Return a sequence of node to go from node_start to node_end
fn dijkstra(graph: Graph, node_start: Node, node_end: Node) -> Vec<Node> {
    let sstart = graph.nodes_ids.get(&node_start).unwrap();
    let send = graph.nodes_ids.get(&node_end).unwrap();
    let mut distances: Vec<Option<f64>> = vec![None; graph.n_nodes];
    distances[sstart] = Some(0.0);

    let mut weights: Vec<Vec<Option<f64>>> = vec![vec![None; graph.n_nodes]; graph.n_nodes];
    let mut neighbours: Vec<Vec<NodeId>> = vec![vec![]; graph.n_nodes];

    for edge in graph.edges {
        weights[edge.node_a][edge.node_b] = Some(edge.weight);
        weights[edge.node_b][edge.node_a] = Some(edge.weight);
        neighbours[edge.node_a].push(edge.node_b);
        neighbours[edge.node_b].push(edge.node_a);
    }

    let predecessors: Vec<Option<NodeId>> = vec![None; graph.n_nodes];

    let find_min = |queue: &HashSet<NodeId>| -> Option<NodeId> {
        let mut mini = None;
        let mut top: Option<NodeId> = None;
        for node in queue {
            match mini {
                None => mini = distances[node],
                Some(mut v) => {
                    if distances[node] < v {
                        mini = distances[node];
                        top = Some(*node)
                    }
                }
            }
        }
        top
    };

    let maj_distances = |s1: &NodeId, s2: &NodeId| {
        if distances[s2] > distances[s1] + weights[s1][s2] {
            distances[s2] = distances[s1] + weights[s1][s2];
            predecessors[s2] = Some(s1)
        }
    };

    let mut queue: HashSet<NodeId>;
    for id in 0..graph.n_nodes {
        queue.insert(id);
    }

    while !queue.is_empty() {
        let s1 = find_min(&queue).unwrap();
        queue.remove(&s1);
        for s2 in &neighbours[s1] {
            maj_distances(&s1, s2);
        }
    }

    let mut route = VecDeque::default();
    let mut s = send;
    while s != sstart {
        route.push_front(s);
        s = predecessors[s];
    }

    route.drain(..).collect()
}*/
