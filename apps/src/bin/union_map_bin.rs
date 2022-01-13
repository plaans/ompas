//Example in https://upload.wikimedia.org/wikipedia/commons/thumb/2/2b/Union_find_example.png/220px-Union_find_example.png

use im::HashMap;
use ompas_planning::union_find::Forest;

fn main() {
    let chars = "abcdef".chars();

    let mut forest: Forest<char> = Forest::default();

    let mut map_char_node = HashMap::new();

    for char in chars {
        map_char_node.insert(char, forest.new_node(char));
    }

    //println!("[debug] forest: {:?}\n", forest);
    println!("forest: {}\n", forest);

    forest.union(
        map_char_node.get(&'a').unwrap(),
        map_char_node.get(&'b').unwrap(),
    );

    println!("union a b");

    println!("forest: {}\n", forest);

    forest.union(
        map_char_node.get(&'c').unwrap(),
        map_char_node.get(&'d').unwrap(),
    );

    println!("union c d");

    println!("forest: {}\n", forest);

    forest.union(
        map_char_node.get(&'a').unwrap(),
        map_char_node.get(&'c').unwrap(),
    );

    println!("union a c");

    println!("forest: {}\n", forest);
    println!("[debug] forest: {:?}\n", forest);

    forest.find(map_char_node.get(&'d').unwrap());

    println!("find d");

    println!("forest: {}\n", forest);

    forest.union(
        map_char_node.get(&'b').unwrap(),
        map_char_node.get(&'e').unwrap(),
    );

    println!("union b e");

    println!("forest: {}\n", forest);
}
