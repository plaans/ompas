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
}
