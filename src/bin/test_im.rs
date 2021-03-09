use im::HashMap;

fn main() {
    let mut hash_map: HashMap<String, u32> = HashMap::new();
    hash_map.insert("bob".to_string(), 32);
    hash_map.insert("alice".to_string(), 5);
    for (key, value) in hash_map.iter() {
        println!("{}:{}", key, value);
    }
}
