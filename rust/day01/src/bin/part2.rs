use std::{collections::HashMap, env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Require input file path");
    }

    let file_path = &args[1];
    let content = fs::read_to_string(file_path).expect("cannot read file");
    let mut left: Vec<i32> = vec![];
    let mut right: HashMap<i32, i32> = HashMap::new();
    for line in content.trim().split('\n') {
        let items: Vec<&str> = line.split(' ').collect();
        left.push(items.first().expect("invalid input").parse().unwrap());
        let right_value: i32 = items.last().expect("invalid input").parse().unwrap();
        *right.entry(right_value).or_insert(0) += 1;
    }
    let mut result: i32 = 0;
    for val in left.iter() {
        if let Some(freq) = right.get(val) {
            result += freq * val;
        }
    }
    println!("{result}");
}
