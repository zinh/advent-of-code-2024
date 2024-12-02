use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Require input file path");
    }

    let file_path = &args[1];
    let content = fs::read_to_string(file_path).expect("cannot read file");
    let mut left: Vec<i32> = vec![];
    let mut right: Vec<i32> = vec![];
    for line in content.trim().split('\n') {
        let items: Vec<&str> = line.split(' ').collect();
        left.push(items.first().expect("invalid input").parse().unwrap());
        right.push(items.last().expect("invalid input").parse().unwrap());
    }
    left.sort();
    right.sort();
    let mut result: i32 = 0;
    for (idx, val) in left.iter().enumerate() {
        result += (val - right[idx]).abs();
    }
    println!("{result}");
}
