use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Require input file path");
    }

    let file_path = &args[1];
    let content = fs::read_to_string(file_path).expect("cannot read file");
    let mut c = 0;
    for line in content.trim().split('\n') {
        let numbers: Vec<i32> = line.split(' ').map(|x| x.parse().unwrap()).collect();
        if valid(numbers) {
            c += 1;
        }
    }
    println!("{c}");
}

fn valid(numbers: Vec<i32>) -> bool {
    let mut previous = numbers[0];
    let mut current_direction = numbers[0] - numbers[1];
    for number in &numbers[1..] {
        let d = previous - *number;
        if d == 0 || d > 3 || d < -3 {
            return false;
        }
        if d * current_direction < 0 {
            return false;
        }
        previous = *number;
        current_direction = d;
    }
    return true;
}
