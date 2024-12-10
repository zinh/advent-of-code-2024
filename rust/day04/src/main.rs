use std::{env, fs};

fn main() {
    let args: Vec<String> = env::args().collect();
    if args.len() != 2 {
        panic!("Require input file");
    }

    let file_path = &args[1];
    let content = fs::read_to_string(file_path).expect("cannot read file");
    let lines: Vec<&str> = content.trim().split('\n').collect();
    // ROW:
    // for row in &lines {
    //     println!("{}", count_occurence(row));
    // }
    let arr_lines = lines
        .iter()
        .map(|row| row.chars().collect::<Vec<_>>())
        .collect::<Vec<_>>();
    // COLUMN:
    for col_idx in 0..(lines[0].len()) {
        let s = arr_lines.iter().map(|row| row[col_idx]).collect::<String>();
        println!("{}", count_occurence(&s));
    }
}

fn count_occurence(s: &str) -> usize {
    let count = s.matches("XMAS").count() + s.matches("SAMX").count();
    return count;
}
