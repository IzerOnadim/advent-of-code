use std::fs;

fn main() {
    let result = fs::read_to_string("./input.txt").expect("Unable to read file.")
        .as_str().split("\n\n").map(|s| s.split("\n"))
        .map(|e| e.fold(0, |a, b| a + b.parse::<u32>().unwrap_or_default()))
        .max().unwrap();
    println!("{}", result);
}

