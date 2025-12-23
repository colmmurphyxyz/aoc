use std::fs::read_to_string;

pub fn read_input(name: &str) -> String {
    return read_to_string(format!("input/{}.txt", name)).unwrap()
}

pub fn read_input_lines(name: &str) -> Vec<String> {
    read_input(name)
        .split_terminator('\n')
        .map(|s| s.to_string())
        .collect()
}
