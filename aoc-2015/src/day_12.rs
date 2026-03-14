use regex::Regex;

use crate::util::fs_utils::read_input;

fn sum_nums_in_string(s: &str) -> i64 {
    let nums_pattern = Regex::new("-?\\d+").unwrap();
    let caps = nums_pattern.find_iter(&s);
    let sum = caps.map(|cap| cap.as_str().parse::<i64>().unwrap()).sum();
    sum
}

pub fn day12_a() -> i64 {
    let inp = read_input("Day12");
    sum_nums_in_string(&inp)
}

fn get_last_bracket(stack: &Vec<char>) -> char {
    match stack.last() {
        Some(b) => *b,
        _ => 'X',
    }
}

fn remove_after_last_object(xs: &mut Vec<char>) {
    let mut k = xs.len() - 1;
    let mut offset = 0;
    while k > 0 && offset >= 0 {
        let curr = xs[k];
        if curr == '}' {
            offset += 1;
        } else if curr == '{' {
            offset -= 1;
        }
        k -= 1;
    }
    xs.truncate(k);
}

pub fn day12_b() -> i64 {
    let inp = read_input("Day12").chars().collect::<Vec<char>>();
    let mut i = 0usize;
    let mut brackets_stack: Vec<char> = vec![];
    let mut filtered: Vec<char> = vec![];

    while i < inp.len() {
        let curr = inp[i];

        if curr == '\"' && get_last_bracket(&brackets_stack) == '{' {
            let word = &inp[(i + 1)..(i + 4)];
            if word == ['r', 'e', 'd'] {
                remove_after_last_object(&mut filtered);
                let desired_stack_size = brackets_stack.len() - 1;
                while brackets_stack.len() != desired_stack_size {
                    i += 1;
                    let foo = inp[i];
                    match foo {
                        '}' | ']' => {
                            brackets_stack.pop();
                        }
                        '[' => {
                            brackets_stack.push('[');
                        }
                        '{' => {
                            brackets_stack.push('{');
                        }
                        _ => {}
                    }
                }
                filtered.push('#');
            }
        } else if curr == '{' || curr == '[' {
            brackets_stack.push(curr);
            filtered.push(curr)
        } else if curr == '}' || curr == ']' {
            brackets_stack.pop();
            filtered.push(curr);
        } else {
            filtered.push(curr);
        }
        i += 1
    }

    let filtered_str = filtered.iter().collect::<String>();

    sum_nums_in_string(&filtered_str)
}
