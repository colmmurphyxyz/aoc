use rayon::iter::{IntoParallelRefIterator, ParallelIterator};
use regex::Regex;

use crate::util::fs_utils::read_input_lines;

fn code_characters(s: &str) -> i64 {
    s.len() as i64
}

fn memory_characters(s: &str) -> i64 {
    let re = Regex::new(r#"\\"|\\'|\\x.{2}|\\\\"#).unwrap();
    let cleaned = re.replace_all(s, "$");

    // subtract 2 for opening/closing brackets
    (cleaned.len() - 2) as i64
}

pub fn day08_a() -> i64 {
    // --- RULES ---
    // discard opening and closing quotes
    // escaped single and double quotes go from 2 to 1 char/byte
    // escaped hexadecimal notation (\x + 2 hex chars) becomes just one
    // double backslashes are only one character
    read_input_lines("Day08")
        .par_iter()
        .map(|s| code_characters(&s) - memory_characters(&s))
        .sum()
}

fn re_encoded_difference(s: &str) -> i64 {
    // --- RULES --
    // quotes are escaped, +1 char
    // backslashes are escaped, +1 char
    // +2 chars for opening/closing quotes of re-encoded string
    let mut chars = s.chars();
    let mut diff = 2i64;
    loop {
        match chars.next() {
            None => break,
            Some(next) => match next {
                '"' => diff += 1,
                '\'' => diff += 1,
                '\\' => diff += 1,
                _ => {}
            },
        }
    }

    diff
}

pub fn day08_b() -> i64 {
    read_input_lines("Day08")
        .par_iter()
        .map(|s| re_encoded_difference(s))
        .sum()
}
