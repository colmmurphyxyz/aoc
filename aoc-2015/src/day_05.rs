use crate::util::fs_utils::read_input_lines;
use fancy_regex::Regex;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

fn is_nice(s: &str) -> bool {
    // we only need to examine two consecutive letters at a time
    // while also tracking what vowels we've seen
    // no need for regex here
    let mut vowels_seen = 0;
    let mut repeat_seen = false;
    let chars = s.as_bytes();
    for i in 0..(s.len()) - 1 {
        let curr = chars[i];
        // track vowels seen so far
        match curr {
            97 => vowels_seen += 1,  // a
            101 => vowels_seen += 1, // e
            105 => vowels_seen += 1, // i
            111 => vowels_seen += 1, // o
            117 => vowels_seen += 1, // u
            _ => {}
        }
        let next = chars[i + 1];
        if curr == next {
            repeat_seen = true;
        }

        // terminate early if one of 'ab' 'cd' 'pq' 'xy' are found
        if curr == 97 && next == 98 {
            return false;
        }
        if curr == 99 && next == 100 {
            return false;
        }
        if curr == 112 && next == 113 {
            return false;
        }
        if curr == 120 && next == 121 {
            return false;
        }
    }
    // check last letter for vowel
    let last = chars.last().unwrap();
    match last {
        97 => vowels_seen += 1,  // a
        101 => vowels_seen += 1, // e
        105 => vowels_seen += 1, // i
        111 => vowels_seen += 1, // o
        117 => vowels_seen += 1, // u
        _ => {}
    }

    repeat_seen && vowels_seen >= 3
}

pub fn day05_a() -> i64 {
    read_input_lines("Day05")
        .par_iter()
        .filter(|s| is_nice(s.as_str()))
        .count() as i64
}

pub fn is_really_nice(s: &str) -> bool {
    let double_pair = Regex::new(r"([a-z])([a-z]).*\1\2").unwrap();
    let quick_repeat = Regex::new(r"([a-z])([a-z])\1").unwrap();

    double_pair.is_match(s).unwrap() && quick_repeat.is_match(s).unwrap()
}

pub fn day05_b() -> i64 {
    read_input_lines("Day05")
        .par_iter()
        .filter(|s| is_really_nice(s.as_str()))
        .count() as i64
}
