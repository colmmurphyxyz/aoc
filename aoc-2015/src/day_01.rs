use crate::util::fs_utils::read_input;

pub fn day01_a() -> i64 {
    let brackets = read_input("Day01");
    let mut count: i64 = 0;
    for bracket in brackets.chars() {
        match bracket {
            '(' => count += 1,
            ')' => count -= 1,
            _ => panic!()
        }
    }
    return count
}

pub fn day01_b() -> i64 {
    let brackets = read_input("Day01");
    let mut count = 0;
    for (idx, bracket) in brackets.char_indices() {
        match bracket {
            '(' => count += 1,
            ')' => count -= 1,
            _ => panic!()
        }
        if count < 0 {
            return (idx + 1) as i64;
        }
    }
    return -1;
}