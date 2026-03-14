use crate::util::base26::{DIGITS};
use crate::util::{base26, fs_utils::read_input};

fn parse_input(inp: &str) -> i64 {
    return base26::from_base_26(inp);
}

fn is_valid_password(password: i64) -> bool {
    let mut last_last_digit: u8 = 127;
    let mut last_digit: u8 = 127;
    let mut double_letters: [bool; 26] = [false; 26];

    let mut three_consecutive = false;

    let mut num = password;
    while num > 0 {
        let remainder = num % 26;
        let curr_digit = remainder as u8;

        let c = DIGITS[curr_digit as usize];
        if c == 'i' || c == 'o' || c == 'l' {
            return false;
        }

        if curr_digit == last_digit {
            double_letters[curr_digit as usize] = true;
        }
        if (curr_digit + 1 == last_digit) && (last_digit + 1 == last_last_digit) {
            three_consecutive = true;
        }

        last_last_digit = last_digit;
        last_digit = curr_digit;

        num /= 26;
    }

    if !three_consecutive {
        return false;
    }

    let mut first_double_letter_found = false;
    for d in double_letters {
        if d {
            if first_double_letter_found {
                return true;
            }
            first_double_letter_found = true;
        }
    }
    return false;
}

pub fn day11_a() -> i64 {
    let mut candidate = parse_input(&read_input("Day11"));

    loop {
        if is_valid_password(candidate) {
            // let b26_password = to_base26(candidate).iter().collect::<String>();
            // println!("Day 11, part a: {}", b26_password);
            return candidate;
        }
        candidate += 1
    }
}

pub fn day11_b() -> i64 {
    let mut candidate = parse_input(&read_input("Day11"));
    let mut first_loop = true;
    loop {
        if is_valid_password(candidate) {
            if !first_loop {
                // let b26_password = to_base26(candidate).iter().collect::<String>();
                // println!("Day 11, part b: {}", b26_password);
                return candidate;
            }
            first_loop = false;
        }
        candidate += 1
    }
}
