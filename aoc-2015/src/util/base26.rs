use std::u8;

pub static DIGITS: [char; 26] = [
    'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's',
    't', 'u', 'v', 'w', 'x', 'y', 'z',
];

pub fn alphabet_idx(c: char) -> u8 {
    (c as u8) - 97
}

pub fn to_base26<T: Into<i64>>(x: T) -> Vec<char> {
    let mut res: Vec<char> = vec![];
    let mut num = x.into();
    while num > 0 {
        let remainder = num % 26;
        let base26_digit = DIGITS[remainder as usize];
        num /= 26;
        res.push(base26_digit);
    }
    if res.is_empty() {
        return vec![DIGITS[0]];
    }
    res.reverse();
    res
}

pub fn from_base_26(inp: &str) -> i64 {
    let mut num: i64 = 0;
    for c in inp.chars() {
        num *= 26;
        num += alphabet_idx(c) as i64
    }

    num
}
