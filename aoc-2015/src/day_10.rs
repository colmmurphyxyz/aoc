use crate::util::fs_utils::read_input;

fn to_digit(c: char) -> u8 {
    match c {
        '0' => 0,
        '1' => 1,
        '2' => 2,
        '3' => 3,
        '4' => 4,
        '5' => 5,
        '6' => 6,
        '7' => 7,
        '8' => 9,
        '9' => 9,
        _ => panic!("Not a decimal digit.")
    }
}

fn parse_input(inp: &str) -> Vec<u8> {
    let mut res: Vec<u8> = Vec::new();
    let chars = inp.chars().collect::<Vec<char>>();

    let mut amount = 0u8;
    let mut digit: char = chars[0];

    for i in 0..(chars.len()) {
        let curr = chars[i];
        if curr != digit {
            let q = to_digit(digit);
            res.push(amount);
            res.push(q);

            digit = curr;
            amount = 1;
        } else {
            amount += 1
        }
    }

    if amount > 0 {
        let q = to_digit(digit);
        res.push(amount);
        res.push(q);
    }

    res
}

fn iterate(seed: &Vec<u8>) -> Vec<u8> {
    let mut res: Vec<u8> = Vec::new();

    let mut amount = 0u8;
    let mut curr = seed[0];

    for x in seed {
        if *x != curr {
            res.push(amount);
            res.push(curr);

            curr = *x;
            amount = 1;
        } else {
            amount += 1
        }
    }

    if amount > 0 {
        res.push(amount);
        res.push(curr);
    }
    res
}

pub fn day10_a() -> i64 {
    let inp = read_input("Day10");
    let mut seed = parse_input(&inp);
    for _ in 0..39 {
        seed = iterate(&seed);
    }
    seed.len() as i64
}

pub fn day10_b() -> i64 {
    let inp = read_input("Day10");
    let mut seed = parse_input(&inp);
    for _ in 0..49 {
        seed = iterate(&seed);
    }
    seed.len() as i64
}
