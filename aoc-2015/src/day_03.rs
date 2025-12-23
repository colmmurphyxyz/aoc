use std::collections::HashSet;

use crate::util::fs_utils::read_input;

pub fn day03_a() -> i64 {
    let directions = read_input("Day03");
    // x-y coordinates represented as two packed 32 bit ints
    // initialised to 0x77... to avoid dealing with overflow logic
    let mut pos: u64 = 0x7777777777777777;
    let mut seen: HashSet<u64> = HashSet::with_capacity(directions.len() + 1);
    seen.insert(pos);
    for dir in directions.chars() {
        match dir {
            '^' => pos += 1,
            '>' => pos += 1 << 32,
            'v' => pos -= 1,
            '<' => pos -= 1 << 32,
            _ => panic!(),
        }
        seen.insert(pos);
    }

    seen.len() as i64
}

pub fn day03_b() -> i64 {
    let directions = read_input("Day03");
    let mut santa_pos: u64 = 0x7777777777777777;
    let mut robo_pos: u64 = 0x7777777777777777;
    let mut is_robo_turn = false;
    let mut seen: HashSet<u64> = HashSet::with_capacity(directions.len() + 1);
    seen.insert(santa_pos);
    for dir in directions.chars() {
        let pos: &mut u64 = match is_robo_turn {
            true => &mut robo_pos,
            false => &mut santa_pos,
        };
        match dir {
            '^' => *pos += 1,
            '>' => *pos += 1 << 32,
            'v' => *pos -= 1,
            '<' => *pos -= 1 << 32,
            _ => panic!(),
        }
        seen.insert(*pos);
        is_robo_turn = !is_robo_turn;
    }

    seen.len() as i64
}
