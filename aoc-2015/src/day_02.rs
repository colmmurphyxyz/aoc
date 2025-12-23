use std::{cmp::min, i64};

use crate::util::fs_utils::read_input_lines;

fn required_paper(length: i64, width: i64, height: i64) -> i64 {
    let side1 = length * width;
    let side2 = length * height;
    let side3 = width * height;
    let smallest_side = min(min(side1, side2), side3);

    (2 * (side1 + side2 + side3)) + smallest_side
}

fn required_ribbon(length: i64, width: i64, height: i64) -> i64 {
    let smallest_perimeter = 2 * min(min(length + width, length + height), width + height);
    let volume = length * width * height;
    smallest_perimeter + volume
}

pub fn day02_a() -> i64 {
    read_input_lines("Day02")
        .into_iter()
        .map(|x| {
            x.split_terminator('x')
                .map(|n| n.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .map(|v| required_paper(v[0], v[1], v[2]))
        .sum()
}

pub fn day02_b() -> i64 {
    read_input_lines("Day02")
        .into_iter()
        .map(|x| {
            x.split_terminator('x')
                .map(|n| n.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .map(|v| required_ribbon(v[0], v[1], v[2]))
        .sum()
}
