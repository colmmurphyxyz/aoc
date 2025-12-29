use std::cmp::max;

use crate::util::fs_utils::read_input_lines;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

#[derive(Debug, Clone, Copy)]
enum Action {
    TurnOn,
    TurnOff,
    Toggle,
}

#[derive(Debug, Clone, Copy)]
struct Instruction {
    action: Action,
    start: (usize, usize),
    end: (usize, usize),
}

fn to_num(x: &str) -> usize {
    x.to_string().parse::<usize>().unwrap()
}

fn parse_input_line(line: &String) -> Instruction {
    let y: Vec<&str> = line.split(" ").collect();

    let action: Action;
    let mut i = 0usize;
    if y[i] == "toggle" {
        action = Action::Toggle;
        i += 1;
    } else {
        if y[i + 1] == "off" {
            action = Action::TurnOff
        } else {
            action = Action::TurnOn
        }
        i += 2;
    }
    let start_raw: Vec<&str> = y[i].split(',').collect();
    let sx = to_num(start_raw[0]);
    let sy = to_num(start_raw[1]);

    let end_raw: Vec<&str> = y[i + 2].split(',').collect();
    let ex = to_num(end_raw[0]);
    let ey = to_num(end_raw[1]);
    Instruction {
        action: action,
        start: (sx, sy),
        end: (ex, ey),
    }
}

pub fn day06_a() -> i64 {
    let instructions = read_input_lines("Day06")
        .par_iter()
        .map(parse_input_line)
        .collect::<Vec<Instruction>>();
    let mut grid: [[bool; 1000]; 1000] = [[false; 1000]; 1000];
    for inst in instructions {
        for x in inst.start.0..(inst.end.0 + 1) {
            for y in inst.start.1..(inst.end.1 + 1) {
                match inst.action {
                    Action::Toggle => grid[x][y] = !grid[x][y],
                    Action::TurnOff => grid[x][y] = false,
                    Action::TurnOn => grid[x][y] = true,
                }
            }
        }
    }

    let mut lit = 0i64;
    for i in 0..1000 {
        for j in 0..1000 {
            if grid[i][j] {
                lit += 1;
            }
        }
    }

    lit
}

pub fn day06_b() -> i64 {
    let instructions = read_input_lines("Day06")
        .par_iter()
        .map(parse_input_line)
        .collect::<Vec<Instruction>>();
    let mut grid: [[i64; 1000]; 1000] = [[0i64; 1000]; 1000];
    for inst in instructions {
        for x in inst.start.0..(inst.end.0 + 1) {
            for y in inst.start.1..(inst.end.1 + 1) {
                match inst.action {
                    Action::Toggle => grid[x][y] += 2,
                    Action::TurnOff => grid[x][y] = max(0, grid[x][y] - 1),
                    Action::TurnOn => grid[x][y] += 1,
                }
            }
        }
    }

    let mut lit = 0i64;
    for i in 0..1000 {
        for j in 0..1000 {
            lit += grid[i][j];
        }
    }

    lit
}
