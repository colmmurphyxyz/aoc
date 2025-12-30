use clap::Parser;
use day_01::{day01_a, day01_b};
use day_02::{day02_a, day02_b};
use day_03::{day03_a, day03_b};
use day_04::{day04_a, day04_b};
use day_05::{day05_a, day05_b};
use day_06::{day06_a, day06_b};
use day_07::{day07_a, day07_b};
use day_08::{day08_a, day08_b};
use day_09::{day09_a, day09_b};
use day_10::{day10_a, day10_b};
use std::time::Instant;

pub mod day_01;
pub mod day_02;
pub mod day_03;
pub mod day_04;
pub mod day_05;
pub mod day_06;
pub mod day_07;
pub mod day_08;
pub mod day_09;
pub mod day_10;
pub mod util;

fn run_single_day<F>(name: &str, func: F)
where
    F: Fn() -> i64,
{
    let start = Instant::now();
    let res = func();
    let runtime = Instant::now() - start;
    println!(
        "{}. Result: {:<20} Time: {} ms\t{} μs",
        name,
        res,
        runtime.as_millis(),
        runtime.as_micros()
    );
}

#[derive(Parser, Debug)]
#[command(version, about="...", long_about = None)]
struct Cli {
    #[arg(short, long, default_value_t = 0)]
    day: usize,
}

fn stub() -> i64 {
    -1
}

static DAYS: [fn() -> i64; 50] = [
    day01_a, day01_b, day02_a, day02_b, day03_a, day03_b, day04_a, day04_b, day05_a, day05_b,
    day06_a, day06_b, day07_a, day07_b, day08_a, day08_b, day09_a, day09_b, day10_a, day10_b, stub,
    stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub,
    stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub, stub,
];

fn main() {
    let day = Cli::parse().day;
    if day < 1 || day > 25 {
        for idx in 0..DAYS.len() {
            let day_number = (idx / 2) + 1;
            let part_number = (idx % 2) + 1;
            let label = format!("Day {:>2}, part {}", day_number, part_number);
            run_single_day(&label, DAYS[idx]);
        }
    } else {
        let idx = (day * 2) - 2;
        run_single_day(&format!("Day {:>2}, part 1", day), DAYS[idx]);
        run_single_day(&format!("Day {:>2}, part 2", day), DAYS[idx + 1]);
    }
}
