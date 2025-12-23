use day_01::{day01_a, day01_b};
use day_02::{day02_a, day02_b};
use day_03::{day03_a, day03_b};
use std::time::Instant;

pub mod day_01;
pub mod day_02;
pub mod day_03;
pub mod util;

fn run_single_day<F>(name: &str, func: F)
where
    F: Fn() -> i64,
{
    let start = Instant::now();
    let res = func();
    let runtime = Instant::now() - start;
    println!(
        "{}. Result: {}\t\tTime: {} ms\t{} μs",
        name,
        res,
        runtime.as_millis(),
        runtime.as_micros()
    );
}

fn main() {
    run_single_day("Day  1, part 1", day01_a);
    run_single_day("Day  1, part 2", day01_b);
    run_single_day("Day  2, part 1", day02_a);
    run_single_day("Day  2, part 2", day02_b);
    run_single_day("Day  3, part 1", day03_a);
    run_single_day("Day  3, part 2", day03_b);
}
