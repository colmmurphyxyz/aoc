use std::time::Instant;
use day_01::{day01_a, day01_b};

pub mod util;
pub mod day_01;

fn run_single_day<F>(name: &str, func: F) where F: Fn() -> i64  {
    let start = Instant::now();
    let res = func();
    let runtime = Instant::now() - start;
    println!("{}. Result: {}. Time: {} μs", name, res, runtime.as_micros());
}

fn main() {
    run_single_day("Day  1, part 1", day01_a);
    run_single_day("Day  1, part 2", day01_b);
}
