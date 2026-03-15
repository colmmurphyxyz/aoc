use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::util::fs_utils::read_input_lines;

#[derive(Debug)]
struct Reindeer {
    #[allow(dead_code)]
    name: String,
    speed: i64,
    fly_time: i64,
    rest_time: i64,
}

fn parse_input() -> Vec<Reindeer> {
    read_input_lines("Day14")
        .iter()
        .map(|s| parse_input_line(&s))
        .collect()
}

fn parse_input_line(line: &str) -> Reindeer {
    let w = words(line);
    let name = w[0].clone();
    let speed = w[3].parse::<i64>().unwrap();
    let fly_time = w[6].parse::<i64>().unwrap();
    let rest_time = w[13].parse::<i64>().unwrap();

    return Reindeer {
        name,
        speed,
        fly_time,
        rest_time,
    };
}

fn distance_travelled_after(reindeer: &Reindeer, seconds: i64) -> i64 {
    let cycle_time = reindeer.fly_time + reindeer.rest_time;
    let full_cycles = seconds / cycle_time;
    let remaining_time = seconds % cycle_time;
    let bonus_time = std::cmp::min(reindeer.fly_time, remaining_time);
    return (full_cycles * reindeer.speed * reindeer.fly_time) + (bonus_time * reindeer.speed);
}

pub fn day14_a() -> i64 {
    parse_input()
        .par_iter()
        .map(|reindeer| distance_travelled_after(&reindeer, 2503))
        .max()
        .unwrap()
}

fn distance_by_second(reindeer: &Reindeer) -> impl Iterator<Item = i64> {
    (1..).map(|seconds| distance_travelled_after(reindeer, seconds))
}

pub fn day14_b() -> i64 {
    let total_seconds = 2503;
    let reindeers = parse_input();
    let n = reindeers.len();
    
    let mut scores = vec![0; n];
    let mut distances = reindeers.iter().map(|r| Box::new(distance_by_second(&r)) as Box<dyn Iterator<Item = i64>>).collect::<Vec<Box<dyn Iterator<Item = i64>>>>();
    for _ in 0..total_seconds {
        let mut max_idxs: Vec<usize> = vec![];
        let mut max_dist_at_time = -1i64;
        for i in 0..n {
            let curr_dist = distances[i].next().unwrap();
            if curr_dist == max_dist_at_time {
                max_idxs.push(i);
            } else if curr_dist > max_dist_at_time {
                max_dist_at_time = curr_dist;
                max_idxs = vec![i];
            }
        }

        for idx in max_idxs {
            scores[idx] += 1;
        }
    }
    *scores.iter().max().unwrap() as i64
}

fn words(s: &str) -> Vec<String> {
    s.splitn(15, ' ').map(|st| st.to_string()).collect()
}
