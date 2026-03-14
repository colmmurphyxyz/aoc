use std::{collections::HashMap, i64};

use itertools::Itertools;
use rayon::iter::{IntoParallelRefIterator, ParallelIterator};

use crate::util::fs_utils::read_input_lines;

type HappinessMap = HashMap<String, HashMap<String, i64>>;

fn parse_input() -> HappinessMap {
    let mut outer_map: HappinessMap = HashMap::new();
    let input_lines = read_input_lines("Day13");
    for line in input_lines {
        let w = words(&line);
        let person_a = &w[0];
        let person_b = &w[10].replace(".", "");
        let raw_happiness = w[3].parse::<i64>().unwrap();
        let happiness = match w[2].as_str() {
            "lose" => -raw_happiness,
            _ => raw_happiness,
        };

        add_happiness_value(&mut outer_map, person_a, person_b, happiness);
    }

    outer_map
}

fn add_happiness_value(map: &mut HappinessMap, a: &String, b: &String, happiness: i64) {
    if !map.contains_key(a) {
        map.insert(a.clone(), HashMap::new());
    }
    let inner = map.get_mut(a).unwrap();
    inner.insert(b.clone(), happiness);
}

fn get_happiness_value(map: &HappinessMap, a: &String, b: &String) -> i64 {
    let inner = map.get(a);
    match inner {
        None => 0,
        Some(inner_map) => *inner_map.get(b).unwrap_or(&0),
    }
}

fn happiness_score(map: &HappinessMap, candidate: &Vec<String>) -> i64 {
    let mut happiness = 0i64;

    happiness += get_happiness_value(map, &candidate[0], &candidate.last().unwrap());
    happiness += get_happiness_value(map, &candidate[0], &candidate[1]);
    for i in 1..(candidate.len() - 1) {
        happiness += get_happiness_value(map, &candidate[i], &candidate[i - 1]);
        happiness += get_happiness_value(map, &candidate[i], &candidate[i + 1]);
    }
    let last_idx = candidate.len() - 1;
    happiness += get_happiness_value(map, &candidate[last_idx], &candidate[last_idx - 1]);
    happiness += get_happiness_value(map, &candidate[last_idx], &candidate[0]);

    return happiness;
}

pub fn day13_a() -> i64 {
    let map = parse_input();
    let seating_arrangement = map.keys().map(|s| s.clone());
    let num_guests = seating_arrangement.len();
    let permutations = seating_arrangement
        .permutations(num_guests)
        .collect::<Vec<Vec<String>>>();
    let perms_iter = permutations.par_iter();

    let maximum = perms_iter
        .map(|candidate| happiness_score(&map, &candidate))
        .max();

    maximum.unwrap()
}

fn happiness_score_with_you(map: &HappinessMap, cand: &Vec<String>) -> i64 {
    let n = cand.len();
    let first_pairing = get_happiness_value(map, &cand[0], &cand[n - 1])
        + get_happiness_value(map, &cand[n - 1], &cand[0]);
    let mut worst_pairing: i64 = first_pairing;
    let mut happiness = first_pairing;
    for i in 0..(n - 1) {
        let pairing_score = get_happiness_value(map, &cand[i], &cand[i + 1])
            + get_happiness_value(map, &cand[i + 1], &cand[i]);
        if pairing_score < worst_pairing {
            worst_pairing = pairing_score;
        }
        happiness += pairing_score;
    }

    happiness - worst_pairing
}

pub fn day13_b() -> i64 {
    let map = parse_input();
    let seating_arrangement = map.keys().map(|s| s.clone());
    let num_guests = seating_arrangement.len();
    let permutations = seating_arrangement
        .permutations(num_guests)
        .collect::<Vec<Vec<String>>>();
    let perms_iter = permutations.par_iter();

    let maximum = perms_iter
        .map(|candidate| happiness_score_with_you(&map, &candidate))
        .max();

    maximum.unwrap()
}

fn words(s: &str) -> Vec<String> {
    s.splitn(11, ' ').map(|st| st.to_string()).collect()
}
