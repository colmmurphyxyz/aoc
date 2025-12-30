use itertools::Itertools;
use rayon::iter::{
    IntoParallelIterator, ParallelBridge, ParallelIterator,
};
use std::collections::{HashMap, HashSet};

use crate::util::fs_utils::read_input_lines;

fn total_distance(distances: &HashMap<String, HashMap<String, i64>>, path: &Vec<&String>) -> i64 {
    let mut total = 0i64;
    for i in 0..(distances.len() - 1) {
        total += distances[path[i]][path[i + 1]]
    }

    total
}

pub fn day09_a() -> i64 {
    let mut places: HashSet<String> = HashSet::new();
    let mut distances: HashMap<String, HashMap<String, i64>> = HashMap::new();
    let lines = read_input_lines("Day09");
    for line in lines {
        let xs: Vec<&str> = line.split(" ").collect();
        let source = xs[0].to_string();
        let dest = xs[2].to_string();
        let distance = xs[4].parse::<i64>().unwrap();

        places.insert(source.clone());
        places.insert(dest.clone());
        if !distances.contains_key(&source) {
            distances.insert(source.clone(), HashMap::new());
        }
        if !distances.contains_key(&dest) {
            distances.insert(dest.clone(), HashMap::new());
        }
        distances
            .get_mut(&source)
            .unwrap()
            .insert(dest.clone(), distance);
        distances
            .get_mut(&dest)
            .unwrap()
            .insert(source.clone(), distance);
    }
    places
        .iter()
        .permutations(places.len())
        .par_bridge()
        .into_par_iter()
        .map(|x| total_distance(&distances, &x))
        .min()
        .unwrap()
}

pub fn day09_b() -> i64 {
    let mut places: HashSet<String> = HashSet::new();
    let mut distances: HashMap<String, HashMap<String, i64>> = HashMap::new();
    let lines = read_input_lines("Day09");
    for line in lines {
        let xs: Vec<&str> = line.split(" ").collect();
        let source = xs[0].to_string();
        let dest = xs[2].to_string();
        let distance = xs[4].parse::<i64>().unwrap();

        places.insert(source.clone());
        places.insert(dest.clone());
        if !distances.contains_key(&source) {
            distances.insert(source.clone(), HashMap::new());
        }
        if !distances.contains_key(&dest) {
            distances.insert(dest.clone(), HashMap::new());
        }
        distances
            .get_mut(&source)
            .unwrap()
            .insert(dest.clone(), distance);
        distances
            .get_mut(&dest)
            .unwrap()
            .insert(source.clone(), distance);
    }
    places
        .iter()
        .permutations(places.len())
        .par_bridge()
        .into_par_iter()
        .map(|x| total_distance(&distances, &x))
        .max()
        .unwrap()
}
