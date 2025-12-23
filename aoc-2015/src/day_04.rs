use md5::compute;
use rayon::prelude::*;

static INPUT: &str = "yzbqklnj";

pub fn day04_a() -> i64 {
    fn predicate(hash: [u8; 16]) -> bool {
        hash[0] == 0 && hash[1] == 0 && hash[2] < 16
    }
    let result = (1i64..)
        .par_bridge()
        .find_any(|&n| {
            let digest = compute(format!("{}{}", INPUT, n));
            predicate(digest.0)
        });

    result.unwrap()
}

pub fn day04_b() -> i64 {
    fn predicate(hash: [u8; 16]) -> bool {
        hash[0] == 0 && hash[1] == 0 && hash[2] == 0
    }
    let result = (1i64..)
        .par_bridge()
        .find_any(|&n| {
            let digest = compute(format!("{}{}", INPUT, n));
            predicate(digest.0)
        });

    result.unwrap()
}
