use md5::compute;

static INPUT: &str = "yzbqklnj";

pub fn day04_a() -> i64 {
    let mut x = 0;
    loop {
        let hash = compute(format!("{}{}", INPUT, x)).0;
        if hash[0] == 0 && hash[1] == 0 && hash[2] < 16 {
            return x;
        }
        x += 1;
    }
}

pub fn day04_b() -> i64 {
    let mut x = 0;
    loop {
        let hash = compute(format!("{}{}", INPUT, x)).0;
        if hash[0] == 0 && hash[1] == 0 && hash[2] == 0 {
            return x;
        }
        x += 1;
    }
}
