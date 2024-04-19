use rand; // 0.8.5
use rand::{Rng, distributions::Alphanumeric};
use std::{time::Instant, collections::HashMap, collections::hash_map::DefaultHasher, hash::{Hash, Hasher}, ops::Range};

fn main() {
    let prep_start = Instant::now();
    let mut rng = rand::thread_rng();
    let mut pool: String = String::new();
    let mut ranges: Vec<Range<usize>> = Vec::new();
    for _ in 0..10000000 {
        let start = pool.len();
        let len = rng.gen_range(1..20);
        for _ in 0..len {
            pool.push(rng.sample(Alphanumeric) as char);
        }
        ranges.push(start..pool.len());
    }
    let mut strs: Vec<&str> = ranges.iter().map(|r| pool.get(r.clone()).unwrap()).collect();

    let hash_start = Instant::now();
    println!("prep: {:.3}ms, len: {:.3}M", (hash_start - prep_start).as_millis(), pool.len() as f64 / 1e6);

    let mut hashes: Vec<(u64, [usize; 4])> = Vec::with_capacity(strs.len());
    for s in strs.iter().copied() {
        let mut hasher = DefaultHasher::new();
        s.hash(&mut hasher);
        hashes.push((hasher.finish(), [0; 4]));
    }

    let map_start = Instant::now();
    println!("hash: {:.3}ms", (map_start - hash_start).as_millis());
    
    let mut map: HashMap<&str, usize> = HashMap::new();
    for s in strs.iter().copied() {
        let len = map.len();
        map.entry(s).or_insert(len);
    }
    
    let sort_start = Instant::now();
    println!("map: {:.3}ms, len: {:.3}M", (sort_start - map_start).as_millis(), map.len() as f64 / 1e6);
    
    strs.sort_unstable();
    let hsort_start = Instant::now();
    println!("sort: {:.3}ms, first: {}, last: {}", (hsort_start - sort_start).as_millis(), strs[0], strs.last().unwrap());

    hashes.sort_unstable_by_key(|h| h.0);
    let main_end = Instant::now();
    println!("hsort: {:.3}ms, first: {}, last: {}", (main_end - hsort_start).as_millis(), hashes[0].0, hashes.last().unwrap().0);
}
