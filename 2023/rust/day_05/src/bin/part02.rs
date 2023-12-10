use crate::lexer::*;
use indicatif::ParallelProgressIterator;
use rayon::prelude::{IntoParallelIterator, ParallelIterator};

mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    let result = Almanac::from_str(data)
        .generate_new_seeds()
        .generate_walk_trace()
        .into_par_iter()
        .progress();

    result.min().unwrap()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "seeds: 79 14 55 13

        seed-to-soil map:
        50 98 2
        52 50 48

        soil-to-fertilizer map:
        0 15 37
        37 52 2
        39 0 15

        fertilizer-to-water map:
        49 53 8
        0 11 42
        42 0 7
        57 7 4

        water-to-light map:
        88 18 7
        18 25 70

        light-to-temperature map:
        45 77 23
        81 45 19
        68 64 13

        temperature-to-humidity map:
        0 69 1
        1 0 69

        humidity-to-location map:
        60 56 37
        56 93 4";

        let result = process_data(input);
        assert_eq!(result, 46);
    }
}
