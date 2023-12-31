use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    let map = data.parse::<WasteLand>().unwrap();
    map.walk_and_count()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "LLR

    AAA = (BBB, BBB)
    BBB = (AAA, ZZZ)
    ZZZ = (ZZZ, ZZZ)",
        "6"
    )]
    #[case(
        "RL

        AAA = (BBB, CCC)
        BBB = (DDD, EEE)
        CCC = (ZZZ, GGG)
        DDD = (DDD, DDD)
        EEE = (EEE, EEE)
        GGG = (GGG, GGG)
        ZZZ = (ZZZ, ZZZ)",
        "2"
    )]
    fn should_return_sum_of_path_walk(#[case] input: &str, #[case] expected: &str) {
        let result = process_data(input).to_string();
        assert_eq!(result, expected);
    }
}
