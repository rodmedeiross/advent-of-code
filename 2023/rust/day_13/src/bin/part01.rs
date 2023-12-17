use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    32
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "#.##..##.
        ..#.##.#.
        ##......#
        ##......#
        ..#.##.#.
        ..##..##.
        #.#.##.#.

        #...##..#
        #....#..#
        ..##..###
        #####.##.
        #####.##.
        ..##..###
        #....#..#",
        "405"
    )]
    fn should_return_sum_of_path_walk(#[case] input: &str, #[case] expected: &str) {
        let result = process_data(input).to_string();
        assert_eq!(result, expected);
    }
}
