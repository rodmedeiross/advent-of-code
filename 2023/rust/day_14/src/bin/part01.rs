use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    data.parse::<RockSchema>()
        .unwrap()
        .calculate_rounded_amount()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "O....#....
        O.OO#....#
        .....##...
        OO.#O....O
        .O.....O#.
        O.#..O.#.#
        ..O..#O..O
        .......O..
        #....###..
        #OO..#....",
        "136"
    )]
    fn should_return_sum_of_path_walk(#[case] input: &str, #[case] expected: &str) {
        let result = process_data(input).to_string();
        assert_eq!(result, expected);
    }
}
