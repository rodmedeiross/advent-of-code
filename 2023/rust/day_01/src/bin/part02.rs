use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part02.txt");
    let input = read_lines(data).unwrap();
    let result = process_data(input);
    println!("Result: {}", result);
}

fn process_data(data: Vec<&str>) -> u32 {
    let values: Vec<LineParser> = data
        .iter()
        .map(|line| LineParser::new(line).generate_token())
        .collect::<Vec<_>>();

    values.into_iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "two1nine
        eightwothree
        abcone2threexyz
        xtwone3four
        4nineeightseven2
        zoneight234
        7pqrstsixteen";

        let data: Vec<&str> = read_lines(&input).unwrap();

        let result = process_data(data);
        assert_eq!(result, 281);
    }
}
