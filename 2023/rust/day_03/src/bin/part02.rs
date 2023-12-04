use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    let results = data
        .parse::<Machine>()
        .unwrap()
        .convert_in_unified_numbers()
        .verify_gears()
        .unwrap();

    results
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "467..114..
        ...*......
        ..35..633.
        ......#...
        617*......
        .....+.58.
        ..592.....
        ......755.
        ...$.*....
        .664.598..";

        let result = process_data(input);
        assert_eq!(result, 467835);
    }
}
