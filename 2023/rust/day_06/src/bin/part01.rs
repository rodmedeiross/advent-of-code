use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> usize {
    let game_status: Vec<(Vec<usize>, usize)> = GameStatus::new(data).get_win_rate();

    let lengths: Vec<usize> = game_status
        .iter()
        .map(|(valid_range, _)| (valid_range.len().clone()))
        .collect();

    let total: usize = lengths.iter().fold(1, |acc, &length| acc * length);

    total
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "Time:      7  15   30
        Distance:  9  40  200";

        let result = process_data(input);
        assert_eq!(result, 288);
    }
}
