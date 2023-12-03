use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let input = read_lines(data).unwrap();
    let result = process_data(input);
    println!("Result: {}", result);
}

fn process_data(data: Vec<&str>) -> u32 {
    let values: Vec<LineParser> = data
        .iter()
        .map(|line| LineParser::new(line))
        .collect::<Vec<_>>();

    let result = values
        .iter()
        .map(|x| x.get_max_item_power() as u32)
        .collect::<Vec<_>>();

    result.into_iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
        Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
        Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
        Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
        Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green";

        let data: Vec<&str> = read_lines(&input).unwrap();

        let result = process_data(data);
        assert_eq!(result, 2286);
    }
}
