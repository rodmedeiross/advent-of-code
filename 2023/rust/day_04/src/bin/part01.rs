use crate::lexer::*;
mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> u16 {
    let game: u16 = read_lines(data)
        .expect("JUMMMP IN THE FIREE")
        .into_iter()
        .map(|lines| Card::new(&lines))
        .collect::<Vec<Card>>()
        .iter()
        .map(|x| x.compute_result())
        .sum();

    game
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
        card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
        card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
        card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
        card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
        card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11";

        let result = process_data(input);
        assert_eq!(result, 13);
    }
}
