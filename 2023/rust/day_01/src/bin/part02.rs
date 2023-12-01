use crate::lexer::read_lines;

mod lexer;

fn main() {
    let data = include_str!("input_part01.txt");
    let input = read_lines(data).unwrap();
    let result = process_data(input);
    println!("Result: {}", result);
}

fn process_data(data: Vec<&str>) -> i32 {
    let mut sum = 0;
    for line in data {
        let mut digits = line.chars().filter(|c| c.is_digit(10));
        let first = digits.next().unwrap();
        let mut last = digits.last().unwrap_or_default();
        if last == '\0' {
            last = first;
        }
        let concat = format!("{}{}", first, last);
        sum += concat.parse::<i32>().unwrap();
    }
    sum as i32
}

fn lexer_digits_and_words() {}
