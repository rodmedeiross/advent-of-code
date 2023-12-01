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

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lexer::read_lines;

    #[test]
    fn should_return_sum_of_first_and_last_digits() {
        let input = "1abc2
        pqr3stu8vwx
        a1b2c3d4e5f
        treb7uchet
        sdad1";

        let data = read_lines(&input).unwrap();

        let result = process_data(data);
        assert_eq!(result, 153);
    }
}
