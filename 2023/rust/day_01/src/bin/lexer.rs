use std::iter::Sum;
use std::{error::Error, str::FromStr};

#[derive(Default)]
pub struct CalibrationToken {
    first: u32,
    last: u32,
}

impl FromStr for CalibrationToken {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(CalibrationToken::new(s))
    }
}

impl CalibrationToken {
    fn new(input: &str) -> Self {
        get_numbers(input).unwrap_or_default()
    }
}

#[derive(Default)]
pub struct LineParser {
    input: String,
    result: CalibrationToken,
}

impl Sum<LineParser> for u32 {
    fn sum<I: Iterator<Item = LineParser>>(iter: I) -> Self {
        iter.fold(0, |mut total, item| {
            total += item.result.first * 10 + item.result.last;
            return total;
        })
    }
}

impl LineParser {
    pub fn new(input: &str) -> Self {
        LineParser {
            input: input.to_string(),
            ..Default::default()
        }
    }
    pub fn generate_token(mut self) -> Self {
        self.result = self.input.parse::<CalibrationToken>().unwrap_or_default();
        self
    }
}

pub fn read_lines(file: &str) -> Result<Vec<&str>, Box<dyn Error>> {
    let lines: Vec<&str> = file.lines().map(|line| line.trim()).collect();
    match lines.is_empty() {
        true => Err("Input is empty".into()),
        false => Ok(lines),
    }
}

fn get_numbers(line: &str) -> Result<CalibrationToken, Box<dyn Error>> {
    let mut index = 0;
    let line_iter = std::iter::from_fn(move || {
        let reduced_line = &line[index..];
        let result = if reduced_line.starts_with("one") {
            Some('1')
        } else if reduced_line.starts_with("two") {
            Some('2')
        } else if reduced_line.starts_with("three") {
            Some('3')
        } else if reduced_line.starts_with("four") {
            Some('4')
        } else if reduced_line.starts_with("five") {
            Some('5')
        } else if reduced_line.starts_with("six") {
            Some('6')
        } else if reduced_line.starts_with("seven") {
            Some('7')
        } else if reduced_line.starts_with("eight") {
            Some('8')
        } else if reduced_line.starts_with("nine") {
            Some('9')
        } else {
            let result = reduced_line.chars().next();
            result
        };
        index += 1;
        result
    });

    let mut number_chars = line_iter.filter_map(|c| c.to_digit(10));

    let first = number_chars.next().expect("U Can Ask for my Engieenier");

    match number_chars.last() {
        Some(last) => Ok(CalibrationToken { first, last }),
        None => Ok(CalibrationToken { first, last: first }),
    }
}
