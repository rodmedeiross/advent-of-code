use regex::Regex;
use std::str::FromStr;

const MULT_INST_REGEX: &str = r"mul\((\d{1,3}),(\d{1,3})\)";
const DO_DONT_M_REGEX: &str = r"(?:^|do\(\))(?:.*?)(?:don't\(\)|$|\n)";

struct Program {
    numbers: Vec<(u16, u16)>,
}

impl FromStr for Program {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re_do_dont = Regex::new(DO_DONT_M_REGEX).unwrap();
        let re = Regex::new(MULT_INST_REGEX).unwrap();

        let do_dont = re_do_dont
            .find_iter(&s.replace("\n", ""))
            .map(|line| line.as_str().to_string())
            .collect::<Vec<String>>();

        let numbers = re
            .captures_iter(&do_dont.join(""))
            .map(|n| (n[1].parse::<u16>().unwrap(), n[2].parse::<u16>().unwrap()))
            .collect::<Vec<(u16, u16)>>();

        Ok(Program { numbers })
    }
}

impl Program {
    fn new(i: &str) -> Self {
        i.parse::<Program>().unwrap()
    }

    fn multiply(&self) -> i32 {
        self.numbers
            .iter()
            .map(|(x, y)| (*x as i32 * *y as i32))
            .sum()
    }
}

fn process_input(i: &str) -> i32 {
    Program::new(i).multiply()
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input(input);

    println!("Result: {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    // #[rstest]
    // #[case(
    //     "xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))",
    //     "161"
    // )]
    // fn get_valid_instructions_and_process(#[case] input: &str, #[case] expected: &str) {
    //     let result = process_input(input).to_string();
    //     assert_eq!(result, expected);
    // }

    #[rstest]
    #[case(
        "xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))",
        "48"
    )]
    fn get_valid_instructions_and_process_with_do(#[case] input: &str, #[case] expected: &str) {
        let result = process_input(input).to_string();
        assert_eq!(result, expected);
    }
}
