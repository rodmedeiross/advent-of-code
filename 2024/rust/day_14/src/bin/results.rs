use regex::Regex;
use std::str::FromStr;

const REGEX_PARSE: &str = r"(\-)*\d+";

#[derive(Debug, Default)]
struct Robot {
    position: (isize, isize),
    range: (isize, isize),
}

impl FromStr for Robot {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(REGEX_PARSE).unwrap();
        let numbers = re
            .captures_iter(i)
            .filter_map(|captures| captures.get(0))
            .map(|number| number.as_str().parse::<isize>().ok())
            .collect::<Option<Vec<isize>>>()
            .ok_or("JUMP IN THE FIRE");

        Ok(Robot::new(numbers?))
    }
}

impl Robot {
    fn new(i: Vec<isize>) -> Self {
        Robot {
            position: (i[0], i[1]),
            range: (i[2], i[3]),
        }
    }
}

fn main() {
    let input = include_str!("input.txt");
    let result = process(input);

    println!("Result: {}", result);
}

fn process(i: &str) -> usize {
    let robots = i
        .lines()
        .map(|line| line.parse::<Robot>().unwrap())
        .collect::<Vec<Robot>>();

    dbg!(robots);

    21
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "p=0,4 v=3,-3
p=6,3 v=-1,-3
p=10,3 v=-1,2
p=2,0 v=2,-1
p=0,0 v=1,3
p=3,0 v=-2,-2
p=7,6 v=-1,-3
p=3,0 v=-1,-2
p=9,3 v=2,3
p=7,3 v=-1,2
p=2,4 v=2,-3
p=9,5 v=-3,-3",
        12
    )]
    fn number_of_robots_by_quadrant(#[case] i: &str, #[case] expected: usize) {
        let result = process(i);
        assert_eq!(result, expected);
    }
}
