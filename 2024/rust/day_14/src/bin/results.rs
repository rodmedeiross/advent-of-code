use regex::Regex;
use std::collections::HashSet;
use std::str::FromStr;

const REGEX_PARSE: &str = r"(\-)*\d+";

#[derive(Debug, Default, Clone)]
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

    fn walk(&self, times: isize, x_max: isize, y_max: isize) -> Self {
        Robot {
            position: (
                ((self.position.0 + (self.range.0 * times)) % x_max + x_max) % x_max,
                ((self.position.1 + (self.range.1 * times)) % y_max + y_max) % y_max,
            ),
            range: self.range,
        }
    }
}

trait RobotChecks {
    fn check(&self) -> bool;
}

impl RobotChecks for Vec<Robot> {
    fn check(&self) -> bool {
        let mut seen = HashSet::new();
        self.iter().all(|robot| seen.insert(robot.position))
    }
}

fn div_rem(a: isize, b: isize) -> (isize, isize) {
    (a / b, a % b)
}

fn main() {
    let input = include_str!("input.txt");
    let result = process(input, 101, 103);
    let result2 = process_part2(input, 101, 103);

    println!("Result: {}, {}", result, result2);
}

fn process(i: &str, x_range: isize, y_range: isize) -> usize {
    let robots = i
        .lines()
        .map(|line| line.parse::<Robot>().unwrap())
        .collect::<Vec<Robot>>();

    let robots_walked = robots
        .iter()
        .map(|robot| robot.walk(100, x_range, y_range))
        .collect::<Vec<Robot>>();

    let x_div = div_rem(x_range, 2);
    let y_div = div_rem(y_range, 2);

    let (mut quadrant_1_count, mut quadrant_2_count, mut quadrant_3_count, mut quadrant_4_count) =
        (0, 0, 0, 0);

    robots_walked.iter().for_each(|robot| {
        if robot.position.0 < x_div.0 && robot.position.1 < y_div.0 {
            quadrant_1_count += 1;
        } else if robot.position.0 < x_div.0 && robot.position.1 >= y_div.0 + y_div.1 {
            quadrant_2_count += 1;
        } else if robot.position.0 >= x_div.0 + x_div.1 && robot.position.1 < y_div.0 {
            quadrant_3_count += 1;
        } else if robot.position.0 >= x_div.0 + x_div.1 && robot.position.1 >= y_div.0 + y_div.1 {
            quadrant_4_count += 1;
        }
    });

    quadrant_1_count * quadrant_2_count * quadrant_3_count * quadrant_4_count
}

fn process_part2(i: &str, x_range: isize, y_range: isize) -> usize {
    let robots = i
        .lines()
        .map(|line| line.parse::<Robot>().unwrap())
        .collect::<Vec<Robot>>();

    let mut i = 0;
    loop {
        let robots_walked = robots
            .iter()
            .map(|robot| robot.walk(i, x_range, y_range))
            .collect::<Vec<Robot>>();

        if robots_walked.check() {
            break;
        }

        i += 1;
    }

    i as usize
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
        let result = process(i, 11, 7);
        assert_eq!(result, expected);
    }
}
