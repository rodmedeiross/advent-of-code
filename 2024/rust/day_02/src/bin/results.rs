use std::str::FromStr;

#[derive(Debug)]
struct NuclearFusion {
    sequences: Vec<Vec<usize>>,
}

impl FromStr for NuclearFusion {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let sequences = s
            .lines()
            .map(|line| {
                line.split_whitespace()
                    .map(|n| n.parse::<usize>().expect("Must be a valid usize"))
                    .collect::<Vec<usize>>()
            })
            .collect::<Vec<Vec<usize>>>();

        Ok(NuclearFusion { sequences })
    }
}

impl NuclearFusion {
    fn new(s: &str) -> Self {
        s.parse::<NuclearFusion>().unwrap()
    }

    fn count_safe(&self) -> usize {
        self.sequences
            .iter()
            .filter(|sequence| {
                let increasing = sequence
                    .windows(2)
                    .all(|pair| pair[1] > pair[0] && (pair[1] as i16 - pair[0] as i16).abs() <= 3);

                let decreasing = sequence
                    .windows(2)
                    .all(|pair| pair[1] < pair[0] && (pair[1] as i16 - pair[0] as i16).abs() <= 3);

                increasing || decreasing
            })
            .count() as usize
    }

    fn count_safe_without(&self) -> usize {
        self.sequences
            .iter()
            .filter(|sequence| {
                for i in 0..sequence.len() {
                    let n_seq: Vec<usize> = sequence
                        .iter()
                        .enumerate()
                        .filter(|(index, _)| *index != i)
                        .map(|(_, v)| *v)
                        .collect();

                    let increasing = n_seq.windows(2).all(|pair| {
                        pair[1] > pair[0] && (pair[1] as i16 - pair[0] as i16).abs() <= 3
                    });

                    let decreasing = n_seq.windows(2).all(|pair| {
                        pair[1] < pair[0] && (pair[1] as i16 - pair[0] as i16).abs() <= 3
                    });

                    if increasing || decreasing {
                        return true;
                    }
                }
                false
            })
            .count()
    }
}

fn process_result(input: &str) -> usize {
    NuclearFusion::new(input).count_safe()
}

fn process_result_new_rule(input: &str) -> usize {
    NuclearFusion::new(input).count_safe_without()
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_result_new_rule(input);

    println!("Number of count safe: {}", result);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9",
        "2"
    )]
    fn should_find_safe_report(#[case] input: &str, #[case] expected: &str) {
        let result = process_result(input).to_string();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "7 6 4 2 1
1 2 7 8 9
9 7 6 2 1
1 3 2 4 5
8 6 4 4 1
1 3 6 7 9",
        "4"
    )]
    fn should_find_safe_report_new_rules(#[case] input: &str, #[case] expected: &str) {
        let result = process_result_new_rule(input).to_string();
        assert_eq!(result, expected);
    }
}
