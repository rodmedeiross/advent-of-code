use itertools::Itertools;
use std::collections::HashMap;

const OPS: [char; 3] = ['+', '*', '|'];

fn main() {
    let i = include_str!("input.txt");
    let result = process_part1(i);
    println!("Process Part: {}", result);
}

fn parse(i: &str) -> HashMap<usize, Vec<usize>> {
    i.lines()
        .filter_map(|lines| {
            if let Some((key, rest)) = lines.split_once(':') {
                let key = key.trim().parse::<usize>().ok()?;
                let numbers = rest
                    .trim()
                    .split_whitespace()
                    .filter_map(|n| n.trim().parse::<usize>().ok())
                    .collect::<Vec<_>>();
                Some((key, numbers))
            } else {
                None
            }
        })
        .collect::<HashMap<usize, Vec<usize>>>()
}

fn find_valid_permutations(i: &HashMap<usize, Vec<usize>>) -> usize {
    i.iter()
        .filter_map(|(total, numbers)| {
            (0..(numbers.len() - 1))
                .map(|_| OPS)
                .multi_cartesian_product()
                .any(|expr| {
                    let mut seq = expr.iter();
                    *total
                        == numbers
                            .iter()
                            .copied()
                            .reduce(|a, b| match seq.next().unwrap() {
                                '+' => a * b,
                                '*' => a + b,
                                '|' => format!("{}{}", a, b).parse::<usize>().unwrap(),
                                _ => panic!("XIT"),
                            })
                            .unwrap()
                })
                .then_some(total)
        })
        .sum()
}

fn process_part1(i: &str) -> usize {
    let table = parse(i);
    find_valid_permutations(&table)
}

#[cfg(test)]
mod test {

    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "190: 10 19
3267: 81 40 27
83: 17 5
156: 15 6
7290: 6 8 6 15
161011: 16 10 13
192: 17 8 14
21037: 9 7 18 13
292: 11 6 16 20
",
        11387
    )]
    fn should_validate_part1(#[case] input: &str, #[case] expected: usize) {
        let result = process_part1(input);
        assert_eq!(result, expected);
    }
}
