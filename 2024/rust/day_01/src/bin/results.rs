use std::collections::HashMap;
use std::str::FromStr;

struct DistanceMap {
    first: Vec<i32>,
    second: Vec<i32>,
}

impl DistanceMap {
    fn new(s: &str) -> DistanceMap {
        s.parse::<DistanceMap>().unwrap()
    }

    fn order(&mut self) -> &mut Self {
        self.first.sort_by(|a, b| b.cmp(a));
        self.second.sort_by(|a, b| b.cmp(a));
        self
    }

    fn sum(&self) -> i32 {
        let dist: Vec<i32> = self
            .first
            .iter()
            .zip(self.second.iter())
            .map(|(&f, &s)| (s - f).abs())
            .collect();

        dist.iter().sum()
    }

    fn get_similarity(&self) -> i32 {
        let mut first_map = HashMap::new();
        let mut second_map = HashMap::new();

        for i in 0..self.first.len() {
            *first_map.entry(self.first[i]).or_insert(0) += 1;
            *second_map.entry(self.second[i]).or_insert(0) += 1;
        }

        let sum = first_map
            .iter()
            .filter_map(|(k, v)| second_map.get(k).map(|s_v| k * v * s_v))
            .sum();

        sum
    }
}

impl FromStr for DistanceMap {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<DistanceMap, Self::Err> {
        let (first, second) = s
            .lines()
            .into_iter()
            .map(|line| {
                let mut columns = line.split_whitespace();
                (
                    columns
                        .next()
                        .expect("Jump in the fire")
                        .parse::<i32>()
                        .expect("Must to be a u32"),
                    columns
                        .next()
                        .expect("Jump in the fire")
                        .parse::<i32>()
                        .expect("Must to be a u32"),
                )
            })
            .fold((Vec::new(), Vec::new()), |(mut f, mut s), (a, b)| {
                f.push(a);
                s.push(b);
                (f, s)
            });

        Ok(DistanceMap { first, second })
    }
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input(input);
    let similarity = process_input_similarity(input);
    println!("The sum is {}", result);
    println!("The similarity is {}", similarity);
}

fn process_input(i: &str) -> i32 {
    DistanceMap::new(i).order().sum()
}

fn process_input_similarity(i: &str) -> i32 {
    DistanceMap::new(i).get_similarity()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "3   4
4   3
2   5
1   3
3   9
3   3",
        "11"
    )]
    fn should_order_by_lower_pair(#[case] input: &str, #[case] expected: &str) {
        let result = process_input(input).to_string();
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "3   4
4   3
2   5
1   3
3   9
3   3",
        "31"
    )]
    fn should_give_similarity_score(#[case] input: &str, #[case] expected: &str) {
        let result = process_input_similarity(input).to_string();
        assert_eq!(result, expected);
    }
}
