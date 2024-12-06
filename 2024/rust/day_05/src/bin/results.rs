use std::collections::HashMap;
use std::str::FromStr;

struct PrinterTable {
    rules: HashMap<usize, Vec<usize>>,
    page_updates: Vec<Vec<usize>>,
}

impl FromStr for PrinterTable {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let parts: Vec<&str> = i.split("\n\n").collect();

        let mut rules: HashMap<usize, Vec<usize>> = HashMap::new();

        for line in parts[0].lines() {
            let mut numbers = line.split('|');
            rules
                .entry(numbers.next().unwrap().parse::<usize>().unwrap())
                .or_insert_with(Vec::new)
                .push(numbers.next().unwrap().parse::<usize>().unwrap());
        }

        let page_updates: Vec<Vec<usize>> = parts[1]
            .lines()
            .map(|lines| {
                lines
                    .split(',')
                    .map(|x| x.parse::<usize>().unwrap())
                    .collect::<Vec<usize>>()
            })
            .collect();

        Ok(PrinterTable {
            rules,
            page_updates,
        })
    }
}

impl PrinterTable {
    fn new(i: &str) -> Self {
        i.parse::<PrinterTable>().unwrap()
    }

    fn process(&self) -> usize {
        let result: Vec<Vec<usize>> = self
            .page_updates
            .iter()
            .filter_map(|numbers| {
                let pass = numbers.iter().enumerate().all(|(i, &number)| {
                    if let Some(rules) = self.rules.get(&number) {
                        (0..i).all(|v| !rules.contains(&numbers[v]))
                    } else {
                        true
                    }
                });

                if pass {
                    Some(numbers.clone())
                } else {
                    None
                }
            })
            .collect();

        result
            .iter()
            .map(|numbers| {
                let middle = numbers.len() / 2;

                numbers.get(middle).unwrap()
            })
            .sum()
    }

    fn process_with_adjustment(&self) -> usize {
        let mut invalid: Vec<Vec<usize>> = self
            .page_updates
            .iter()
            .filter_map(|numbers| {
                let pass = numbers.iter().enumerate().all(|(i, &number)| {
                    if let Some(rules) = self.rules.get(&number) {
                        (0..i).all(|y| !rules.contains(&numbers[y]))
                    } else {
                        true
                    }
                });

                if !pass {
                    Some(numbers.clone())
                } else {
                    None
                }
            })
            .collect();

        let result: Vec<Vec<usize>> = invalid
            .iter_mut()
            .map(|numbers| {
                for index in 1..numbers.len() {
                    let value = numbers[index];
                    if let Some(rules) = self.rules.get(&value) {
                        for i in 0..index {
                            if rules.contains(&numbers[i]) {
                                numbers.swap(index, i);
                            }
                        }
                    }
                }
                numbers.clone()
            })
            .collect();

        result
            .iter()
            .map(|numbers| {
                let middle = numbers.len() / 2;
                numbers.get(middle).unwrap()
            })
            .sum()
    }
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input(input);
    let result_part2 = process_input_part_2(input);

    println!("Results -> Part 1: {} | Part 2: {}", result, result_part2);
}

fn process_input(i: &str) -> usize {
    PrinterTable::new(i).process()
}

fn process_input_part_2(i: &str) -> usize {
    PrinterTable::new(i).process_with_adjustment()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47",
        143
    )]
    fn should_get_sum_middle_page_number(#[case] input: &str, #[case] expected: usize) {
        let result = process_input(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "47|53
97|13
97|61
97|47
75|29
61|13
75|53
29|13
97|29
53|29
61|53
97|53
61|29
47|13
75|47
97|75
47|61
75|61
47|29
75|13
53|13

75,47,61,53,29
97,61,53,29,13
75,29,13
75,97,47,61,53
61,13,29
97,13,75,29,47",
        123
    )]
    fn should_get_sum_middle_page_number_part_2(#[case] input: &str, #[case] expected: usize) {
        let result = process_input_part_2(input);
        assert_eq!(result, expected);
    }
}
