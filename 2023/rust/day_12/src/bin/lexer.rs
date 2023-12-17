use itertools::{repeat_n, Itertools};
use std::str::FromStr;

#[derive(Debug, Default)]
pub struct Puzzle {
    batches: Vec<u32>,
    spaces_questions: u32,
    line: String,
}

#[derive(Debug, Default)]
pub struct GridPuzzle {
    pub puzzles: Vec<Puzzle>,
}

impl Puzzle {
    pub fn generate_combinations(&self) -> impl Iterator<Item = String> {
        repeat_n([".", "#"].into_iter(), self.spaces_questions as usize)
            .multi_cartesian_product()
            .map(|item| item.join(""))
    }

    pub fn check_options(&self, options: &str) -> bool {
        let mut option_iter = options.chars();

        let filled_options = self
            .line
            .chars()
            .map(|c| match c {
                '?' => option_iter.next().expect("maybe some"),
                value => value,
            })
            .collect::<String>();

        let counts = filled_options
            .chars()
            .group_by(|x| x == &'#')
            .into_iter()
            .filter_map(|(hashes, group)| hashes.then_some(group.into_iter().count() as u32))
            .collect::<Vec<u32>>();

        &self.batches[..] == &counts[..]
    }
}

impl FromStr for GridPuzzle {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let puzzles = s
            .lines()
            .map(|line| {
                let (_, batch_vec) = line
                    .trim()
                    .split_once(' ')
                    .map(|(left, right)| {
                        let chars: Vec<_> = left.chars().map(|c| c).collect();
                        let batches: Vec<_> = right
                            .split(',')
                            .map(|c| c.parse::<u32>().unwrap())
                            .collect();

                        (chars, batches)
                    })
                    .unwrap();

                // let mut chars_to_mult: Vec<_> = chars_vec.clone();
                // let batches_to_mult = batch_vec.clone();

                // chars_to_mult.insert(0, '?');

                // (1..5).for_each(|_| {
                //     chars_vec.extend_from_slice(&chars_to_mult);
                //     batch_vec.extend_from_slice(&batches_to_mult);
                // });

                // let new_line = format!(
                //     "{} {}",
                //     chars_vec.iter().join(""),
                //     batch_vec.iter().join(",")
                // );

                let new_line = std::iter::repeat(line).take(5).join("?");

                let spaces = new_line.chars().filter(|c| c == &'?').count() as u32;

                Puzzle {
                    batches: std::iter::repeat(batch_vec).take(5).flatten().collect(),
                    spaces_questions: spaces,
                    line: new_line,
                }
            })
            .collect::<Vec<Puzzle>>();
        Ok(GridPuzzle { puzzles })
    }
}
