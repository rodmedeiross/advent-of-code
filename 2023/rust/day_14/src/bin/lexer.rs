// use itertools::{repeat_n, Itertools};
use std::{collections::BTreeMap, str::FromStr, task::Wake};

#[derive(Debug, Default)]
pub struct RockSchema {
    rocks: BTreeMap<(usize, usize), RockType>,
    rocks_vec: Vec<Vec<char>>,
}

#[derive(Debug, Default)]
pub enum RockType {
    Rounded,
    Cube,
    #[default]
    Empty,
}

fn print_schema(schema: &Vec<Vec<char>>) {
    schema.iter().for_each(|line| {
        let formated = line.iter().map(|ch| ch.to_string()).collect::<Vec<_>>();
        print!("{:?} \n", formated.join(""));
    });
}

impl RockSchema {
    pub fn rolllouutttt(&self) -> usize {
        let mut rocks = self.rocks_vec.clone();

        (0..rocks.len() - 1).for_each(|_| {
            self.rocks_vec
                .clone()
                .into_iter()
                .enumerate()
                .for_each(|(y, row)| {
                    row.into_iter()
                        .enumerate()
                        .for_each(|(x, _)| match rocks[y][x] {
                            '.' if y < rocks.len() - 1 && rocks[y + 1][x] == 'O' => {
                                rocks[y + 1][x] = '.';
                                rocks[y][x] = 'O'
                            }
                            _ch => (),
                        });
                });
        });
        // dbg!(print_schema(&rocks));

        let count = rocks
            .iter()
            .enumerate()
            .map(|(index, lines)| {
                let mult = rocks.len() - index;
                let rounded_count = lines
                    .iter()
                    .filter_map(|x| if *x == 'O' { Some(x) } else { None })
                    .count();

                mult * rounded_count
            })
            .collect::<Vec<_>>();

        // dbg!(&count);

        count.iter().sum()
    }
}

impl FromStr for RockSchema {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rocks =
            s.lines()
                .enumerate()
                .flat_map(|(index, line)| {
                    line.trim().chars().enumerate().into_iter().flat_map(
                        move |(inner_index, ch)| match ch {
                            ch if ch == 'O' => Some(((inner_index, index), RockType::Rounded)),
                            ch if ch == '#' => Some(((inner_index, index), RockType::Cube)),
                            _ => Some(((inner_index, index), RockType::Empty)),
                        },
                    )
                })
                .collect::<BTreeMap<(usize, usize), RockType>>();

        let rocks_vec = s
            .lines()
            .map(|line| line.trim().chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();

        // dbg!(&rocks, &rocks_vec);

        Ok(RockSchema { rocks, rocks_vec })
    }
}
