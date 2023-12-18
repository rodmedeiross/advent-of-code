// use itertools::{repeat_n, Itertools};
use indicatif::ProgressIterator;
use std::{
    collections::BTreeMap,
    collections::{HashMap, HashSet},
    str::FromStr,
};

#[derive(Debug, Default)]
pub struct RockSchema {
    rocks_vec: Vec<Vec<char>>,
}

fn print_schema(schema: &Vec<Vec<char>>) {
    schema.iter().for_each(|line| {
        let formated = line.iter().map(|ch| ch.to_string()).collect::<Vec<_>>();
        print!("{:?} \n", formated.join(""));
    });
}

#[derive(Debug, Default, Eq)]
pub struct State {
    step: usize,
    grid: Vec<Vec<char>>,
}

impl State {
    fn new(step: usize, grid: Vec<Vec<char>>) -> Self {
        Self { step, grid }
    }
}

impl PartialEq for State {
    fn eq(&self, other: &Self) -> bool {
        self.grid == other.grid
    }
}

impl std::hash::Hash for State {
    fn hash<H: std::hash::Hasher>(&self, state: &mut H) {
        self.grid.hash(state);
    }
}

fn north_rollout(
    original_map: &Vec<Vec<char>>,
    output_map: &mut Vec<Vec<char>>,
    boundaries: usize,
    cache: &mut HashMap<String, Vec<Vec<char>>>,
) {
    let hash = output_map
        .clone()
        .into_iter()
        .map(|linha| {
            linha
                .into_iter()
                .map(|ch| ch.to_string())
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("");

    if let Some(cache_value) = cache.get(&hash) {
        *output_map = cache_value.to_vec();
    }

    (0..boundaries).for_each(|_| {
        original_map.iter().enumerate().for_each(|(y, row)| {
            row.into_iter()
                .enumerate()
                .for_each(|(x, _)| match output_map[y][x] {
                    '.' if y < output_map.len() - 1 && output_map[y + 1][x] == 'O' => {
                        output_map[y + 1][x] = '.';
                        output_map[y][x] = 'O'
                    }
                    _ch => (),
                });
        });
    });

    cache.insert(hash, output_map.clone());
}

fn west_rollout(
    original_map: &Vec<Vec<char>>,
    output_map: &mut Vec<Vec<char>>,
    boundaries: usize,
    cache: &mut HashMap<String, Vec<Vec<char>>>,
) {
    let hash = output_map
        .clone()
        .into_iter()
        .map(|linha| {
            linha
                .into_iter()
                .map(|ch| ch.to_string())
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("");

    if let Some(cache_value) = cache.get(&hash) {
        *output_map = cache_value.to_vec();
    }

    (0..boundaries).for_each(|_| {
        original_map.iter().enumerate().for_each(|(y, row)| {
            row.into_iter()
                .enumerate()
                .for_each(|(x, _)| match output_map[y][x] {
                    '.' if x < output_map[0].len() - 1 && output_map[y][x + 1] == 'O' => {
                        output_map[y][x + 1] = '.';
                        output_map[y][x] = 'O'
                    }
                    _ch => (),
                });
        });
    });

    cache.insert(hash, output_map.clone());
}

fn south_rollout(
    original_map: &Vec<Vec<char>>,
    output_map: &mut Vec<Vec<char>>,
    boundaries: usize,
    cache: &mut HashMap<String, Vec<Vec<char>>>,
) {
    let hash = output_map
        .clone()
        .into_iter()
        .map(|linha| {
            linha
                .into_iter()
                .map(|ch| ch.to_string())
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("");

    if let Some(cache_value) = cache.get(&hash) {
        *output_map = cache_value.to_vec();
    }

    (0..boundaries).for_each(|_| {
        original_map.iter().enumerate().for_each(|(y, row)| {
            row.into_iter()
                .enumerate()
                .for_each(|(x, _)| match output_map[y][x] {
                    'O' if y < output_map.len() - 1 && output_map[y + 1][x] == '.' => {
                        output_map[y + 1][x] = 'O';
                        output_map[y][x] = '.'
                    }
                    _ch => (),
                });
        });
    });
    cache.insert(hash, output_map.clone());
}

fn east_rollout(
    original_map: &Vec<Vec<char>>,
    output_map: &mut Vec<Vec<char>>,
    boundaries: usize,
    cache: &mut HashMap<String, Vec<Vec<char>>>,
) {
    let hash = output_map
        .clone()
        .into_iter()
        .map(|linha| {
            linha
                .into_iter()
                .map(|ch| ch.to_string())
                .collect::<Vec<_>>()
                .join("")
        })
        .collect::<Vec<_>>()
        .join("");

    if let Some(cache_value) = cache.get(&hash) {
        *output_map = cache_value.to_vec();
    }

    (0..boundaries).for_each(|_| {
        original_map.iter().enumerate().for_each(|(y, row)| {
            row.into_iter()
                .enumerate()
                .for_each(|(x, _)| match output_map[y][x] {
                    'O' if x < output_map[0].len() - 1 && output_map[y][x + 1] == '.' => {
                        output_map[y][x + 1] = 'O';
                        output_map[y][x] = '.'
                    }
                    _ch => (),
                });
        });
    });

    cache.insert(hash, output_map.clone());
}

impl RockSchema {
    pub fn rolllouutttt(&self) -> usize {
        let mut rocks = self.rocks_vec.clone();
        let y_length = rocks.len() - 1;
        let x_length = rocks[0].len() - 1;
        let mut cache: HashMap<String, Vec<Vec<char>>> = HashMap::new();
        let mut result_cache = HashSet::new();

        for iter in 0..1_000_000_000 {
            result_cache.insert(State::new(iter, rocks.clone()));

            north_rollout(&self.rocks_vec, &mut rocks, y_length, &mut cache);
            west_rollout(&self.rocks_vec, &mut rocks, x_length, &mut cache);
            south_rollout(&self.rocks_vec, &mut rocks, y_length, &mut cache);
            east_rollout(&self.rocks_vec, &mut rocks, x_length, &mut cache);

            if let Some(state) = result_cache.get(&State::new(0, rocks.to_owned())) {
                let cycle_len = iter + 1 - state.step;
                let remaining = 1_000_000_000 - iter - 1;
                let remaining = remaining % cycle_len;

                for _ in 0..remaining {
                    north_rollout(&self.rocks_vec, &mut rocks, y_length, &mut cache);
                    west_rollout(&self.rocks_vec, &mut rocks, x_length, &mut cache);
                    south_rollout(&self.rocks_vec, &mut rocks, y_length, &mut cache);
                    east_rollout(&self.rocks_vec, &mut rocks, x_length, &mut cache);
                }

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
                return count.iter().sum();
            }
        }
        panic!("So lonng")
    }
}

impl FromStr for RockSchema {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rocks_vec = s
            .lines()
            .map(|line| line.trim().chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();

        Ok(RockSchema { rocks_vec })
    }
}
