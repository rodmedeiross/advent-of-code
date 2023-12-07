use rayon::prelude::*;
use std::{ops::Range, str::FromStr};

#[derive(Debug, Default, Clone)]
pub struct GameStatus {
    time_distance: Vec<(Range<usize>, usize)>,
}

impl GameStatus {
    pub fn new(input: &str) -> Self {
        input.parse::<GameStatus>().unwrap()
    }

    pub fn get_win_rate(&self) -> Vec<(Vec<usize>, usize)> {
        self.time_distance
            .par_iter()
            .filter_map(|(range, distance)| {
                let valid_range: Vec<usize> = range
                    .clone()
                    .filter_map(|element| {
                        let speed_rate = element;
                        let rest = range.end - speed_rate;
                        if speed_rate * rest > range.end && speed_rate * rest > *distance {
                            Some(element)
                        } else {
                            None
                        }
                    })
                    .collect();

                if !valid_range.is_empty() {
                    Some((valid_range, *distance))
                } else {
                    None
                }
            })
            .collect()
    }
}

impl FromStr for GameStatus {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed_game = s
            .lines()
            .into_iter()
            .map(|line| {
                line.split_once(':')
                    .into_iter()
                    .flat_map(|(_, x)| {
                        x.trim()
                            .split_whitespace() // part01 replace instead
                            // .replace(" ", "").parse::<usize>().ok()
                            .filter_map(|ch| ch.parse::<usize>().ok())
                    })
                    .collect::<Vec<usize>>()
            })
            .collect::<Vec<Vec<usize>>>();

        let time_distance = {
            let (time, distance) = parsed_game.split_at(1);

            let time_values: Vec<Range<usize>> = time[0]
                .iter()
                .scan(0..0, |state, &num| Some(state.start..num))
                .collect();
            // Its not necessary, buy I need learn more aboute Iter

            let distance_values: Vec<usize> = distance[0].to_vec();

            time_values
                .into_iter()
                .zip(distance_values)
                .collect::<Vec<(Range<usize>, usize)>>()
        };

        Ok(GameStatus { time_distance })
    }
}
