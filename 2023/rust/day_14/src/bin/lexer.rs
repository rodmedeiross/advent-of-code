// use itertools::{repeat_n, Itertools};
use std::str::FromStr;

#[derive(Debug, Default)]
pub struct RockSchema {
    rocks: Vec<(usize, usize)>,
    rows: u32,
}

impl RockSchema {
    pub fn calculate_rounded_amount(&self) -> usize {
        let total = self
            .rocks
            .iter()
            .enumerate()
            .map(|(i, (rocks, _))| *rocks as u32 * (self.rows - i as u32))
            .collect::<Vec<_>>();

        // dbg!(&total);

        total.iter().fold(0, |mut total, item| {
            total += *item as usize;
            total
        })
    }
}

impl FromStr for RockSchema {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let rocks: Vec<(usize, usize)> = s
            .lines()
            .map(|line| {
                let rounded_rock = line.chars().filter(|ch| *ch == 'O').count();
                let cube_rocks = line.chars().filter(|ch| *ch == '#').count();

                (rounded_rock, cube_rocks)
            })
            .collect::<Vec<_>>();

        let rows = rocks.iter().count() as u32;

        dbg!(&rows, &rocks);

        Ok(RockSchema { rocks, rows })
    }
}
