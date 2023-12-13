use std::cmp::{max, min};
use std::collections::BTreeMap;
use std::str::FromStr;

#[derive(Debug, Default, PartialEq)]
pub enum Astro {
    #[default]
    Empyt,
    Galaxy(char),
}

#[derive(Debug, Default)]
pub struct Universe {
    astros: BTreeMap<(i32, i32), Astro>,
    empyt_rows: Vec<i32>,
    empyt_columns: Vec<i32>,
}

// #[derive(Debug, Default)]
// pub struct Point {
//     x: i32,
//     y: i32,
// }

impl Universe {
    pub fn calculate_distance(&self) -> i64 {
        let distances = self
            .astros
            .iter()
            .enumerate()
            .flat_map(|(index, (current_galaxy, _))| {
                self.astros
                    .iter()
                    .skip(index + 1)
                    .map(move |(next_galaxy, _)| {
                        let start_row = min(current_galaxy.1, next_galaxy.1);
                        let start_column = min(current_galaxy.0, next_galaxy.0);
                        let end_row = max(current_galaxy.1, next_galaxy.1);
                        let end_column = max(current_galaxy.0, next_galaxy.0);

                        let offset_columns = self
                            .empyt_columns
                            .iter()
                            .filter(|c| start_column < **c && **c < end_column)
                            .count() as i64
                            * (1000000 - 1);

                        let offset_rows = self
                            .empyt_rows
                            .iter()
                            .filter(|r| start_row < **r && **r < end_row)
                            .count() as i64
                            * (1000000 - 1);

                        let distance = (end_row as i64 - start_row as i64)
                            + (end_column as i64 - start_column as i64)
                            + offset_columns
                            + offset_rows;

                        distance
                    })
            })
            .collect::<Vec<_>>();

        distances.iter().sum()
    }
}

impl FromStr for Universe {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut astros: BTreeMap<(i32, i32), Astro> = BTreeMap::new();

        for (y, lines) in s.lines().into_iter().enumerate() {
            for (x, character) in lines.trim().chars().enumerate() {
                match character {
                    character if character == '#' => {
                        astros.insert((x as i32, y as i32), Astro::Galaxy(character));
                    }
                    _ => {
                        // astros.insert((x as i32, y as i32), Astro::Empyt);
                        continue;
                    }
                }
            }
        }

        let grid = s
            .lines()
            .map(|line| line.trim().chars().collect::<Vec<_>>())
            .collect::<Vec<_>>();

        // let galaxys = &grid
        //     .iter()
        //     .enumerate()
        //     .flat_map(|(y, line)| {
        //         line.iter()
        //             .enumerate()
        //             .filter(|(_, column)| **column == '#')
        //             .map(move |(x, _)| Point {
        //                 x: x as i32,
        //                 y: y as i32,
        //             })
        //     })
        //     .collect::<Vec<_>>();

        let empyt_rows = grid
            .iter()
            .enumerate()
            .filter(|(_, row)| row.iter().all(|ch| *ch == '.'))
            .map(|(y, _)| y as i32)
            .collect::<Vec<_>>();

        let empyt_columns = (0..grid[0].len())
            .filter(|x| grid.iter().all(|row| row[*x] == '.'))
            .map(|x| x as i32)
            .collect::<Vec<_>>();

        Ok(Universe {
            astros,
            empyt_rows,
            empyt_columns,
        })
    }
}
