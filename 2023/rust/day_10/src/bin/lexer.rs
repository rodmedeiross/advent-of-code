use std::collections::{HashMap, HashSet};
use std::str::FromStr;

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum PipeType {
    Vertical(char),
    Horizontal(char),
    NorthEast(char),
    NorthWest(char),
    SouthWest(char),
    SouthEast(char),
    Ground(char),
    Start(char),
}

#[derive(Debug, Clone, Copy)]
pub enum Diretion {
    South,
    North,
    East,
    West,
}

#[derive(Debug, Eq, PartialEq)]
enum Status {
    In,
    Out,
}

#[derive(Debug, Clone, Copy)]
pub struct Pipe {
    pipe_symbol: PipeType,
    axis: (i32, i32),
}

#[derive(Debug, Default, Clone)]
pub struct PipeGrid {
    grid: HashMap<(i32, i32), PipeType>,
    pub score: i32,
    input: String,
}

impl PipeGrid {
    pub fn walk(&self) -> usize {
        let (x_start, y_start) = self
            .grid
            .iter()
            .find_map(|(key, value)| (value == &PipeType::Start('S')).then_some(key))
            .expect("FOR DAT");

        let north = (x_start + 0, y_start - 1);

        let north_position: Option<(Diretion, (i32, i32))> = self
            .grid
            .get(&north)
            .is_some_and(|pipe_type| match pipe_type {
                PipeType::Vertical(_) | PipeType::SouthWest(_) | PipeType::SouthEast(_) => true,
                _ => false,
            })
            .then_some((Diretion::South, north));

        let south = (x_start + 0, y_start + 1);
        let south_position = self
            .grid
            .get(&south)
            .is_some_and(|pipe_type| match pipe_type {
                PipeType::Vertical(_) | PipeType::NorthWest(_) | PipeType::NorthEast(_) => true,
                _ => false,
            })
            .then_some((Diretion::North, south));

        let east = (x_start + 1, y_start + 0);
        let east_position = self
            .grid
            .get(&east)
            .is_some_and(|pipe_type| match pipe_type {
                PipeType::Horizontal(_) | PipeType::NorthWest(_) | PipeType::SouthWest(_) => true,
                _ => false,
            })
            .then_some((Diretion::West, east));

        let west = (x_start - 1, y_start + 0);
        let west_position = self
            .grid
            .get(&west)
            .is_some_and(|pipe_type| match pipe_type {
                PipeType::Horizontal(_) | PipeType::NorthEast(_) | PipeType::SouthEast(_) => true,
                _ => false,
            })
            .then_some((Diretion::East, west));

        let next_positions = vec![north_position, south_position, east_position, west_position];

        let mut iters = next_positions.iter().flatten().cloned().map(|tuple| {
            std::iter::successors(Some(tuple), |(from_direction, (x_current, y_current))| {
                let pipe_type = self
                    .grid
                    .get(&(*x_current, *y_current))
                    .expect("Run to the hillss");
                let to_go = match (from_direction, pipe_type) {
                    (Diretion::North, PipeType::NorthEast(_)) => Diretion::East,
                    (Diretion::North, PipeType::NorthWest(_)) => Diretion::West,
                    (Diretion::North, PipeType::Vertical(_)) => Diretion::South,
                    (Diretion::South, PipeType::SouthEast(_)) => Diretion::East,
                    (Diretion::South, PipeType::SouthWest(_)) => Diretion::West,
                    (Diretion::South, PipeType::Vertical(_)) => Diretion::North,
                    (Diretion::East, PipeType::Horizontal(_)) => Diretion::West,
                    (Diretion::East, PipeType::NorthEast(_)) => Diretion::North,
                    (Diretion::East, PipeType::SouthEast(_)) => Diretion::South,
                    (Diretion::West, PipeType::Horizontal(_)) => Diretion::East,
                    (Diretion::West, PipeType::NorthWest(_)) => Diretion::North,
                    (Diretion::West, PipeType::SouthWest(_)) => Diretion::South,
                    _ => unreachable!("Where I Go"),
                };

                Some(match to_go {
                    Diretion::North => (Diretion::South, (*x_current, y_current - 1)),
                    Diretion::South => (Diretion::North, (*x_current, y_current + 1)),
                    Diretion::East => (Diretion::West, (x_current + 1, *y_current)),
                    Diretion::West => (Diretion::East, (x_current - 1, *y_current)),
                })
            })
        });

        let path_a = iters.next().expect("Prayy");
        let path_b = iters.next().expect("Prayy");

        // let final_position = path_a
        //     .zip(path_b)
        //     .position(|(a, b)| a.1 == b.1)
        //     .expect("workkk");

        let mut zip_it = path_a.zip(path_b);
        let mut pipe_locations: HashSet<(i32, i32)> = HashSet::from([(*x_start, *y_start)]);

        while let Some((path_node_a, path_node_b)) = zip_it.next() {
            pipe_locations.insert(path_node_a.1);
            pipe_locations.insert(path_node_b.1);

            if path_node_a.1 == path_node_b.1 {
                break;
            }
        }

        let result = self
            .input
            .lines()
            .enumerate()
            .map(|(y, line)| {
                let mut status = Status::Out;

                line.trim()
                    .chars()
                    .enumerate()
                    .filter(|(x, _)| {
                        let position = (*x as i32, y as i32);
                        let pipe_type = self.grid.get(&position).expect("Running");
                        if pipe_locations.contains(&position) {
                            if [
                                // PipeType::Start('S'),
                                PipeType::Vertical('|'),
                                PipeType::SouthWest('7'),
                                PipeType::SouthEast('F'),
                            ]
                            .contains(pipe_type)
                            {
                                status = match status {
                                    Status::In => Status::Out,
                                    Status::Out => Status::In,
                                };
                            };
                            false
                        } else {
                            match status {
                                Status::In => true,
                                Status::Out => false,
                            }
                        }
                    })
                    .count()
            })
            .sum::<usize>();

        result
        // final_position + 1
    }
}

impl FromStr for PipeGrid {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut pipes: Vec<Pipe> = vec![];
        let mut score = 0; //pip Algo
        let mut crossings = 0;

        for (y_cords, lines) in s.lines().enumerate() {
            for (x_cords, chs) in lines.trim().chars().enumerate() {
                match chs {
                    chs if chs == '|' => {
                        crossings += 1;
                        pipes.push(Pipe {
                            pipe_symbol: PipeType::Vertical(chs),
                            axis: (x_cords as i32, y_cords as i32),
                        });
                    }
                    chs if chs == '-' => pipes.push(Pipe {
                        pipe_symbol: PipeType::Horizontal('_'),
                        axis: (x_cords as i32, y_cords as i32),
                    }),
                    chs if chs == 'L' => pipes.push(Pipe {
                        pipe_symbol: PipeType::NorthEast('L'),
                        axis: (x_cords as i32, y_cords as i32),
                    }),
                    chs if chs == 'J' => pipes.push(Pipe {
                        pipe_symbol: PipeType::NorthWest('J'),
                        axis: (x_cords as i32, y_cords as i32),
                    }),
                    chs if chs == '7' => {
                        crossings += 1;
                        pipes.push(Pipe {
                            pipe_symbol: PipeType::SouthWest('7'),
                            axis: (x_cords as i32, y_cords as i32),
                        })
                    }
                    chs if chs == 'F' => {
                        crossings += 1;
                        pipes.push(Pipe {
                            pipe_symbol: PipeType::SouthEast('F'),
                            axis: (x_cords as i32, y_cords as i32),
                        })
                    }
                    chs if chs == '.' => {
                        if crossings % 2 != 0 {
                            score += 1;
                        };
                        pipes.push(Pipe {
                            pipe_symbol: PipeType::Ground('.'),
                            axis: (x_cords as i32, y_cords as i32),
                        })
                    }
                    chs if chs == 'S' => {
                        crossings += 1;
                        pipes.push(Pipe {
                            pipe_symbol: PipeType::Start('S'),
                            axis: (x_cords as i32, y_cords as i32),
                        })
                    }
                    _ => continue,
                }
            }
        }

        let grid: HashMap<(i32, i32), PipeType> = pipes
            .iter()
            .map(|pipe| (pipe.axis, pipe.pipe_symbol))
            .collect();

        Ok(PipeGrid {
            grid,
            score,
            input: s.to_string(),
        })
    }
}
