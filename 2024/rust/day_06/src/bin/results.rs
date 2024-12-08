#![allow(dead_code)]

use std::collections::HashMap;
use std::str::FromStr;

#[derive(Default)]
struct PatrolMap {
    positions: HashMap<(usize, usize), char>,
    walked: HashMap<(usize, usize), char>,
    dimensions: (usize, usize),
}

impl PatrolMap {
    fn new(i: &str) -> Self {
        i.parse::<PatrolMap>().unwrap()
    }

    fn walk_path(&mut self) -> usize {
        let mut initial_pos = find_key(&self.positions, '^');
        let mut initial_direction = Directions::Up;
        self.walked = self.positions.clone();

        for _ in 0..self.dimensions.1 {
            for _ in 0..self.dimensions.0 {
                match initial_direction {
                    Directions::Up => {
                        if let Some((x_pos, mut y_pos)) = initial_pos {
                            if let Some(ch) = self.walked.get_mut(&(x_pos, y_pos)) {
                                *ch = 'x'
                            }
                            if let Some(next) = self.positions.get(&(x_pos, y_pos - 1)) {
                                match next {
                                    '#' => {
                                        initial_direction = Directions::Right;
                                    }
                                    _ => {
                                        y_pos -= 1;
                                        initial_pos = Some((x_pos, y_pos));
                                    }
                                }
                            }
                        }
                    }

                    Directions::Right => {
                        if let Some((mut x_pos, y_pos)) = initial_pos {
                            if let Some(ch) = self.walked.get_mut(&(x_pos, y_pos)) {
                                *ch = 'x'
                            }
                            if let Some(next) = self.positions.get(&(x_pos + 1, y_pos)) {
                                match next {
                                    '#' => {
                                        initial_direction = Directions::Down;
                                    }
                                    _ => {
                                        x_pos += 1;
                                        initial_pos = Some((x_pos, y_pos));
                                    }
                                }
                            }
                        }
                    }

                    Directions::Down => {
                        if let Some((x_pos, mut y_pos)) = initial_pos {
                            if let Some(ch) = self.walked.get_mut(&(x_pos, y_pos)) {
                                *ch = 'x'
                            }
                            if let Some(next) = self.positions.get(&(x_pos, y_pos + 1)) {
                                match next {
                                    '#' => {
                                        initial_direction = Directions::Left;
                                    }
                                    _ => {
                                        y_pos += 1;
                                        initial_pos = Some((x_pos, y_pos));
                                    }
                                }
                            }
                        }
                    }

                    Directions::Left => {
                        if let Some((mut x_pos, y_pos)) = initial_pos {
                            if let Some(ch) = self.walked.get_mut(&(x_pos, y_pos)) {
                                *ch = 'x'
                            }
                            if let Some(next) = self.positions.get(&(x_pos - 1, y_pos)) {
                                match next {
                                    '#' => {
                                        initial_direction = Directions::Up;
                                    }
                                    _ => {
                                        x_pos -= 1;
                                        initial_pos = Some((x_pos, y_pos));
                                    }
                                }
                            }
                        }
                    }
                }
            }
        }

        self.walked = filter_hashmap(&self.walked, 'x');
        self.walked.iter().count()
    }

    fn walk_in_cycle(&mut self) -> bool {
        let mut initial_pos = find_key(&self.positions, '^');
        let mut initial_direction = Directions::Up;
        let mut rocks: HashMap<(usize, usize), char> = HashMap::new();

        loop {
            match initial_pos 
            

        }
        
    }

    fn walk(&self){

    }
}

fn find_key(map: &HashMap<(usize, usize), char>, target: char) -> Option<(usize, usize)> {
    for (pos, ch) in map.iter() {
        if *ch == target {
            return Some(*pos);
        }
    }
    None
}

fn filter_hashmap(map: &HashMap<(usize, usize), char>, ch: char) -> HashMap<(usize, usize), char> {
    map.iter()
        .filter(|(_, v)| **v == ch)
        .map(|(&k, &v)| (k, v))
        .collect::<HashMap<(usize, usize), char>>()
}

#[derive(Debug)]
enum Directions {
    Up,
    Right,
    Down,
    Left,
}

impl FromStr for PatrolMap {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let dimensions = (
            i.lines().collect::<Vec<_>>()[0].len(),
            i.lines().collect::<Vec<_>>().len(),
        );

        let positions: HashMap<(usize, usize), char> = i
            .lines()
            .enumerate()
            .flat_map(|(y, line)| line.chars().enumerate().map(move |(x, ch)| ((x, y), ch)))
            .collect();

        Ok(PatrolMap {
            positions,
            walked: Default::default(),
            dimensions,
        })
    }
}

fn main() {
    let input = include_str!("input.txt");
    let part1 = process_input_part_1(input);
    let part2 = 2;

    println!("Number of positions walked: {}, Rounded: {}", part1, part2);
}

fn process_input_part_1(i: &str) -> usize {
    PatrolMap::new(i).walk_path()
}

fn process_input_part_2(i: &str) -> usize {
    let mut map = PatrolMap::new(i);
    map.walk_path();
    map.walk_in_cycle();
    21
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...",
        41
    )]
    fn should_return_number_visited(#[case] input: &str, #[case] expected: usize) {
        let results = process_input_part_1(input);
        assert_eq!(results, expected);
    }

    #[rstest]
    #[case(
        "....#.....
.........#
..........
..#.......
.......#..
..........
.#..^.....
........#.
#.........
......#...",
        6
    )]
    fn should_return_number_rounded(#[case] input: &str, #[case] expected: usize) {
        let results = process_input_part_2(input);
        assert_eq!(results, expected);
    }
}
