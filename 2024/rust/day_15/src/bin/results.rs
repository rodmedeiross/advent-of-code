use std::collections::HashMap;
use std::convert::TryFrom;
use std::str::FromStr;

#[derive(Debug)]
enum Dir {
    Top,
    Down,
    Left,
    Right,
}

#[derive(Debug)]
enum Item {
    Rock,
    Robot,
    Wall,
    Empty,
}

impl Dir {
    fn symbol(&self) -> char {
        match self {
            Dir::Top => '^',
            Dir::Down => 'v',
            Dir::Left => '<',
            Dir::Right => '>',
        }
    }
}

impl TryFrom<char> for Dir {
    type Error = &'static str;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            '^' => Ok(Dir::Top),
            'v' => Ok(Dir::Down),
            '<' => Ok(Dir::Left),
            '>' => Ok(Dir::Right),
            _ => Err("Priviet"),
        }
    }
}

impl Item {
    fn symbol(&self) -> char {
        match self {
            Item::Rock => 'O',
            Item::Wall => '#',
            Item::Robot => '@',
            Item::Empty => '.',
        }
    }
}

impl TryFrom<char> for Item {
    type Error = &'static str;
    fn try_from(c: char) -> Result<Self, Self::Error> {
        match c {
            'O' => Ok(Item::Rock),
            '#' => Ok(Item::Wall),
            '@' => Ok(Item::Robot),
            '.' => Ok(Item::Empty),
            _ => Err("Priviet"),
        }
    }
}

#[derive(Debug)]
struct Map {
    grid: HashMap<(isize, isize), Item>,
    dirs: Vec<Dir>,
}

impl FromStr for Map {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let (grid, dirs) = i
            .split_once("\n\n")
            .and_then(|(table, directions)| {
                let grid = table
                    .lines()
                    .enumerate()
                    .flat_map(|(y, line)| {
                        line.chars().enumerate().map(move |(x, ch)| {
                            dbg!(line, ch);
                            ((x as isize, y as isize), Item::try_from(ch).unwrap())
                        })
                    })
                    .collect::<HashMap<(isize, isize), Item>>();

                let dirs = directions
                    .chars()
                    .filter_map(|ch| {
                        if ch != '\n' {
                            Some(Dir::try_from(ch).unwrap())
                        } else {
                            None
                        }
                    })
                    .collect::<Vec<Dir>>();

                Some((grid, dirs))
            })
            .unwrap();

        Ok(Map { grid, dirs })
    }
}

fn main() {}

fn process_input_p1(i: &str) -> usize {
    let map = i.parse::<Map>().unwrap();
    dbg!(map);
    21
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<",
        10092
    )]
    fn should_walk_and_get_gps(#[case] i: &str, #[case] expected: usize) {
        let result = process_input_p1(i);
        assert_eq!(result, expected);
    }
}
