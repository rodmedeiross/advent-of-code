use std::collections::HashMap;
use std::convert::TryFrom;
use std::mem::swap;
use std::str::FromStr;

#[derive(Debug, Copy, Clone)]
enum Dir {
    Top,
    Down,
    Left,
    Right,
}

#[derive(Debug, PartialEq, Clone, Copy)]
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
    dim: (isize, isize),
}

impl Map {
    fn walk(&mut self) -> &Self {
        let (mut rx, mut ry) = self.get_robot();

        for dir in self.dirs.clone() {
            let coord = self.push((rx, ry), dir);
            dbg!(coord);
        }

        self
    }

    fn push(&mut self, pos: (isize, isize), dir: Dir) -> (isize, isize) {
        if let Some(empty) = self.grid.get(&pos) {
            if *empty == Item::Empty {
                return pos;
            }
        }

        if pos.1 + 1 >= self.dim.1 || pos.1 - 1 <= 0 || pos.0 + 1 >= self.dim.0 || pos.0 - 1 <= 0 {
            return pos;
        }

        let new = match dir {
            Dir::Top => (pos.0, pos.1 - 1),
            Dir::Down => (pos.0, pos.1 + 1),
            Dir::Left => (pos.0 - 1, pos.1),
            Dir::Right => (pos.0 + 1, pos.1),
        };

        let f = self.push(new, dir);

        f
    }

    fn get_robot(&self) -> (isize, isize) {
        self.grid
            .iter()
            .find(|&(_, v)| *v == Item::Robot)
            .map(|(&cord, _)| cord)
            .unwrap()
    }
}

impl FromStr for Map {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let (grid, dirs, dim) = i
            .split_once("\n\n")
            .and_then(|(table, directions)| {
                let grid = table
                    .lines()
                    .enumerate()
                    .flat_map(|(y, line)| {
                        line.chars().enumerate().map(move |(x, ch)| {
                            ((x as isize, y as isize), Item::try_from(ch).unwrap())
                        })
                    })
                    .collect::<HashMap<(isize, isize), Item>>();

                let tmp: Vec<&str> = table.lines().collect();
                let dim = (tmp[0].chars().count() as isize, tmp.len() as isize);

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

                Some((grid, dirs, dim))
            })
            .unwrap();

        Ok(Map { grid, dirs, dim })
    }
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input_p1(input);
    println!("Result to {}", result);
}

fn process_input_p1(i: &str) -> usize {
    let mut map = i.parse::<Map>().unwrap();
    map.walk();
    // dbg!(map);
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
        2028
    )]
    #[case(
        "##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^",
        100092
    )]
    fn should_walk_and_get_gps(#[case] i: &str, #[case] expected: usize) {
        let result = process_input_p1(i);
        assert_eq!(result, expected);
    }
}
