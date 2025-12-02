use std::convert::TryFrom;
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
    grid: Vec<Vec<Item>>,
    dirs: Vec<Dir>,
}

impl Map {
    fn walk(&mut self) {
        let mut rp = find(&self.grid, Item::Robot).unwrap();

        for dir in &self.dirs {
            match dir {
                Dir::Right => {
                    if rp.0 + 1 <= self.grid[rp.1].len() {
                        let next = self.grid[rp.1][rp.0 + 1];

                        match next {
                            Item::Empty => {
                                self.grid[rp.1][rp.0 + 1] = Item::Robot;
                                self.grid[rp.1][rp.0] = Item::Empty;
                                rp = (rp.0 + 1, rp.1)
                            }
                            Item::Wall => {
                                //Nothing
                            }
                            Item::Rock => {
                                let (mut ix, iy) = (rp.0 + 2, rp.1);
                                let mut inner_next = self.grid[iy][ix];
                                while inner_next != Item::Empty {
                                    if inner_next == Item::Wall {
                                        ix = rp.0 + 1;
                                        break;
                                    }

                                    ix += 1;
                                    inner_next = self.grid[iy][ix];
                                }
                                //swap last inner_next(empty) with next
                                self.grid[rp.1][rp.0 + 1] = Item::Empty;
                                self.grid[iy][ix] = Item::Rock; // put rock back with the same pos

                                if self.grid[rp.1][rp.0 + 1] == Item::Empty {
                                    self.grid[rp.1][rp.0 + 1] = Item::Robot;
                                    self.grid[rp.1][rp.0] = Item::Empty;
                                    rp = (rp.0 + 1, rp.1)
                                }
                            }
                            _ => panic!("Priviet"),
                        }
                    }
                }
                Dir::Left => {
                    if rp.0 as isize - 1 >= 0 {
                        let next = self.grid[rp.1][rp.0 - 1];

                        match next {
                            Item::Empty => {
                                self.grid[rp.1][rp.0 - 1] = Item::Robot;
                                self.grid[rp.1][rp.0] = Item::Empty;
                                rp = (rp.0 - 1, rp.1)
                            }
                            Item::Wall => {
                                //Nothing
                            }
                            Item::Rock => {
                                let (mut ix, iy) = (rp.0 - 2, rp.1);
                                let mut inner_next = self.grid[iy][ix];
                                while inner_next != Item::Empty {
                                    if inner_next == Item::Wall {
                                        ix = rp.0 - 1;
                                        break;
                                    }

                                    ix -= 1;
                                    inner_next = self.grid[iy][ix];
                                }
                                //swap last inner_next(empty) with next
                                self.grid[rp.1][rp.0 - 1] = Item::Empty;
                                self.grid[iy][ix] = Item::Rock;

                                if self.grid[rp.1][rp.0 - 1] == Item::Empty {
                                    self.grid[rp.1][rp.0 - 1] = Item::Robot;
                                    self.grid[rp.1][rp.0] = Item::Empty;
                                    rp = (rp.0 - 1, rp.1)
                                }
                            }
                            _ => panic!("Priviet"),
                        }
                    }
                }
                Dir::Top => {
                    if rp.1 as isize - 1 >= 0 {
                        let next = self.grid[rp.1 - 1][rp.0];

                        match next {
                            Item::Empty => {
                                self.grid[rp.1 - 1][rp.0] = Item::Robot;
                                self.grid[rp.1][rp.0] = Item::Empty;
                                rp = (rp.0, rp.1 - 1)
                            }
                            Item::Wall => {
                                //Nothing
                            }
                            Item::Rock => {
                                let (ix, mut iy) = (rp.0, rp.1 - 2);
                                let mut inner_next = self.grid[iy][ix];
                                while inner_next != Item::Empty {
                                    if inner_next == Item::Wall {
                                        iy = rp.1 - 1;
                                        break;
                                    }

                                    iy -= 1;
                                    inner_next = self.grid[iy][ix];
                                }
                                //swap last inner_next(empty) with next
                                self.grid[rp.1 - 1][rp.0] = Item::Empty;
                                self.grid[iy][ix] = Item::Rock;

                                if self.grid[rp.1 - 1][rp.0] == Item::Empty {
                                    self.grid[rp.1 - 1][rp.0] = Item::Robot;
                                    self.grid[rp.1][rp.0] = Item::Empty;
                                    rp = (rp.0, rp.1 - 1)
                                }
                            }
                            _ => panic!("Priviet"),
                        }
                    }
                }
                Dir::Down => {
                    if rp.1 + 1 <= self.grid.len() {
                        let next = self.grid[rp.1 + 1][rp.0];

                        match next {
                            Item::Empty => {
                                self.grid[rp.1 + 1][rp.0] = Item::Robot;
                                self.grid[rp.1][rp.0] = Item::Empty;
                                rp = (rp.0, rp.1 + 1)
                            }
                            Item::Wall => {
                                //Nothing
                            }
                            Item::Rock => {
                                let (ix, mut iy) = (rp.0, rp.1 + 2);
                                let mut inner_next = self.grid[iy][ix];
                                while inner_next != Item::Empty {
                                    if inner_next == Item::Wall {
                                        iy = rp.1 + 1;
                                        break;
                                    }

                                    iy += 1;
                                    inner_next = self.grid[iy][ix];
                                }
                                //swap last inner_next(empty) with next
                                self.grid[rp.1 + 1][rp.0] = Item::Empty;
                                self.grid[iy][ix] = Item::Rock;

                                if self.grid[rp.1 + 1][rp.0] == Item::Empty {
                                    self.grid[rp.1 + 1][rp.0] = Item::Robot;
                                    self.grid[rp.1][rp.0] = Item::Empty;
                                    rp = (rp.0, rp.1 + 1)
                                }
                            }
                            _ => panic!("Priviet"),
                        }
                    }
                }
            }
        }
    }

    fn print_grid(&self) {
        for line in &self.grid {
            let fl = line.iter().map(|i| i.symbol()).collect::<String>();
            print!("{:?}\n", fl);
        }
        println!("\n")
    }

    fn calc_gps_p1(&self) -> usize {
        let mut result = 0;
        for (y, line) in self.grid.iter().enumerate() {
            for (x, item) in line.iter().enumerate() {
                if *item == Item::Rock {
                    result += (100 * y) + x
                }
            }
        }

        result
    }
}

impl FromStr for Map {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let (grid, dirs) = i
            .split_once("\n\n")
            .and_then(|(table, directions)| {
                let grid = table
                    .lines()
                    .map(|line| {
                        line.chars()
                            .map(|ch| Item::try_from(ch).unwrap())
                            .collect::<Vec<Item>>()
                    })
                    .collect::<Vec<Vec<Item>>>();

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

fn find<T: PartialEq>(v: &Vec<Vec<T>>, target: T) -> Option<(usize, usize)> {
    v.iter().enumerate().find_map(|(y, l)| {
        l.iter()
            .enumerate()
            .find(|(_, item)| **item == target)
            .map(|(x, _)| (x, y))
    })
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input_p1(input);
    println!("Result to {}", result);
}

fn process_input_p1(i: &str) -> usize {
    let mut map = i.parse::<Map>().unwrap();
    map.print_grid();
    map.walk();
    map.print_grid();
    map.calc_gps_p1()
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
        10092
    )]
    fn should_walk_and_get_gps(#[case] i: &str, #[case] expected: usize) {
        let result = process_input_p1(i);
        assert_eq!(result, expected);
    }
}
