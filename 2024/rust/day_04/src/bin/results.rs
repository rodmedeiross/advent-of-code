use std::collections::HashMap;
use std::str::FromStr;

struct WordTable {
    parsed: Vec<Vec<char>>,
    word_number: Option<usize>,
}

struct CharSet {
    ch: char,
    dir: (Direction, Direction),
}

impl WordTable {
    fn new(i: &str) -> Self {
        i.parse::<WordTable>().unwrap()
    }

    fn get_xmas(&mut self) -> &Self {
        let mut count = 0;

        for (y, line) in self.parsed.iter().enumerate() {
            for (x, _) in line.iter().enumerate() {
                let mut verify_result: Vec<bool> = Vec::new();

                verify_result.push(self.search((Direction::Up, Direction::Up), (x, y)));
                verify_result.push(self.search((Direction::Up, Direction::Right), (x, y)));
                verify_result.push(self.search((Direction::Right, Direction::Right), (x, y)));
                verify_result.push(self.search((Direction::Down, Direction::Right), (x, y)));
                verify_result.push(self.search((Direction::Down, Direction::Down), (x, y)));
                verify_result.push(self.search((Direction::Down, Direction::Left), (x, y)));
                verify_result.push(self.search((Direction::Left, Direction::Left), (x, y)));
                verify_result.push(self.search((Direction::Left, Direction::Up), (x, y)));

                count += verify_result.iter().filter(|&&x| x).count();
            }
        }

        self.word_number = Some(count);
        self
    }

    fn search(&self, dir: (Direction, Direction), position: (usize, usize)) -> bool {
        let dict = vec!['X', 'M', 'A', 'S'];

        let (x, y) = position;

        let mut chs: Vec<CharSet> = Vec::new();

        for i in 0..dict.len() {
            if let Some((ch, (dir1, dir2))) = match dir {
                (Direction::Up, Direction::Up) => {
                    if y >= i {
                        Some((self.parsed[y - i][x], (Direction::Up, Direction::Up)))
                    } else {
                        None
                    }
                }
                (Direction::Up, Direction::Right) | (Direction::Right, Direction::Up) => {
                    if y >= i && (x + i) < self.parsed[0].len() {
                        Some((self.parsed[y - i][x + i], (Direction::Up, Direction::Right)))
                    } else {
                        None
                    }
                }
                (Direction::Right, Direction::Right) => {
                    if (x + i) < self.parsed[0].len() {
                        Some((self.parsed[y][x + i], (Direction::Right, Direction::Right)))
                    } else {
                        None
                    }
                }
                (Direction::Down, Direction::Right) | (Direction::Right, Direction::Down) => {
                    if (y + i) < self.parsed.len() && (x + i) < self.parsed[0].len() {
                        Some((
                            self.parsed[y + i][x + i],
                            (Direction::Down, Direction::Right),
                        ))
                    } else {
                        None
                    }
                }
                (Direction::Down, Direction::Down) => {
                    if (y + i) < self.parsed.len() {
                        Some((self.parsed[y + i][x], (Direction::Down, Direction::Down)))
                    } else {
                        None
                    }
                }
                (Direction::Down, Direction::Left) | (Direction::Left, Direction::Down) => {
                    if (y + i) < self.parsed.len() && x >= i {
                        Some((
                            self.parsed[y + i][x - i],
                            (Direction::Down, Direction::Left),
                        ))
                    } else {
                        None
                    }
                }
                (Direction::Left, Direction::Left) => {
                    if x >= i {
                        Some((self.parsed[y][x - i], (Direction::Left, Direction::Left)))
                    } else {
                        None
                    }
                }
                (Direction::Up, Direction::Left) | (Direction::Left, Direction::Up) => {
                    if x >= i && y >= i {
                        Some((self.parsed[y - i][x - i], (Direction::Up, Direction::Left)))
                    } else {
                        None
                    }
                }
                _ => None,
            } {
                if ch != dict[i] {
                    return false;
                }
                // println!("Index {}, {} -> {:?}", dict[i], dict[3 - i], ch);
                // println!("X:{} Y:{} -> {} -> Index: {}", &x, &y, &i, &i_ch);

                chs.push(CharSet {
                    ch,
                    dir: (dir1, dir2),
                });
            } else {
                return false;
            }
        }

        // println!("Index {}, {} ->   {:?}", x, y, chs);
        true
    }

    fn get_xmax(&self) -> i32 {
        let mut count = 0;
        let rows = self.parsed.len();

        for (y, line) in self.parsed.iter().enumerate() {
            let cols = line.len();

            for (x, ch) in line.iter().enumerate() {
                if *ch == 'A' {
                    let diagonals = [(1, -1), (1, 1), (-1, 1), (-1, -1)];
                    let mut cross_number = 0;

                    let mut hash: HashMap<(isize, isize), char> = HashMap::new();

                    for &(dy, dx) in &diagonals {
                        let ny = y as isize + dy;
                        let nx = x as isize + dx;

                        if ny >= 0 && ny < rows as isize && nx >= 0 && nx < cols as isize {
                            let ny = ny as usize;
                            let nx = nx as usize;

                            if self.parsed[ny][nx] == 'M' || self.parsed[ny][nx] == 'S' {
                                cross_number += 1;
                                hash.entry((dy, dx)).or_insert(self.parsed[ny][nx]);
                            }
                        }
                    }

                    //
                    //M.S
                    //.A.
                    //S.M
                    //
                    // -> S M S M
                    // -> S M M S
                    // -> M S M S
                    // -> S M M S
                    // -> M S S M
                    // -> M M S S

                    // println!("{:?}", chs);
                    if cross_number == 4
                        && hash[&(1, -1)] != hash[&(-1, 1)]
                        && hash[&(1, 1)] != hash[&(-1, -1)]
                    {
                        count += 1;
                    }
                }
            }
        }
        count
    }
}

#[derive(Debug)]
enum Direction {
    Up,
    Down,
    Left,
    Right,
}

impl FromStr for WordTable {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let parsed = s
            .lines()
            .map(|lines| lines.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();
        Ok(WordTable {
            parsed,
            word_number: None,
        })
    }
}

fn process_input(i: &str) -> usize {
    WordTable::new(i).get_xmas().word_number.unwrap()
}

fn process_input_xmas(i: &str) -> usize {
    WordTable::new(i).get_xmax() as usize
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_input(input);
    let result2 = process_input_xmas(input);

    println!("Number of XMAS: {}", result);
    println!("Number of X-MAS: {}", result2);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX",
        18
    )]
    fn should_get_number_occurrence(#[case] input: &str, #[case] expected: usize) {
        let result = process_input(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "MMMSXXMASM
MSAMXMSMSA
AMXSXMAAMM
MSAMASMSMX
XMASAMXAMM
XXAMMXXAMA
SMSMSASXSS
SAXAMASAAA
MAMMMXMMMM
MXMXAXMASX",
        9
    )]
    fn should_get_number_occurrence_xmas(#[case] input: &str, #[case] expected: usize) {
        let result = process_input_xmas(input);
        assert_eq!(result, expected);
    }
}
