use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = include_str!("input.txt");
    let result = process_part01(input);
    println!("Garden {}", result);
}

fn process_part01(i: &str) -> usize {
    let grid = i
        .lines()
        .map(|line| line.chars().map(|ch| ch).collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut queue: VecDeque<(isize, isize)> = VecDeque::new();
    let mut map: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();

    for (y, line) in grid.iter().enumerate() {
        for (x, ch) in line.iter().enumerate() {
            queue.push_back((x as isize, y as isize));
            map.entry(*ch)
                .or_insert(HashSet::new())
                .insert((x as usize, y as usize));

            while queue.len() > 0 {
                if let Some((qx, qy)) = queue.pop_back() {
                    for (nx, ny) in vec![(qx - 1, qy), (qx, qy + 1), (qx + 1, qy), (qx, qy - 1)] {
                        if nx < 0
                            || ny < 0
                            || nx >= grid[0].len() as isize
                            || ny >= grid.len() as isize
                        {
                            continue;
                        }

                        if grid[ny as usize][nx as usize] != *ch {
                            continue;
                        }

                        if let Some(saw) = map.get(ch) {
                            if saw.contains(&(nx as usize, ny as usize)) {
                                continue;
                            }
                        }

                        map.entry(*ch)
                            .or_insert(HashSet::new())
                            .insert((nx as usize, ny as usize));

                        queue.push_front((nx, ny))
                    }
                }
            }
        }
    }

    dbg!(&map, &per);

    21
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "RRRRIICCFF
RRRRIICCCF
VVRRRCCFFF
VVRCCCJFFF
VVVVCJJCFE
VVIVCCJJEE
VVIIICJJEE
MIIIIIJJEE
MIIISIJEEE
MMMISSJEEE
",
        1930
    )]
    #[case(
        "
OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
",
        772
    )]
    fn process_garden_groups(#[case] input: &str, #[case] expected: usize) {
        let result = process_part01(input);
        assert_eq!(result, expected);
    }
}
