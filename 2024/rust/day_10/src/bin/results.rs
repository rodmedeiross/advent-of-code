use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = include_str!("input.txt");
    let result = process_part01(input);
    let result2 = process_part02(input);
    println!("The trailheads is {}, {}", result, result2);
}

fn process_part01(i: &str) -> usize {
    let mut acc = 0;
    let items = i
        .lines()
        .map(|y| {
            y.chars()
                .map(|x| x.to_digit(10).unwrap() as usize)
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<Vec<usize>>>();

    let trailheads = &items
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter().enumerate().filter_map(move |(x, &n)| {
                if n == 0 {
                    Some((x as isize, y as isize))
                } else {
                    None
                }
            })
        })
        .collect::<Vec<(isize, isize)>>();

    for (x, y) in trailheads {
        acc += get_score(&items, *x, *y);
    }

    acc
}

fn process_part02(i: &str) -> usize {
    let mut acc = 0;
    let items = i
        .lines()
        .map(|y| {
            y.chars()
                .map(|x| x.to_digit(10).unwrap() as usize)
                .collect::<Vec<usize>>()
        })
        .collect::<Vec<Vec<usize>>>();

    let trailheads = &items
        .iter()
        .enumerate()
        .flat_map(|(y, row)| {
            row.iter().enumerate().filter_map(move |(x, &n)| {
                if n == 0 {
                    Some((x as isize, y as isize))
                } else {
                    None
                }
            })
        })
        .collect::<Vec<(isize, isize)>>();

    for (x, y) in trailheads {
        acc += get_score_pt2(&items, *x, *y);
    }

    acc
}

fn get_score(map: &Vec<Vec<usize>>, x: isize, y: isize) -> usize {
    let mut queue = VecDeque::from(vec![(x, y)]);
    let mut saw = HashSet::new();
    saw.insert((x, y));

    let mut paths = 0;

    while queue.len() > 0 {
        if let Some((qx, qy)) = queue.pop_back() {
            for (next_x, next_y) in vec![(qx - 1, qy), (qx, qy + 1), (qx + 1, qy), (qx, qy - 1)] {
                if next_x < 0
                    || next_y < 0
                    || next_x >= map[0].len() as isize
                    || next_y >= map.len() as isize
                {
                    continue;
                }

                if map[next_y as usize][next_x as usize] != map[qy as usize][qx as usize] + 1 {
                    continue;
                }

                if saw.contains(&(next_x, next_y)) {
                    continue;
                }

                saw.insert((next_x, next_y));

                if map[next_y as usize][next_x as usize] == 9 {
                    paths += 1
                } else {
                    queue.push_front((next_x, next_y))
                }
            }
        }
    }

    paths
}

fn get_score_pt2(map: &Vec<Vec<usize>>, x: isize, y: isize) -> usize {
    let mut queue = VecDeque::from(vec![(x, y)]);
    let mut saw = HashMap::new();
    saw.insert((x, y), 1);

    let mut paths = 0;

    while queue.len() > 0 {
        if let Some((qx, qy)) = queue.pop_back() {
            if map[qy as usize][qx as usize] == 9 {
                if let Some(n) = saw.get(&(qx, qy)) {
                    paths += n
                }
            }
            for (next_x, next_y) in vec![(qx - 1, qy), (qx, qy + 1), (qx + 1, qy), (qx, qy - 1)] {
                if next_x < 0
                    || next_y < 0
                    || next_x >= map[0].len() as isize
                    || next_y >= map.len() as isize
                {
                    continue;
                }

                if map[next_y as usize][next_x as usize] != map[qy as usize][qx as usize] + 1 {
                    continue;
                }

                let current_paths = saw.get(&(qx, qy)).cloned().unwrap_or(0);

                if saw.contains_key(&(next_x, next_y)) {
                    saw.entry((next_x, next_y))
                        .and_modify(|v| *v += current_paths);
                    continue;
                }

                saw.insert((next_x, next_y), current_paths);

                queue.push_front((next_x, next_y));
            }
        }
    }

    paths
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732",
        36
    )]
    fn process_trailheads(#[case] input: &str, #[case] expected: usize) {
        let result = process_part01(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "89010123
78121874
87430965
96549874
45678903
32019012
01329801
10456732",
        81
    )]
    fn process_trailheads_part2(#[case] input: &str, #[case] expected: usize) {
        let result = process_part02(input);
        assert_eq!(result, expected);
    }
}
