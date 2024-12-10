use std::collections::{HashMap, HashSet};

fn parse(i: &str) -> (HashMap<char, Vec<(usize, usize)>>, (usize, usize)) {
    let map = i
        .lines()
        .enumerate()
        .flat_map(|(y, l)| l.chars().enumerate().map(move |(x, c)| (c, (x, y))))
        .fold(HashMap::new(), |mut map, (c, x_y)| {
            map.entry(c).or_insert_with(Vec::new).push(x_y);
            map
        });

    let mut lines = i.lines();
    let dimensions = (
        lines.clone().count() - 1,
        lines.next().map(|line| line.len() - 1).unwrap_or(0),
    );

    (map, dimensions)
}

fn calculate_nodes_part1(i: HashMap<char, Vec<(usize, usize)>>, dim: (usize, usize)) -> usize {
    let mut nodes: HashSet<(usize, usize)> = HashSet::new();
    i.iter().for_each(|(k, antenna)| match k {
        '.' => {}
        _ => {
            antenna.iter().enumerate().for_each(|(i, &a)| {
                antenna[i + 1..].iter().for_each(|&b| {
                    let dx = (a.0 as i32) - (b.0 as i32);
                    let dy = (a.1 as i32) - (b.1 as i32);

                    let ab_dx = dx.abs() as usize;
                    let ab_dy = dy.abs() as usize;

                    match (dx, dy) {
                        (x, y) if x > 0 && y < 0 => {
                            if let Some(y1) = a.1.checked_sub(ab_dy) {
                                if !antenna.contains(&(a.0 + ab_dx, y1)) {
                                    nodes.insert((a.0 + ab_dx, y1));
                                }
                            }
                            if let Some(x2) = b.0.checked_sub(ab_dx) {
                                if !antenna.contains(&(x2, b.1 + ab_dy)) {
                                    nodes.insert((x2, b.1 + ab_dy));
                                }
                            }
                        }
                        (x, y) if x < 0 && y < 0 => {
                            if let (Some(x1), Some(y1)) =
                                (a.0.checked_sub(ab_dx), a.1.checked_sub(ab_dy))
                            {
                                if !antenna.contains(&(x1, y1)) {
                                    nodes.insert((x1, y1));
                                }
                            }
                            if !antenna.contains(&(b.0 + ab_dx, b.1 + ab_dy)) {
                                nodes.insert((b.0 + ab_dx, b.1 + ab_dy));
                            }
                        }
                        _ => panic!("PICNIC"),
                    }
                })
            });
        }
    });

    nodes
        .iter()
        .filter(|(x, y)| x <= &dim.0 && y <= &dim.1)
        .count()
}

fn calculate_nodes_part2(i: HashMap<char, Vec<(usize, usize)>>, dim: (usize, usize)) -> usize {
    let mut nodes: HashSet<(usize, usize)> = HashSet::new();
    i.iter().for_each(|(k, antenna)| match k {
        '.' => {}
        _ => {
            antenna.iter().for_each(|&a| {
                antenna.iter().for_each(|&b| {
                    if a == b {
                        return;
                    }

                    let dx = (b.0 as i64) - (a.0 as i64);
                    let dy = (b.1 as i64) - (a.1 as i64);

                    let (mut a_x, mut a_y) = (a.0 as i64, a.1 as i64);

                    loop {
                        if !(a_x >= 0 && a_y >= 0 && a_x <= dim.0 as i64 && a_y <= dim.1 as i64) {
                            break;
                        }

                        nodes.insert((a_x as usize, a_y as usize));
                        a_x -= dx;
                        a_y -= dy;
                    }
                })
            });
        }
    });

    nodes.len()
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_part1(input);
    let result2 = process_part2(input);
    println!("Number of Nodes: {}, {}", result, result2);
}

fn process_part1(i: &str) -> usize {
    let (table, dim) = parse(i);
    calculate_nodes_part1(table, dim)
}

fn process_part2(i: &str) -> usize {
    let (table, dim) = parse(i);
    calculate_nodes_part2(table, dim)
}

#[cfg(test)]
mod tests {

    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............",
        14
    )]
    fn should_validade_collinearity(#[case] input: &str, #[case] expected: usize) {
        let result = process_part1(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case(
        "............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............",
        34
    )]
    fn should_validade_collinearity_part2(#[case] input: &str, #[case] expected: usize) {
        let result = process_part2(input);
        assert_eq!(result, expected);
    }
}
