use itertools::Itertools;
use std::collections::{HashMap, HashSet, VecDeque};

fn main() {
    let input = include_str!("input.txt");
    let result = process(input);
    println!("Garden {}, {}", result.0, result.1);
}

fn process(i: &str) -> (usize, usize) {
    let grid = i
        .lines()
        .map(|line| line.chars().map(|ch| ch).collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();

    let mut regions: Vec<HashMap<char, HashSet<(usize, usize)>>> = Vec::new();
    let mut saw: HashSet<(usize, usize)> = HashSet::new();
    let mut map: HashMap<(i32, i32), char> = HashMap::new();

    for (y, line) in grid.iter().enumerate() {
        for (x, &ch) in line.iter().enumerate() {
            if saw.contains(&(x, y)) {
                continue;
            }

            saw.insert((x, y));
            let mut queue = VecDeque::from([(x as isize, y as isize)]);
            let mut region: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();

            region.entry(ch).or_insert_with(HashSet::new).insert((x, y));
            map.entry((x as i32, y as i32)).or_insert(ch);

            while let Some((qx, qy)) = queue.pop_front() {
                for (nx, ny) in [(qx - 1, qy), (qx, qy + 1), (qx + 1, qy), (qx, qy - 1)] {
                    if !(0..grid[0].len() as isize).contains(&nx)
                        || !(0..grid.len() as isize).contains(&ny)
                    {
                        continue;
                    }

                    let (nx, ny) = (nx as usize, ny as usize);

                    if grid[ny][nx] != ch || saw.contains(&(nx, ny)) {
                        continue;
                    }

                    region
                        .entry(ch)
                        .or_insert_with(HashSet::new)
                        .insert((nx, ny));

                    map.entry((nx as i32, ny as i32)).or_insert(ch);

                    saw.insert((nx, ny));
                    queue.push_back((nx as isize, ny as isize));
                }
            }

            regions.push(region);
        }
    }

    let areas: Vec<usize> = regions
        .iter()
        .flat_map(|region| region.values().map(|set| set.len()))
        .collect();

    let mut perimeters: Vec<usize> = Vec::new();

    let mut perimeters_path: Vec<HashMap<char, HashSet<(isize, isize)>>> = Vec::new();

    for sets in regions.iter() {
        for (&k, vs) in sets.iter() {
            let mut perimeter = 0;

            let mut per_path: HashMap<char, HashSet<(isize, isize)>> = HashMap::new();

            for &(x, y) in vs {
                for (dx, dy) in [(-1, 0), (1, 0), (0, -1), (0, 1)] {
                    let nx = x as isize + dx;
                    let ny = y as isize + dy;

                    if nx < 0
                        || ny < 0
                        || nx >= grid[0].len() as isize
                        || ny >= grid.len() as isize
                        || grid[ny as usize][nx as usize] != k
                    {
                        perimeter += 1;

                        per_path
                            .entry(k)
                            .or_insert_with(HashSet::new)
                            .insert((nx, ny));
                    }
                }
            }
            perimeters_path.push(per_path);
            perimeters.push(perimeter);
        }
    }

    let mut sides = Vec::new();

    for sets in regions.iter() {
        let pers = sets
            .iter()
            .map(|(group_id, set)| {
                set.iter()
                    .map(|&n| {
                        let coord = (n.0 as i32, n.1 as i32);
                        calculate_corner_count(&coord, &map, &group_id)
                    })
                    .sum::<usize>()
            })
            .collect::<Vec<usize>>();
        sides.push(pers[0]);
    }

    (
        areas
            .iter()
            .zip(perimeters.iter())
            .map(|(a, b)| a * b)
            .sum(),
        areas.iter().zip(sides.iter()).map(|(a, b)| a * b).sum(),
    )
}

const DIRECTIONS: [[i32; 2]; 4] = [[0, 1], [1, 0], [0, -1], [-1, 0]];
fn calculate_corner_count(
    n: &(i32, i32),
    map: &HashMap<(i32, i32), char>,
    group_id: &char,
) -> usize {
    let mut count = 0;
    for ([x, y], [x1, y1]) in DIRECTIONS.iter().circular_tuple_windows() {
        let test_a = map.get(&(x + n.0, y + n.1)).is_some_and(|c| c == group_id);
        let test_b = map
            .get(&(x1 + n.0, y1 + n.1))
            .is_some_and(|c| c == group_id);
        if test_a
            && test_b
            && map
                .get(&(x + x1 + n.0, y + y1 + n.1))
                .is_some_and(|c| c != group_id)
        {
            count += 1;
        } else if !test_a && !test_b {
            count += 1;
        }
    }
    count
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
        "OOOOO
OXOXO
OOOOO
OXOXO
OOOOO
",
        772
    )]
    fn process_garden_groups(#[case] input: &str, #[case] expected: usize) {
        let result = process(input);
        assert_eq!(result.0, expected);
    }

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
        1206
    )]
    fn process_garden_groups_part02(#[case] input: &str, #[case] expected: usize) {
        let result = process(input);
        assert_eq!(result.1, expected);
    }
}
