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

    let mut regions: Vec<HashMap<char, HashSet<(usize, usize)>>> = Vec::new();
    let mut saw: HashSet<(usize, usize)> = HashSet::new();

    for (y, line) in grid.iter().enumerate() {
        for (x, &ch) in line.iter().enumerate() {
            if saw.contains(&(x, y)) {
                continue;
            }

            saw.insert((x, y));
            let mut queue = VecDeque::from([(x as isize, y as isize)]);
            let mut region: HashMap<char, HashSet<(usize, usize)>> = HashMap::new();

            region.entry(ch).or_insert_with(HashSet::new).insert((x, y));

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

    for sets in regions.iter() {
        for (&k, vs) in sets.iter() {
            let mut perimeter = 0;

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
                    }
                }
            }

            perimeters.push(perimeter);
        }
    }

    areas
        .iter()
        .zip(perimeters.iter())
        .map(|(a, b)| a * b)
        .sum()
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
