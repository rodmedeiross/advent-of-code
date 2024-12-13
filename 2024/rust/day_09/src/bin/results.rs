use itertools::Itertools;
use std::collections::HashSet;
use std::iter::repeat;

fn parse(i: &str) -> Vec<isize> {
    i.chars()
        .chunks(2)
        .into_iter()
        .enumerate()
        .flat_map(|(i, chunk)| {
            let chk: Vec<char> = chunk.collect();

            let ids = repeat(i)
                .take(gbi(0, &chk))
                .map(|x| x as isize)
                .collect::<Vec<isize>>();

            let free_space = repeat(-1 as isize)
                .take(gbi(1, &chk))
                .collect::<Vec<isize>>();

            ids.into_iter().chain(free_space.into_iter())
        })
        .collect::<Vec<isize>>()
}

fn parse_chunks(i: &str) -> Vec<Vec<isize>> {
    i.chars()
        .chunks(2)
        .into_iter()
        .enumerate()
        .flat_map(|(i, chunk)| {
            let chk: Vec<char> = chunk.collect();

            let ids = repeat(i)
                .take(gbi(0, &chk))
                .map(|x| x as isize)
                .collect::<Vec<isize>>();

            let free_space = repeat(-1 as isize)
                .take(gbi(1, &chk))
                .collect::<Vec<isize>>();

            [ids, free_space]
        })
        .filter(|x| x.len() > 0)
        .collect::<Vec<Vec<isize>>>()
}

fn gbi(i: usize, chunk: &Vec<char>) -> usize {
    match chunk.get(i).copied() {
        Some(c) => {
            if c != '\n' {
                c.to_digit(10).unwrap() as usize
            } else {
                0
            }
        }
        None => 0,
    }
}

fn get_arrange(input: Vec<isize>) -> HashSet<(usize, usize)> {
    let mut arrange = HashSet::new();
    let mut i = 0;
    let mut k = input.len() - 1;

    while i <= k {
        if input[i] == -1 && input[k] != -1 {
            arrange.insert((i, input[k] as usize));
            k -= 1;
            i += 1;
        }

        if input[k] == -1 {
            k -= 1;
        }

        if input[i] != -1 {
            arrange.insert((i, input[i] as usize));
            i += 1;
        }
    }

    arrange
}

fn get_arrange_part02(input: Vec<Vec<isize>>) -> Vec<Vec<isize>> {
    let reverse = input.iter().rev();
    let mut arrange = input.clone();
    // dbg!(&input);

    for (_, chunk) in reverse.enumerate() {
        let Some((i, _)) = arrange.iter().enumerate().find(|(_, empty_space)| {
            empty_space.len() >= chunk.len() && empty_space.iter().all(|x| *x == -1)
        }) else {
            continue;
        };

        if chunk.iter().all(|x| *x == -1) {
            continue;
        }

        let (r, _) = arrange
            .iter()
            .enumerate()
            .find(|(_, v)| v.iter().all(|x| *x == chunk[0]))
            .unwrap();

        if i > r {
            continue;
        }

        let empty_sub = &arrange.remove(i);

        let space = empty_sub.len() - chunk.len();

        if space > 0 {
            let new_empty = vec![-1; space];
            arrange.insert(i, new_empty);
        }

        // check again after insert
        let (r, _) = arrange
            .iter()
            .enumerate()
            .find(|(_, v)| v.iter().all(|x| *x == chunk[0]))
            .unwrap();

        arrange.remove(r);
        arrange.insert(r, vec![-1; chunk.len()]);
        arrange.insert(i, chunk.to_vec());
        // arrange.remove(arrange.len() - k - 1);
        // arrange.insert(arrange.len() - k, vec![-1; chunk.len()]);
    }

    // dbg!(&arrange);
    arrange
}

fn get_checksum(input: HashSet<(usize, usize)>) -> usize {
    input.iter().map(|(a, b)| a * b).sum()
}

fn get_checksum_part2(input: Vec<Vec<isize>>) -> usize {
    input
        .concat()
        .iter()
        .enumerate()
        .filter_map(|(i, &v)| if v != -1 { Some(i as isize * v) } else { None })
        .map(|x| x as usize)
        .sum()
}

fn print_seq(input: &HashSet<(usize, usize)>, len: usize) {
    let mut acc = vec![46; len];

    input.iter().fold(&mut acc, |acc, &(i, v)| {
        acc[i] = v;
        acc
    });

    println!("{:?}", acc);
}

fn main() {
    let input = include_str!("input.txt");
    let result = process_part01(input);
    let result2 = process_part02(input);
    println!("The checksum is {}, {}", result, result2);
}

fn process_part01(i: &str) -> usize {
    let items = parse(i);
    let len = &items.len();
    let arrange = get_arrange(items);
    print_seq(&arrange, *len);
    get_checksum(arrange)
}

fn process_part02(i: &str) -> usize {
    let items = parse_chunks(i);
    let arrange = get_arrange_part02(items);
    println!("{:?}", arrange);
    get_checksum_part2(arrange)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("2333133121414131402", 1928)]
    fn should_return_checksum(#[case] input: &str, #[case] expected: usize) {
        let result = process_part01(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case("2333133121414131402", 2858)]
    fn should_return_checksum_part2(#[case] input: &str, #[case] expected: usize) {
        let result = process_part02(input);
        assert_eq!(result, expected);
    }
}
