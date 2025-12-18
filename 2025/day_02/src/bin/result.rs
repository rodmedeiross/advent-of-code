use std::cmp::{max, min};
use std::collections::HashSet;
use std::str::FromStr;

// Some math here
// Invalid number = N = x*(10^k + 1) | 12 31 23 = 123*(10^3+1)
// N = x*m
// m = 10^k + 1 = 10^1 + 1 = 11
// k = Major number divided for 2 | 99-123 = 123/2 = 2
// 10^k-1 <= x <= (10^k) -1 | Getting edges for x
// first number / m <= x <= last number / m  | 11-22 = 11/11 <= x <= 22/11
// numbers will be from min(10^k-1, first number / m)
// to max((10^k) -1, first number / m)

type ParseErr = &'static str;

#[derive(Debug)]
struct Table {
    ids: Vec<Ids>,
}

#[derive(Debug)]
struct Ids {
    min: usize,
    max: usize,
    min_s: String,
    max_s: String,
}

impl Ids {
    // how to solve something like 9-2321
    fn km_iter(&self) -> impl Iterator<Item = (u32, usize)> + '_ {
        let k2min = self.min_s.len();
        let k2max = self.max_s.len();

        (k2min..=k2max)
            .filter(|k2| k2 % 2 == 0)
            .map(|k2| (k2 / 2) as u32)
            .map(|k| (k, 10_usize.pow(k) + 1))
    }

    fn km2_iter(&self) -> impl Iterator<Item = (usize, usize, u32, usize)> + '_ {
        let dmin = self.min_s.len();
        let dmax = self.max_s.len();

        (dmin..=dmax).flat_map(|d| {
            (2..=d).filter(move |t| d % t == 0).map(move |t| {
                let k = d / t;
                let mut m: usize = 0;
                for i in 0..t {
                    m += 10_usize.pow((i * k) as u32)
                }
                (d, k, t as u32, m)
            })
        })
    }

    // applying 10^k-1 <= x <= (10^k) -1 -> here I get through min_e and max_e the values of wich
    // number and assure that it is inside of the edges
    // applying first number / m <= x <= last number / m
    // numbers will be from min(10^k-1, first number / m)
    // to max((10^k) -1, first number / m)
    // here should be floor and ceil
    fn get_edges_limit(
        &self,
    ) -> impl Iterator<Item = ((usize, usize), (usize, usize), usize)> + '_ {
        self.km_iter().map(|(k, m)| {
            // MIN-MAX 2-1232
            // 2 - 9 -> 10 - 99
            let min_e = max(self.min, 10usize.pow(2 * k - 1));
            let max_e = min(self.max, 10usize.pow(2 * k) - 1);

            let min_seq_a = max(
                10_usize.pow(k - 1),
                ((min_e as f32 / m as f32).ceil()) as usize,
            );
            let max_seq_b = min(
                10_usize.pow(k) - 1,
                ((max_e as f32 / m as f32).floor()) as usize,
            );

            let res = ((min_e, max_e), (min_seq_a, max_seq_b), m);
            println!("{:?}", &res);
            res
        })
    }

    fn get_edges_limit_2(
        &self,
    ) -> impl Iterator<Item = ((usize, usize), (usize, usize), usize)> + '_ {
        self.km2_iter().map(|(_d, k, t, m)| {
            let min_e = max(self.min, 10usize.pow(t * k as u32 - 1));
            let max_e = min(self.max, 10usize.pow(t * k as u32) - 1);

            let min_seq_a = max(
                10_usize.pow(k as u32 - 1),
                ((min_e as f32 / m as f32).ceil()) as usize,
            );

            let max_seq_b = min(
                10_usize.pow(k as u32) - 1,
                ((max_e as f32 / m as f32).floor()) as usize,
            );

            let res = ((min_e, max_e), (min_seq_a, max_seq_b), m);
            println!("{:?}", &res);
            res
        })
    }

    fn get_invalid_numbers(&self) -> Vec<usize> {
        self.get_edges_limit()
            .filter(|(_, (a, b), _)| a <= b)
            .flat_map(|(_, (a, b), m)| (a..=b).map(move |x| x * m))
            .collect()
    }

    fn get_invalid_numbers_2(&self) -> HashSet<usize> {
        self.get_edges_limit_2()
            .filter(|(_, (a, b), _)| a <= b)
            .flat_map(|(_, (a, b), m)| (a..=b).filter_map(move |x| x.checked_mul(m)))
            .collect()
    }
}

impl Table {
    fn get_sum_invalid_numbers(&self) -> usize {
        self.ids
            .iter()
            .flat_map(|ids| ids.get_invalid_numbers())
            .sum::<usize>()
    }

    fn get_sum_invalid_numbers_part2(&self) -> usize {
        self.ids
            .iter()
            .flat_map(|ids| ids.get_invalid_numbers_2())
            .sum::<usize>()
    }
}

impl FromStr for Ids {
    type Err = ParseErr;
    fn from_str(s: &str) -> Result<Ids, Self::Err> {
        let (min_s, max_s) = s.split_once("-").ok_or("DAMMIT")?;
        let min: usize = min_s.parse().map_err(|_| "DAMMIT")?;
        let max: usize = max_s.parse().map_err(|_| "DAMMIT")?;

        Ok(Ids {
            min,
            max,
            min_s: min_s.to_string(),
            max_s: max_s.to_string(),
        })
    }
}

impl FromStr for Table {
    type Err = ParseErr;
    fn from_str(s: &str) -> Result<Table, Self::Err> {
        let ids: Vec<Ids> = s
            .trim()
            .split(",")
            .map(Ids::from_str)
            .collect::<Result<Vec<_>, _>>()?;

        println!("{:?}", &ids);
        Ok(Table { ids })
    }
}

fn main() {
    let input = include_str!("input.txt");
    println!("{:?}", input);
    let table = Table::from_str(input).unwrap();

    let res = table.get_sum_invalid_numbers();
    println!("Result = {}", res);

    let res2 = table.get_sum_invalid_numbers_part2();
    println!("Result Part 2 = {}", res2);
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", 1227775554)]
    fn should_calculate_invalid_dis(#[case] input: &str, #[case] expected: usize) {
        let table = Table::from_str(input).unwrap();
        let res = table.get_sum_invalid_numbers();

        assert_eq!(res, expected)
    }

    #[rstest]
    #[case("11-22,95-115,998-1012,1188511880-1188511890,222220-222224,1698522-1698528,446443-446449,38593856-38593862,565653-565659,824824821-824824827,2121212118-2121212124", 4174379265)]
    fn should_calculate_invalid_dis_part2(#[case] input: &str, #[case] expected: usize) {
        let table = Table::from_str(input).unwrap();
        let res = table.get_sum_invalid_numbers_part2();
        assert_eq!(res, expected)
    }
}
