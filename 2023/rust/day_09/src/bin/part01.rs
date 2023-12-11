use rayon::prelude::*;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result);
}

fn process_data(data: &str) -> i64 {
    let line_vec: Vec<Vec<i64>> = data
        .lines()
        .into_iter()
        .map(|line| {
            line.split_whitespace()
                .map(|digit| digit.parse::<i64>().unwrap())
                .collect::<Vec<i64>>()
        })
        .collect();

    let line = line_vec
        .par_iter()
        .map(|item: &Vec<i64>| {
            // let len_last = item.len();
            // let increment = len_last % 2;
            // let lines = (len_last + increment) / 2;
            let mut matrix = vec![item.clone()];

            for line in 0.. {
                matrix.push(
                    matrix[line]
                        .iter()
                        .enumerate()
                        .filter_map(|(i, number)| {
                            matrix[line].get(i + 1).map(|next_item| next_item - number)
                        })
                        .collect::<Vec<i64>>(),
                );
                if matrix.last().unwrap().iter().all(|&x| x == 0) {
                    break;
                }
            }

            // dbg!(&matrix);

            let vec_res: Vec<i64> = matrix.iter().map(|item| *item.last().unwrap()).collect();
            //a ij = a (i−1)j −a (i−1)(j−1)

            // dbg!(&vec_res);

            vec_res.iter().sum()
        })
        .collect::<Vec<i64>>();

    line.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "0 3 6 9 12 15
        1 3 6 10 15 21
        10 13 16 21 30 45",
        "114"
    )]
    fn should_return_sum_of_path_walk(#[case] input: &str, #[case] expected: &str) {
        let result = process_data(input).to_string();
        assert_eq!(result, expected);
    }
}
