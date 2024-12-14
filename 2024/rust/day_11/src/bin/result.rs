use cached::proc_macro::cached;

// struct Memoized<'a, F>
// where
//     F: Fn((&'a str, isize)) -> usize,
// {
//     cache: HashMap<(&'a str, isize), usize>,
//     func: F,
// }
//
// impl<'a, F> Memoized<'a, F>
// where
//     F: Fn((&'a str, isize)) -> usize,
// {
//     fn new(func: F) -> Self {
//         Self {
//             cache: HashMap::new(),
//             func,
//         }
//     }
//
//     fn process(&mut self, input: (&'a str, isize)) -> usize {
//         if let Some(&result) = self.cache.get(&input) {
//             return result;
//         }
//
//         let result = (self.func)(input);
//         self.cache.insert(input, result);
//         result
//     }
// }

fn main() {
    let input = include_str!("input.txt");
    // let result = process_part01(input);
    let result2 = process_part02(input);
    println!("Blink 182 {}", result2);
}

fn process_part01(i: &str) -> usize {
    let token = i
        .replace("\n", "")
        .split(" ")
        .map(|x| x.to_string())
        .collect::<Vec<String>>();
    let result = process_rock(token, 25);

    result.len()
}

fn process_part02(i: &str) -> usize {
    let token = i
        .replace("\n", "")
        .split(" ")
        .map(|x| x.to_string())
        .collect::<Vec<String>>();
    let result = process_rock_part2(token);

    result
}

fn process_rock(mut i: Vec<String>, dep: isize) -> Vec<String> {
    if dep == 0 {
        return i;
    }
    let mut inner: Vec<String> = Vec::new();

    for tk in i.iter_mut() {
        if tk.len() % 2 == 0 {
            let mid = tk.len() / 2;

            let (f, s) = tk.split_at_mut(mid);

            inner.push(f.to_string());

            if s.contains("0") {
                let num = s.parse::<isize>().unwrap();
                inner.push(num.to_string());
            } else {
                inner.push(s.to_string());
            }
        } else if tk == "0" {
            inner.push("1".to_string());
        } else {
            let num = tk.parse::<usize>().unwrap();
            let res = num * 2024;
            let res_str = res.to_string();

            inner.push(res_str);
        }
    }
    dbg!(&inner);

    return process_rock(inner, dep - 1);
}

fn process_rock_part2(input: Vec<String>) -> usize {
    let mut results = Vec::new();

    #[cached]
    fn rock_rock(step: (String, isize)) -> usize {
        let (string_part, depth) = step;

        if depth == 0 {
            return 1;
        }

        if string_part.len() % 2 == 0 {
            let mid = string_part.len() / 2;
            let (f, s) = string_part.split_at(mid);

            if s.contains('0') {
                let num = s.parse::<isize>().unwrap_or(0);

                return rock_rock((f.to_string(), depth - 1))
                    + rock_rock((num.to_string(), depth - 1));
            } else {
                return rock_rock((f.to_string(), depth - 1))
                    + rock_rock((s.to_string(), depth - 1));
            }
        } else if string_part == "0" {
            return rock_rock(("1".to_string(), depth - 1));
        } else {
            let num = string_part.parse::<usize>().unwrap_or(0);
            let result = num * 2024;
            return rock_rock((result.to_string(), depth - 1));
        };
    }

    for item in input {
        let result = rock_rock((item, 75));
        results.push(result);
    }

    results.iter().sum()
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case("125 17", 55312)]
    fn process_stones(#[case] input: &str, #[case] expected: usize) {
        let result = process_part01(input);
        assert_eq!(result, expected);
    }

    #[rstest]
    #[case("125 17", 55312)]
    fn process_stones_pt2(#[case] input: &str, #[case] expected: usize) {
        let result = process_part02(input);
        assert_eq!(result, expected);
    }
}
