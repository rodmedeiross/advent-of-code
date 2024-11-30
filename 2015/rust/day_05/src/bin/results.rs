use fancy_regex::Regex;
use std::str::FromStr;

const NICE_STRING_REGEX_PART_1: &str =
    r"^(?=(?:[^aeiou]*[aeiou]){3})(?!.*(?:ab|cd|pq|xy))(?=.*([\w])\1).*$";

const NICE_STRING_REGEX_PART_2: &str = r"^(?=(?:.*([\w]{2}).*\1.*))(?=(?:.*(\w)\w\2.*)).*$";

struct StringsGroup {
    nice: Vec<String>,
    bad: Vec<String>,
}

impl FromStr for StringsGroup {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let re = Regex::new(NICE_STRING_REGEX_PART_2).unwrap();
        let (nice, bad) =
            s.lines()
                .into_iter()
                .fold((Vec::new(), Vec::new()), |(mut nice, mut bad), item| {
                    if re.is_match(item).unwrap() {
                        nice.push(item.to_string())
                    } else {
                        bad.push(item.to_string())
                    }

                    (nice, bad)
                });

        Ok(StringsGroup::new(nice, bad))
    }
}

impl StringsGroup {
    fn new(nice: Vec<String>, bad: Vec<String>) -> Self {
        StringsGroup { nice, bad }
    }

    fn count_nice(&self) -> usize {
        self.nice.len()
    }

    fn count_bad(&self) -> usize {
        self.bad.len()
    }
}

fn process_strings(input: &str) -> (usize, usize) {
    let strings = input.parse::<StringsGroup>().unwrap();
    (strings.count_nice(), strings.count_bad())
}

fn main() {
    let input = include_str!("input.txt");
    let (nice, bad) = process_strings(input);

    println!("Number of nice strings: {}, and bad: {}", nice, bad);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validade_nice_strings() {
        let input = "ugknbfddgicrmopn
        aaa
        jchzalrnumimnmhp
        haegwjzuvuyypxyu
        dvszwmarrgswjxmb";

        let (nice, bad) = process_strings(input);

        assert_eq!(nice, 2);
        assert_eq!(bad, 3);
    }
}
