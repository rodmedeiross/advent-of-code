use std::collections::HashMap;

fn main() {
    let route = include_str!("input.txt");
    let result = process_data(route);
    let result2 = process_data_pt2(route);
    println!("Number of more than 1 visited: {}", result);
    println!("Number of more than 1 visited: {}", result2);
}

struct Table {
    visited: Vec<(i16, i16)>,
}

fn verify_char(increment: (i16, i16), ch: char) -> (i16, i16) {
    match ch {
        '^' => (increment.0, increment.1 + 1),
        '>' => (increment.0 + 1, increment.1),
        'v' => (increment.0, increment.1 - 1),
        '<' => (increment.0 - 1, increment.1),
        _ => increment,
    }
}

impl Table {
    fn traversal(rote: &str) -> Self {
        let mut table = Table {
            visited: Vec::new(),
        };

        let mut santa = (0, 0);
        table.visited.push(santa);

        for ch in rote.chars() {
            santa = verify_char(santa, ch);
            table.visited.push(santa)
        }
        table
    }

    fn traversal_with_robot(rote: &str) -> Self {
        let mut table = Table {
            visited: Vec::new(),
        };

        let mut santa = (0, 0);
        let mut robot = (0, 0);

        let chars = rote.chars().collect::<Vec<_>>();

        for path_to_follow in chars.chunks(2) {
            if let Some(ch1) = path_to_follow.get(0) {
                santa = verify_char(santa, *ch1);
                table.visited.push(santa);
            }

            if let Some(ch2) = path_to_follow.get(1) {
                robot = verify_char(robot, *ch2);
                table.visited.push(robot);
            }
        }

        table
    }

    fn get_map(&self) -> HashMap<(i16, i16), u16> {
        let mut mapped_path = HashMap::new();

        for location in self.visited.iter() {
            mapped_path
                .entry(*location)
                .and_modify(|rate| *rate += 1)
                .or_insert(1);
        }

        mapped_path
    }

    fn count(&self) -> u32 {
        let mut count = 0;

        for (_, value) in self.get_map() {
            if value >= 1 {
                count += 1;
            }
        }

        count
    }
}

fn process_data(s: &str) -> u32 {
    let table_path = Table::traversal(s);
    table_path.count()
}

fn process_data_pt2(s: &str) -> u32 {
    let table_path = Table::traversal_with_robot(s);
    table_path.count()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_path_traversal() {
        let input = "^v^v^v^v^v";

        let result = process_data(input);

        assert_eq!(result, 2);
    }

    #[test]
    fn validate_path_traversal_with_robot() {
        let input = "^v^v^v^v^v";

        let result = process_data_pt2(input);

        assert_eq!(result, 2);
    }
}
