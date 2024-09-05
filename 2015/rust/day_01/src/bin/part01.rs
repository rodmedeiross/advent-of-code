use std::str::FromStr;

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result)
}

struct FloorMapping {
    up_floor: u16,
    down_floor: u16,
}

impl FloorMapping {
    fn new(input: &str) -> Self {
        FloorMapping {
            up_floor: read_char(input, '('),
            down_floor: read_char(input, ')'),
        }
    }

    fn compute_position(&self) -> u16 {
        println!("{}", self.up_floor);
        println!("{}", self.down_floor);

        self.up_floor - self.down_floor
    }
}

impl FromStr for FloorMapping {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(FloorMapping::new(s))
    }
}

fn read_char(s: &str, target: char) -> u16 {
    s.chars().filter(|&c| c == target).count() as u16
}

fn process_data(data: &str) -> u16 {
    let floor_map = FloorMapping::from_str(data).expect("Invalid data");
    floor_map.compute_position()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_right_floor() {
        let input = "))(((((";
        let result = process_data(input);

        assert_eq!(result, 3);
    }
}
