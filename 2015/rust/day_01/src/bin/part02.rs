use std::{error::Error, str::FromStr};

fn main() {
    let data = include_str!("input_part01.txt");
    let result = process_data(data);
    println!("Result: {}", result)
}

#[derive(Default, Debug)]
struct FloorMapping {
    current_position: i16,
    basement_indexes: Vec<u16>,
}

impl FloorMapping {
    fn new(current_position: i16, basement_indexes: Vec<u16>) -> Self {
        FloorMapping {
            current_position,
            basement_indexes,
        }
    }

    fn get_result(&self) -> (i16, u16) {
        (self.current_position, self.basement_indexes[0])
    }
}

impl FromStr for FloorMapping {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(walk(s).expect(""))
    }
}

fn walk(s: &str) -> Result<FloorMapping, Box<dyn Error>> {
    let mut basement_indexes: Vec<u16> = Vec::new();
    let mut current_place: i16 = 0;
    for (i, c) in s.chars().enumerate() {
        match c {
            ')' => {
                current_place -= 1;
                if current_place < 0 as i16 {
                    basement_indexes.push((i + 1) as u16);
                    break;
                }
            }
            '(' => {
                current_place += 1;
            }
            _ => {}
        }
    }

    Ok(FloorMapping::new(current_place, basement_indexes))
}

fn process_data(data: &str) -> u16 {
    let floor_map = FloorMapping::from_str(data).expect("Invalid data");
    floor_map.get_result().1
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn should_return_right_floor() {
        let input = "()())";
        let result = process_data(input);

        assert_eq!(result, 5);
    }
}
