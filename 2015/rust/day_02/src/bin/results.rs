use std::{cmp, str::FromStr};

fn main() {
    let data = include_str!("input.txt");
    let result = process_data(data);
    println!("Squares: {}, Ribbon {}", result.0, result.1);
}

#[derive(Debug, PartialEq, PartialOrd)]
struct PresentBox {
    length: u16,
    width: u16,
    height: u16,
}

struct PresentList {
    items: Vec<PresentBox>,
}

impl PresentBox {
    fn new(length: u16, width: u16, height: u16) -> Self {
        PresentBox {
            length,
            width,
            height,
        }
    }

    fn calculate_square(&self) -> u32 {
        let l = self.length;
        let w = self.width;
        let h = self.height;

        let c1 = l * w;
        let c2 = w * h;
        let c3 = h * l;

        let min = cmp::min(c1, cmp::min(c2, c3)) as u32;

        (c1 + c2 + c3) as u32 * 2 + min
    }

    fn calculate_ribbon(&self) -> u32 {
        let perimeter = smallest_perimeter(self.length, self.width, self.height);
        let volume = self.length * self.width * self.height;

        (perimeter + volume) as u32
    }
}

impl PresentList {
    fn new(input: &str) -> Self {
        let items = input
            .lines()
            .into_iter()
            .map(|line| {
                let data: Vec<u16> = line
                    .trim()
                    .split("x")
                    .map(|c| c.parse::<u16>())
                    .collect::<Result<Vec<u16>, _>>()
                    .unwrap();

                PresentBox::new(data[0], data[1], data[2])
            })
            .collect::<Vec<PresentBox>>();

        PresentList { items }
    }

    fn sum_measures(self) -> (u32, u32) {
        self.items
            .iter()
            .map(|item| {
                (
                    item.calculate_square() as u32,
                    item.calculate_ribbon() as u32,
                )
            })
            .fold((0, 0), |acc, (square, ribbon)| {
                (acc.0 + square, acc.1 + ribbon)
            })
    }
}

fn smallest_perimeter(length: u16, width: u16, height: u16) -> u16 {
    let p1 = 2 * (length + width);
    let p2 = 2 * (length + height);
    let p3 = 2 * (width + height);
    std::cmp::min(p1, std::cmp::min(p2, p3))
}

impl FromStr for PresentList {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        Ok(PresentList::new(s))
    }
}

fn process_data(s: &str) -> (u32, u32) {
    let data = s.parse::<PresentList>().unwrap();
    data.sum_measures()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validate_wrapping_paper_square_feet() {
        let input = "2x3x4
        1x1x10";

        let result = process_data(input);

        assert_eq!(result.0, 101);
    }

    #[test]
    fn validate_ribbon_feet() {
        let input = "2x3x4
        1x1x10";

        let result = process_data(input);

        assert_eq!(result.1, 48);
    }
}
