use std::str::FromStr;

#[derive(Debug)]
struct Coord {
    directions: Vec<Direction>,
}

#[derive(Debug)]
enum Direction {
    Left(u16),
    Right(u16),
}

#[derive(Debug, Default)]
struct Dial {
    value: u16,
    password: u32,
    rounds: u32,
}

impl Dial {
    const START: u16 = 50;

    fn new() -> Self {
        Self {
            value: Self::START,
            password: u32::default(),
            rounds: u32::default(),
        }
    }

    fn left(&mut self, n: u16) {
        let rotation = self.value as i32 - n as i32;
        let value = rotation.rem_euclid(100);

        let k0 = if self.value == 0 { 100 } else { self.value };
        self.rounds += if n >= k0 {
            (1 + (n - k0) / 100) as u32
        } else {
            0
        };

        self.value = value as u16;
    }

    fn right(&mut self, n: u16) {
        let rotation = self.value as i32 + n as i32;
        let value = rotation.rem_euclid(100);

        let k0 = if self.value == 0 {
            100
        } else {
            100 - self.value
        };

        self.rounds += if n >= k0 {
            (1 + (n - k0) / 100) as u32
        } else {
            0
        };
        self.value = value as u16;
    }

    fn compute_rounds(&mut self) {
        self.password += self.rounds as u32
    }
}

impl Coord {
    fn find_password(&self) -> u32 {
        let mut dial = Dial::new();

        self.directions.iter().for_each(|dir| match dir {
            Direction::Right(n) => {
                dial.right(*n);
                println!("The dials is rotated R{} to point at {}", n, dial.value);
                println!("Numbers of rounds through 0 -> {}", dial.rounds)
            }
            Direction::Left(n) => {
                dial.left(*n);
                println!("The dials is rotated L{} to point at {}", n, dial.value);
                println!("Numbers of rounds through 0 -> {}", dial.rounds)
            }
        });
        dial.compute_rounds();
        dial.password
    }
}

impl FromStr for Coord {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Coord, Self::Err> {
        let directions: Vec<Direction> = s
            .lines()
            .filter(|line| !line.trim().is_empty())
            .map(|line| {
                let mut chars = line.chars();
                let directions_letter = chars.next().ok_or("Empty line")?;
                let directions_numbers_str = chars.collect::<String>();
                println!("Numbers parsed: {}", &directions_numbers_str);
                let directions_numbers: u16 = directions_numbers_str
                    .parse()
                    .map_err(|_| "Faild to convert to u16")?;

                let dir = match directions_letter {
                    'R' => Direction::Right(directions_numbers),
                    'L' => Direction::Left(directions_numbers),
                    _ => return Err("Invalid Direction Char"),
                };

                Ok(dir)
            })
            .collect::<Result<Vec<Direction>, Self::Err>>()?;

        Ok(Coord { directions })
    }
}

fn main() {
    let input = include_str!("input.txt");
    let tokens = input.parse::<Coord>().unwrap();
    println!("Tokens = {:?}", tokens);
    let password = tokens.find_password();
    println!("The password is: {}", password)
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::rstest;

    #[rstest]
    #[case(
        "L68
L30
R48
L5
R60
L55
L1
L99
R14
L82",
        6
    )]
    fn should_give_the_password(#[case] input: &str, #[case] expected: u32) {
        let tokens = input.parse::<Coord>().unwrap();
        println!("Direction Tokens: {:?}", tokens);
        let password = tokens.find_password();
        assert_eq!(password, expected)
    }

    #[rstest]
    #[case("R1000", 10)]
    fn should_give_the_password_2(#[case] input: &str, #[case] expected: u32) {
        let tokens = input.parse::<Coord>().unwrap();
        println!("Direction Tokens: {:?}", tokens);
        let password = tokens.find_password();
        assert_eq!(password, expected)
    }
}
