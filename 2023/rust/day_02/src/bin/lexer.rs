use std::iter::Sum;
use std::str::FromStr;

pub type Err = &'static str;

pub fn read_lines(file: &str) -> Result<Vec<&str>, Err> {
    let lines: Vec<&str> = file.lines().map(|line| line.trim()).collect();
    match lines.is_empty() {
        true => Err("Input is empty".into()),
        false => Ok(lines),
    }
}

impl Sum<LineParser> for u32 {
    fn sum<I: Iterator<Item = LineParser>>(iter: I) -> Self {
        iter.fold(0, |mut total, item| {
            total += item.result.game_id;
            return total;
        })
    }
}

#[derive(Default, Clone)]
pub struct GameToken {
    pub game_id: u32,
    pub red_items: Vec<u8>,
    pub green_items: Vec<u8>,
    pub blue_items: Vec<u8>,
    pub is_valid: bool,
}

impl GameToken {
    fn new(input: &str) -> Self {
        input.parse::<GameToken>().unwrap()
    }
}

impl FromStr for GameToken {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (id, game) = s.split_once(':').ok_or("JUMMP IN THE FIREE")?;
        let id = id.chars().filter(|x| x.is_digit(10)).collect::<String>();

        let (red, green, blue) = game.split(';').fold(
            (Vec::new(), Vec::new(), Vec::new()),
            |(mut r, mut g, mut b), game_line| {
                let tokens = game_line.split(',').collect::<Vec<&str>>();

                for token in tokens.iter() {
                    if token.is_empty() {
                        continue;
                    }
                    let (count, color) = token
                        .strip_prefix(' ')
                        .and_then(|x| x.split_once(' '))
                        .expect("SOO COOMMMO ONN");

                    let count: u8 = count.parse().unwrap_or_default();

                    match color.trim() {
                        "red" => r.push(count),
                        "green" => g.push(count),
                        "blue" => b.push(count),
                        &_ => todo!(),
                    }
                }
                (r, g, b)
            },
        );

        Ok(GameToken {
            game_id: id.parse::<u32>().unwrap(),
            red_items: red,
            green_items: green,
            blue_items: blue,
            ..Default::default()
        })
    }
}

#[derive(Default, Clone)]
pub struct LineParser {
    pub input: String,
    pub result: GameToken,
}

impl LineParser {
    pub fn new(input: &str) -> Self {
        LineParser {
            input: input.to_string(),
            result: GameToken::new(input),
        }
    }

    pub fn validate(&self, r_rule: u8, g_rule: u8, b_rule: u8) -> Self {
        let r_valid = self.result.red_items.iter().any(|&x| x > r_rule);
        let g_valid = self.result.green_items.iter().any(|&x| x > g_rule);
        let b_valid = self.result.blue_items.iter().any(|&x| x > b_rule);

        let mut cloned_result = self.result.clone();
        cloned_result.is_valid = !r_valid && !g_valid && !b_valid;

        LineParser {
            input: self.input.clone(),
            result: cloned_result,
        }
    }

    pub fn get_max_item_power(&self) -> u32 {
        let red = *self.result.red_items.iter().max().unwrap() as u32;
        let green = *self.result.green_items.iter().max().unwrap() as u32;
        let blue = *self.result.blue_items.iter().max().unwrap() as u32;
        red * green * blue
    }
}
