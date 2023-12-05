use std::collections::BTreeMap;
use std::iter::Sum;
use std::str::FromStr;

pub type Err = &'static str;

pub trait Process {
    fn process_extra_scratchcards(&self) -> Vec<u32>;
}

pub fn read_lines(file: &str) -> Result<Vec<&str>, Err> {
    let lines: Vec<&str> = file.lines().map(|line| line.trim()).collect();
    match lines.is_empty() {
        true => Err("Input is empty".into()),
        false => Ok(lines),
    }
}

impl Process for Vec<Card> {
    fn process_extra_scratchcards(&self) -> Vec<u32> {
        let new_collection_card = (0..self.len())
            .map(|index| (index, 1))
            .collect::<BTreeMap<usize, u32>>();

        let result =
            self.iter()
                .enumerate()
                .fold(new_collection_card, |mut collection, (index, card)| {
                    for i in (index + 1)..(index + 1 + card.game_acc as usize) {
                        let next_add = *collection.get(&index).unwrap() as u32;
                        collection.entry(i).and_modify(|value| *value += next_add);
                    }
                    collection
                });

        result.into_iter().map(|x| x.1 as u32).collect::<Vec<u32>>()
    }
}

#[derive(Debug, Default, Clone)]
pub struct Card {
    id: u16,
    game_numbers: Vec<u16>,
    winner_numbers: Vec<u16>,
    game_result: u16,
    game_acc: u16,
}

impl Card {
    pub fn new(line_card: &str) -> Self {
        line_card.parse::<Card>().unwrap()
    }

    pub fn compute_result(&self) -> Self {
        let mut result: u16 = 0;

        for game_number in &self.game_numbers {
            if self.winner_numbers.contains(&game_number) {
                result += 1
            }
        }

        let mut card: Card = self.clone();

        card.game_result = match result {
            r if r > 0 => 2u16.pow((r - 1) as u32),
            _ => result as u16,
        };
        card.game_acc = result;

        card
    }
}

impl Sum<Card> for u16 {
    fn sum<I: Iterator<Item = Card>>(iter: I) -> Self {
        iter.fold(0, |mut total, item| {
            total += item.game_result;
            total
        })
    }
}

impl FromStr for Card {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let splited_game = s.split(&[':', '|'][..]).collect::<Vec<_>>();
        let id = splited_game
            .first()
            .expect("Runn to the Hills")
            .trim()
            .chars()
            .filter(|ch| ch.is_digit(10))
            .collect::<String>()
            .parse::<u16>()
            .unwrap();

        let game_numbers = splited_game[1]
            .split_whitespace()
            .filter_map(|n_str| n_str.parse::<u16>().ok())
            .collect::<Vec<u16>>();

        let winner_numbers = splited_game[2]
            .split_whitespace()
            .filter_map(|n_str| n_str.parse::<u16>().ok())
            .collect::<Vec<u16>>();

        Ok(Card {
            id,
            game_numbers,
            winner_numbers,
            ..Default::default()
        })
    }
}
