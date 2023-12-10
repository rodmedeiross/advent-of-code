use itertools::{Itertools, Position};
use rayon::prelude::*;
use std::{collections::BTreeMap, ops::Deref, str::FromStr};

#[derive(Debug, Default, Copy, Clone)]
pub enum CardHandRanking {
    #[default]
    HighCard,
    OnePair,
    TwoPairs,
    ThreeCards,
    FullHouse,
    ForCards,
    FiveCards = 6,
}

#[derive(Debug, Default, Clone)]
pub struct Game {
    cards: Vec<(Vec<u8>, usize)>,
}

#[derive(Debug, Default)]
pub struct HandCards {
    cards: Vec<u8>,
    hand_ranking: CardHandRanking,
    bid: usize,
}

impl Game {
    pub fn new(input: &str) -> Self {
        input.parse::<Game>().unwrap()
    }

    pub fn define_hand(&self) -> Vec<HandCards> {
        use CardHandRanking::*;

        let game_parsed = self
            .cards
            .par_iter()
            .map(|(cards, bid)| {
                let map_card = cards
                    .iter()
                    .map(|card| (*card, 0_u8))
                    .collect::<BTreeMap<u8, u8>>();

                let map = cards.iter().fold(map_card, |mut map, card| {
                    map.entry(*card).and_modify(|value| *value += 1);
                    map
                });

                // if let Some((_, jokers)) = map.remove_entry(&1) {
                //     // Jokers in the game
                //     let max_key = map.iter().position_max().unwrap() as u8;
                //     dbg!(&max_key);
                //     map.entry(max_key).and_modify(|value| {
                //         dbg!(&value, &jokers);
                //         *value += jokers
                //     });

                //     // map.insert(1, jokers);
                // }
                // dbg!(&map);

                (map, *bid)
            })
            .collect::<Vec<(BTreeMap<u8, u8>, usize)>>();

        let hand_cards: Vec<HandCards> = game_parsed
            .iter()
            .zip(self.cards.iter())
            .map(|((game_tree, _), (game_vec, bid))| {
                // let mut game_cloned = game_tree.clone();

                // if game_vec.contains(&1) {
                //     let increment = game_vec.iter().filter(|item| *item == &1).count() as u8;
                //     let max_key = game_tree
                //         .iter()
                //         .max_by_key(|&(_, value)| value)
                //         .map(|(key, _)| key);

                //     if let Some(max_key) = max_key {
                //         if max_key != &1 {
                //             if let Some(value) = game_cloned.get_mut(max_key) {
                //                 *value += increment;
                //             };
                //             game_cloned.remove_entry(&1);
                //         };
                //     }
                // }

                let cards_number = if let Some(joker_count) = game_tree.get(&1) {
                    if *joker_count == 5 {
                        "5".to_string()
                    } else {
                        game_tree
                            .iter()
                            .filter_map(|(key, value)| (key != &1).then_some(value))
                            .sorted()
                            .with_position()
                            .map(|(pos, value)| match pos {
                                Position::Last | Position::Only => value + joker_count,
                                _ => *value,
                            })
                            .join("")
                    }
                } else {
                    game_tree.values().sorted().join("")
                };

                dbg!(&cards_number);

                // let cards_number = game_cloned.values().sorted().join("");

                let cards_game_ranking = match cards_number.deref() {
                    "5" => FiveCards,
                    "14" => ForCards,
                    "23" => FullHouse,
                    "113" => ThreeCards,
                    "122" => TwoPairs,
                    "1112" => OnePair,
                    "11111" => HighCard,
                    _ => panic!("LOOK AWAY `{}`", cards_number),
                };

                HandCards {
                    cards: game_vec.clone(),
                    hand_ranking: cards_game_ranking,
                    bid: *bid,
                }
            })
            .collect();

        hand_cards
    }
}

impl CardHandRanking {
    pub fn to_u8(&self) -> u8 {
        *self as u8
    }
}

impl From<u8> for CardHandRanking {
    fn from(value: u8) -> Self {
        match value {
            0 => CardHandRanking::HighCard,
            1 => CardHandRanking::OnePair,
            2 => CardHandRanking::TwoPairs,
            3 => CardHandRanking::ThreeCards,
            4 => CardHandRanking::FullHouse,
            5 => CardHandRanking::ForCards,
            _ if value >= 6 => CardHandRanking::FiveCards,
            _ => panic!("Invalid value for CardHandRanking"),
        }
    }
}

// fn hand_ranking_value(hand_ranking: &CardHandRanking) -> u8 {
//     match hand_ranking {
//         CardHandRanking::HighCard => 0,
//         CardHandRanking::OnePair => 1,
//         CardHandRanking::TwoPairs => 2,
//         CardHandRanking::ThreeCards => 3,
//         CardHandRanking::FullHouse => 4,
//         CardHandRanking::ForCards => 5,
//         CardHandRanking::FiveCards => 6,
//     }
// }

impl FromStr for Game {
    type Err = &'static str;
    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let cards = s
            .lines()
            .into_iter()
            .map(|line| {
                line.trim()
                    .split_once(' ')
                    .map(|(cards, ranking)| {
                        (
                            cards[0..5]
                                .chars()
                                .map(|ch| match ch {
                                    'A' => 14_u8,
                                    'K' => 13_u8,
                                    'Q' => 12_u8,
                                    'J' => 1_u8, // lexer Changed for part 2
                                    'T' => 10_u8,
                                    _ => ch.to_digit(10).unwrap() as u8,
                                })
                                .collect::<Vec<u8>>(),
                            // cards.to_string(),
                            ranking.parse::<usize>().unwrap(),
                        )
                    })
                    .expect("JUMMMMP IN THE FIREE")
            })
            .collect();

        Ok(Game { cards })
    }
}

pub fn ranking_gamers_result(mut gamers: Vec<HandCards>) -> usize {
    gamers.sort_by(|a, b| {
        a.cards
            .first()
            .cmp(&b.cards.first())
            .then_with(|| a.cards[1].cmp(&b.cards[1]))
            .then_with(|| a.cards[2].cmp(&b.cards[2]))
            .then_with(|| a.cards[3].cmp(&b.cards[3]))
            .then_with(|| a.cards[4].cmp(&b.cards[4]))
    });

    gamers.sort_by(|a, b| a.hand_ranking.to_u8().cmp(&b.hand_ranking.to_u8()));

    dbg!(&gamers);

    gamers
        .into_iter()
        .enumerate()
        .fold(0_usize, |mut result, (index, cards)| {
            let i = index + 1;
            result += i * cards.bid;
            result
        })
}
