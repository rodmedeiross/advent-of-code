use std::collections::{BTreeMap, HashMap};
use std::iter;
use std::ops::Index;
use std::str::FromStr;

#[derive(Debug, Default, Clone)]
pub struct LensLibrary {
    numbers: Vec<(Vec<u8>, String)>,
}

#[derive(Debug, Clone)]
pub enum Action {
    Add,
    Remove,
}

#[derive(Debug, Clone)]
pub struct Lens {
    box_number: usize,
    action: Action,
    tag: String,
    number: u8,
}

impl LensLibrary {
    pub fn calculate_hash(&self) -> usize {
        let numbers = self
            .numbers
            .iter()
            .map(|(numbers, _)| generate_hash(numbers.clone()))
            .collect::<Vec<usize>>();
        numbers.iter().sum()
    }

    pub fn organize_lens(&self) -> usize {
        let lens = self
            .clone()
            .numbers
            .into_iter()
            .filter_map(|(_, tag)| match tag {
                tag if tag.contains("=") => {
                    let (left, right) = tag.split_once("=").unwrap();
                    let numbers = left
                        .chars()
                        .filter_map(|ch| match ch {
                            ch if ch == '\n' => None,
                            _ => Some(ch as u8),
                        })
                        .collect::<Vec<u8>>();
                    Some(Lens {
                        box_number: generate_hash(numbers),
                        action: Action::Add,
                        // tag: format!("{} {}", left.to_string(), right.to_string()),
                        tag: left.to_string(),
                        number: right.parse::<u8>().unwrap(),
                    })
                }
                tag if tag.contains("-") => {
                    let (left, _) = tag.split_once("-").unwrap();

                    let numbers = left
                        .chars()
                        .filter_map(|ch| match ch {
                            ch if ch == '\n' => None,
                            _ => Some(ch as u8),
                        })
                        .collect::<Vec<u8>>();
                    Some(Lens {
                        box_number: generate_hash(numbers),
                        action: Action::Remove,
                        tag: left.to_string(),
                        number: 0_u8,
                    })
                }
                _ => None,
            })
            .collect::<Vec<_>>();

        let mut lens_map: BTreeMap<_, _> = lens
            .iter()
            .map(|item| (item.box_number, Vec::<(String, u8)>::new()))
            .collect();

        lens.iter().for_each(|lens| match lens.action {
            Action::Add => {
                if let Some(lens_vec) = lens_map.get_mut(&lens.box_number) {
                    if lens_vec.is_empty() {
                        lens_vec.push((lens.tag.clone(), lens.number));
                    } else if let Some(to_swap) =
                        lens_vec.iter().position(|(tag, _)| *tag == lens.tag)
                    {
                        lens_vec[to_swap] = (lens.tag.clone(), lens.number);
                    } else {
                        lens_vec.push((lens.tag.clone(), lens.number));
                    }
                }
            }
            Action::Remove => {
                if let Some(lens_vec) = lens_map.get_mut(&lens.box_number) {
                    if let Some(to_remove) = lens_vec.iter().position(|(tag, _)| *tag == lens.tag) {
                        lens_vec.remove(to_remove);
                    }
                }
            }
        });

        let results: usize = lens_map
            .iter()
            .map(|(key, value)| {
                value
                    .iter()
                    .enumerate()
                    .map(|(inner_index, (_, number_lens))| {
                        (key + 1) * (inner_index + 1) * (*number_lens as usize)
                    })
                    .sum::<usize>()
            })
            .sum();

        results
    }
}

fn generate_hash(numbers: Vec<u8>) -> usize {
    let hash = numbers.iter().fold(0_usize, |mut total, item| {
        total = ((total + *item as usize) * 17) % 256;
        total as usize
    });

    hash
}

impl FromStr for LensLibrary {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let numbers = s
            .split(",")
            .map(|command| {
                let numbers = command
                    .chars()
                    .filter_map(|ch| match ch {
                        ch if ch == '\n' => None,
                        _ => Some(ch as u8),
                    })
                    .collect::<Vec<u8>>();
                let labels = command;

                (numbers, labels.to_string())
            })
            .collect::<Vec<_>>();
        Ok(LensLibrary { numbers })
    }
}
