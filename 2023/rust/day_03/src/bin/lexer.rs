use std::collections::HashSet;
use std::{error::Error, str::FromStr};

#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub enum Value {
    #[default]
    Empty,
    Number(u32),
    Symbol(char),
}

#[derive(Debug, Clone, Default)]
pub struct Gear {
    pub axis: (i32, i32),
    pub character: Value,
}

#[derive(Debug, Default, Clone)]
pub struct Machine {
    pub gears: Vec<Gear>,
    pub numbers: Vec<Vec<Gear>>,
    pub symbols: Vec<Gear>,
}

static TABLE_AXIS_VERIFY: [(i32, i32); 8] = [
    (-1, -1),
    (0, -1),
    (1, -1),
    (1, 0),
    (1, 1),
    (0, 1),
    (-1, 1),
    (-1, 0),
];

impl Machine {
    pub fn convert_in_unified_numbers(&self) -> Self {
        let mut numbers: Vec<Vec<Gear>> = vec![];

        for gear in self.gears.iter() {
            if let Value::Number(_) = gear.character {
                match numbers.iter().last() {
                    Some(v) => {
                        let last_number = v.iter().last();
                        match last_number {
                            Some(n) => {
                                if n.axis.0 + 1 == gear.axis.0 {
                                    let last = numbers.iter_mut().last().expect("SOO COMOOO ONN");
                                    last.push(Gear {
                                        axis: gear.axis.clone(),
                                        character: gear.character.clone(),
                                    })
                                } else {
                                    numbers.push(vec![Gear {
                                        axis: gear.axis.clone(),
                                        character: gear.character.clone(),
                                    }])
                                }
                            }
                            None => unimplemented!("U can ask my Engineer"),
                        }
                    }
                    None => numbers.push(vec![Gear {
                        axis: gear.axis.clone(),
                        character: gear.character.clone(),
                        ..Default::default()
                    }]),
                }
            }
        }

        let mut copied_machine = self.clone();
        copied_machine.numbers = numbers;

        copied_machine
    }

    pub fn verify_number(&self) -> Result<u32, Box<dyn Error>> {
        let mut total_gears = 0;
        for number in &self.numbers {
            let num_to_check: Vec<(i32, i32)> =
                number.iter().map(|x| (x.axis.0, x.axis.1)).collect();

            let pos_to_check = number
                .iter()
                .flat_map(|x| {
                    TABLE_AXIS_VERIFY
                        .iter()
                        .map(|outer| (outer.0 + x.axis.0, outer.1 + x.axis.1))
                })
                .filter(|num| !num_to_check.contains(num))
                .collect::<Vec<(i32, i32)>>();

            let is_valid_gear = pos_to_check.iter().any(|pos| {
                let value: Vec<Gear> = self
                    .gears
                    .iter()
                    .filter(|x| x.axis == *pos)
                    .cloned()
                    .collect();

                if let Value::Symbol(_) = value
                    .first()
                    .map(|gear| gear.character.clone())
                    .unwrap_or_default()
                {
                    true
                } else {
                    false
                }
            });

            let formated_number = number
                .iter()
                .fold(String::new(), |formated, to_format| {
                    match to_format.character {
                        Value::Number(n) => formated + &n.to_string(),
                        _ => formated,
                    }
                })
                .parse::<u32>()
                .unwrap();

            if is_valid_gear {
                total_gears += formated_number
            }
        }

        Ok(total_gears)
    }

    pub fn verify_gears(&mut self) -> Result<usize, Box<dyn Error>> {
        let mut total_gears = 0;
        for gear in self
            .gears
            .iter()
            .filter(|x| matches!(x.character, Value::Symbol('*')))
        {
            let pos_to_check = TABLE_AXIS_VERIFY
                .iter()
                .map(|outer| (outer.0 + gear.axis.0, outer.1 + gear.axis.1))
                .collect::<Vec<(i32, i32)>>();

            let mut number_index = vec![];

            for pos in pos_to_check {
                for (index, numbers) in self.numbers.iter().enumerate() {
                    if numbers.iter().find(|x| x.axis == pos).is_some() {
                        number_index.push(index)
                    }
                }
            }

            let is_gear = number_index
                .clone()
                .into_iter()
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
                .len()
                == 2;

            let formated_number = number_index
                .iter()
                .collect::<HashSet<_>>()
                .into_iter()
                .collect::<Vec<_>>()
                .into_iter()
                .map(|i| {
                    self.numbers[*i]
                        .iter()
                        .fold(String::new(), |formated, to_format| {
                            match to_format.character {
                                Value::Number(n) => formated + &n.to_string(),
                                _ => formated,
                            }
                        })
                        .parse::<usize>()
                        .unwrap()
                })
                .product::<usize>();

            if is_gear {
                total_gears += formated_number;
            }
        }
        Ok(total_gears)
    }
}

impl FromStr for Machine {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let mut gears = Vec::new();

        for (y_axis, line) in s.lines().enumerate() {
            for (x_axis, character) in line.trim().chars().enumerate() {
                match character {
                    character if character == '.' => gears.push(Gear {
                        axis: (x_axis as i32, y_axis as i32),
                        character: Value::Empty,
                    }),
                    character if character.is_ascii_digit() => {
                        let digit_value = character.to_digit(10).unwrap();
                        gears.push(Gear {
                            axis: (x_axis as i32, y_axis as i32),
                            character: Value::Number(digit_value),
                        });
                    }
                    character => gears.push(Gear {
                        axis: (x_axis as i32, y_axis as i32),
                        character: Value::Symbol(character),
                    }),
                }
            }
        }
        Ok(Machine {
            gears,
            ..Default::default()
        })
    }
}
