use std::{collections::BTreeMap, str::FromStr};

#[derive(Debug, Clone, Copy)]
pub enum Direction {
    Left,
    Right,
}

#[derive(Debug, Default, Clone)]
pub struct WasteLand {
    instructions: Vec<Direction>,
    map_tree: BTreeMap<String, (String, String)>,
}

impl WasteLand {
    pub fn walk_and_count(&self) -> usize {
        walk(&self.instructions, &self.map_tree, "AAA")
    }

    pub fn par_walk_and_count(&self) -> usize {
        let mut current_nodes: Vec<&str> = self
            .map_tree
            .keys()
            .filter_map(|key| key.ends_with("A").then_some(key.as_str()))
            .collect();

        let results = current_nodes
            .iter()
            .map(|node| {
                let mut cache_nodes = vec![*node];
                let mut current_node = *node;

                dbg!(&cache_nodes, &current_node);

                self.instructions
                    .iter()
                    .cycle()
                    .enumerate()
                    .find_map(|(index, inst)| {
                        let options = self.map_tree.get(current_node).expect("Run to the hills");

                        let next = match inst {
                            Direction::Left => options.0.as_str(),
                            Direction::Right => options.1.as_str(),
                        };

                        if next.ends_with('Z') {
                            Some(index + 1)
                        } else {
                            cache_nodes.push(next);
                            current_node = next;
                            None
                        }
                    })
                    .expect("Sad but true")
            })
            .collect::<Vec<usize>>();
        dbg!(&results);

        least_common_mult(&results)
    }
}

fn least_common_mult(nums: &[usize]) -> usize {
    if nums.len() == 1 {
        return nums[0];
    }
    let a = nums[0];
    let b = least_common_mult(&nums[1..]);
    a * b / greatest_common_divisor_of_two_numbers(a, b)
}

fn greatest_common_divisor_of_two_numbers(a: usize, b: usize) -> usize {
    if b == 0 {
        return a;
    }
    greatest_common_divisor_of_two_numbers(b, a % b)
}

fn walk(
    instructions: &Vec<Direction>,
    tree: &BTreeMap<String, (String, String)>,
    initial_node: &str,
) -> usize {
    let mut current_node = initial_node;

    let Some(step_count) = instructions
        .iter()
        .cycle()
        .enumerate()
        .find_map(|(index, inst)| {
            let options = tree.get(current_node).expect("Run to the hills");

            let next = match inst {
                Direction::Left => options.0.as_str(),
                Direction::Right => options.1.as_str(),
            };

            if next.chars().last().unwrap() == 'Z' {
                Some(index + 1)
            } else {
                current_node = next;
                None
            }
        })
    else {
        panic!("FOR DAT")
    };

    step_count
}

impl FromStr for WasteLand {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (instructions, map_tree) = s
            .split_once('\n')
            .map(|(ins, tree)| {
                let ins_vec = ins
                    .chars()
                    .map(|ch| match ch {
                        'L' => Direction::Left,
                        'R' => Direction::Right,
                        _ => panic!("Run for your liveess"),
                    })
                    .collect::<Vec<_>>();

                let map_tree: BTreeMap<_, _> = tree
                    .trim()
                    .lines()
                    .map(|tree_line| {
                        let line_parts = tree_line
                            .trim()
                            .split_once("=")
                            .map(|(a, b)| (a.trim(), b.trim()))
                            .unwrap();

                        let left = line_parts.0.to_string();

                        let right_coords = line_parts
                            .1
                            .split_once(",")
                            .map(|(left_i, right_i)| {
                                (
                                    left_i.trim().replace("(", ""),
                                    right_i.trim().replace(")", ""),
                                )
                            })
                            .unwrap_or_default();

                        (left, (right_coords.0, right_coords.1))
                    })
                    .collect();

                (ins_vec, map_tree)
            })
            .unwrap();

        Ok(WasteLand {
            instructions,
            map_tree,
        })
    }
}
