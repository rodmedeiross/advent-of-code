use indicatif::ProgressIterator;
use rayon::prelude::*;
use std::str::FromStr;

macro_rules! generate_group_items {
    ($main:expr, $field_name:ident , $content:expr) => {{
        $main.$field_name =
            $content
                .lines()
                .into_iter()
                .fold(Vec::new(), |mut field, collection| {
                    let to_add = collection
                        .trim_start()
                        .split_whitespace()
                        .map(|number| number.parse::<usize>().unwrap())
                        .collect::<Vec<usize>>();
                    field.push(to_add);
                    field
                });
    }};
}

macro_rules! calculate_coord {
    ($self:expr, $seed:expr, $prop:ident) => {
        $self
            .$prop
            .clone()
            .into_par_iter()
            .filter_map(|coord| {
                let init = coord[1];
                let end = coord[1] + coord[2];

                if let Some(next) = (init..end).progress().position(|x| x == *$seed) {
                    Some(coord[0] + next)
                } else {
                    None
                }
            })
            .max()
            .unwrap_or(*$seed)
    };
}

pub fn read_groups(file: &str) -> Result<Vec<&str>, &'static str> {
    let groups: Vec<&str> = file
        .split_terminator("\n\n")
        .collect::<Vec<&str>>()
        .par_iter()
        .map(|group| group.trim_start())
        .collect();

    match groups.is_empty() {
        true => Err("Input is empty"),
        false => Ok(groups),
    }
}

#[derive(Debug, Default, Clone)]
pub struct Almanac {
    seeds: Vec<usize>,
    seed_to_soil: Vec<Vec<usize>>,
    soil_to_fertilizer: Vec<Vec<usize>>,
    fertilizer_to_water: Vec<Vec<usize>>,
    water_to_light: Vec<Vec<usize>>,
    light_to_temperature: Vec<Vec<usize>>,
    temperature_to_humidity: Vec<Vec<usize>>,
    humidity_to_location: Vec<Vec<usize>>,
}

impl Almanac {
    pub fn new() -> Self {
        Almanac {
            ..Default::default()
        }
    }
    pub fn from_str(input: &str) -> Self {
        input.parse::<Almanac>().unwrap()
    }

    pub fn generate_new_seeds(&self) -> Self {
        let new_seeds: Vec<usize> = self
            .seeds
            .par_chunks(2)
            .flat_map(|chunk| {
                let init = chunk[0];
                let end = init + chunk[1] - 1;
                (init..=end).collect::<Vec<usize>>()
            })
            .collect();

        let mut new_almanac = self.clone();
        new_almanac.seeds = new_seeds;
        new_almanac
    }

    pub fn generate_walk_trace(&self) -> Vec<usize> {
        let mut locations = vec![];
        for seed in &self.seeds {
            let soil_coord = calculate_coord!(&self, seed, seed_to_soil);
            let fertilizer_coord = calculate_coord!(&self, &soil_coord, soil_to_fertilizer);
            let water_coord = calculate_coord!(&self, &fertilizer_coord, fertilizer_to_water);
            let light_coord = calculate_coord!(&self, &water_coord, water_to_light);
            let temperature_coord = calculate_coord!(&self, &light_coord, light_to_temperature);
            let humidity_coord =
                calculate_coord!(&self, &temperature_coord, temperature_to_humidity);
            let location_coord = calculate_coord!(&self, &humidity_coord, humidity_to_location);

            locations.push(location_coord);
        }
        locations
    }
}

impl FromStr for Almanac {
    type Err = &'static str;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let groups = read_groups(s).expect("LOOOKKKK AWAAAYYY");

        let mut almanac = Almanac::new();

        for group in groups {
            let (prefix, content) = group
                .split_once(':')
                .map(|x| (x.0.trim(), x.1.trim_start()))
                .unwrap();

            match prefix {
                "seeds" => {
                    almanac.seeds = content
                        .split_whitespace()
                        .into_iter()
                        .map(|x| x.parse::<usize>().unwrap())
                        .collect::<Vec<usize>>();
                }
                "seed-to-soil map" => {
                    generate_group_items!(almanac, seed_to_soil, content);
                }
                "soil-to-fertilizer map" => {
                    generate_group_items!(almanac, soil_to_fertilizer, content);
                }
                "fertilizer-to-water map" => {
                    generate_group_items!(almanac, fertilizer_to_water, content)
                }
                "water-to-light map" => {
                    generate_group_items!(almanac, water_to_light, content);
                }
                "light-to-temperature map" => {
                    generate_group_items!(almanac, light_to_temperature, content);
                }
                "temperature-to-humidity map" => {
                    generate_group_items!(almanac, temperature_to_humidity, content)
                }
                "humidity-to-location map" => {
                    generate_group_items!(almanac, humidity_to_location, content)
                }
                _ => {
                    continue;
                }
            }
        }
        Ok(almanac)
    }
}
fn main() {}
