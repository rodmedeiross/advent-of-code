use std::str::FromStr;

#[derive(Default, Debug)]
struct GamePrize {
    a: (i128, i128),
    b: (i128, i128),
    prize: (i128, i128),
}

impl FromStr for GamePrize {
    type Err = &'static str;

    fn from_str(i: &str) -> Result<Self, Self::Err> {
        let input = i
            .lines()
            .flat_map(|line| {
                line.split_once(":").and_then(|(f, s)| {
                    if f.contains("Prize") {
                        s.split_once(",").and_then(|(inf, ins)| {
                            Some((
                                inf.split_once("=")
                                    .and_then(|(_, v)| Some(v.trim().parse::<i128>().unwrap())),
                                ins.split_once("=")
                                    .and_then(|(_, v)| Some(v.trim().parse::<i128>().unwrap())),
                            ))
                        })
                    } else {
                        s.split_once(",").and_then(|(inf, ins)| {
                            Some((
                                inf.split_once("+")
                                    .and_then(|(_, v)| Some(v.trim().parse::<i128>().unwrap())),
                                ins.split_once("+")
                                    .and_then(|(_, v)| Some(v.trim().parse::<i128>().unwrap())),
                            ))
                        })
                    }
                })
            })
            .filter_map(|(a, b)| match (a, b) {
                (Some(x), Some(y)) => Some((x, y)),
                _ => None,
            })
            .collect::<Vec<(i128, i128)>>();

        Ok(GamePrize::new(input))
    }
}

impl GamePrize {
    fn new(input: Vec<(i128, i128)>) -> Self {
        GamePrize {
            a: input[0],
            b: input[1],
            prize: input[2],
        }
    }
}

fn main() {
    let input = include_str!("input.txt");
    let result = process(input);
    println!("Results {}", result);
}

fn process(i: &str) -> i128 {
    let game_rule = i
        .split("\n\n")
        .map(|item| item.parse::<GamePrize>().unwrap())
        .collect::<Vec<GamePrize>>();

    dbg!(&game_rule);

    let mut total = 0_i128;
    // for game in game_rule {
    //     let mut min_score = f64::INFINITY;
    //
    //     for i in 0.. {
    //         for j in 0.. {
    //             if game.a.0 * i + game.b.0 * j == game.prize.0
    //                 && game.a.1 * i + game.b.1 * j == game.prize.1
    //             {
    //                 min_score = f64::min(min_score, (i * 3 + j) as f64);
    //                 break;
    //             }
    //         }
    //     }
    //
    //     if min_score != f64::INFINITY {
    //         total += min_score;
    //     }
    // }

    for game in game_rule {
        let ta = ((game.prize.0 + 10000000000000) * game.b.1
            - (game.prize.1 + 10000000000000) * game.b.0)
            / (game.a.0 * game.b.1 - game.a.1 * game.b.0);
        let tb = ((game.prize.0 + 10000000000000) - game.a.0 * ta) / game.b.0;

        if ta % 1 == 0 && tb % 1 == 0 {
            // if ta <= 100 && tb <= 100 {
            total += ta * 3 + tb;
            // }
        }
    }

    total
}

#[cfg(test)]
mod tests {
    use super::*;
    use rstest::*;

    #[rstest]
    #[case(
        "Button A: X+94, Y+34
Button B: X+22, Y+67
Prize: X=8400, Y=5400

Button A: X+26, Y+66
Button B: X+67, Y+21
Prize: X=12748, Y=12176

Button A: X+17, Y+86
Button B: X+84, Y+37
Prize: X=7870, Y=6450

Button A: X+69, Y+23
Button B: X+27, Y+71
Prize: X=18641, Y=10279",
        480
    )]
    fn should_get_tokens_to_get_prizes(#[case] input: &str, #[case] expected: i128) {
        let result = process(input);
        assert_eq!(result, expected);
    }
}
