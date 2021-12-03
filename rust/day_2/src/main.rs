use std::env;
use std::fmt::{Display, Formatter};
use std::fs;
use std::str::FromStr;

#[derive(Debug)]
struct ParseError;

impl Display for ParseError {
    fn fmt(&self, f: &mut Formatter) -> std::fmt::Result {
        write!(f, "invalid movement input")
    }
}

impl From<std::num::ParseIntError> for ParseError {
    fn from(_: std::num::ParseIntError) -> Self {
        ParseError
    }
}

enum Movement {
    Forward(u32),
    Up(u32),
    Down(u32),
}

impl FromStr for Movement {
    type Err = ParseError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s
            .split(" ")
            .map(|s| s.trim())
            .collect::<Vec<&str>>()
            .as_slice()
        {
            [dir, count] => {
                let count = count.parse::<u32>()?;

                match *dir {
                    "forward" => Ok(Self::Forward(count)),
                    "up" => Ok(Self::Up(count)),
                    "down" => Ok(Self::Down(count)),
                    _ => Err(ParseError),
                }
            }

            _ => Err(ParseError),
        }
    }
}

fn main() {
    let mut args: Vec<String> = env::args().collect();

    // Trash command args
    args.remove(0);

    if let Some(filename) = args.pop() {
        let contents = fs::read_to_string(filename).expect("unable to read file");
        let movements: Vec<Movement> = contents
            .lines()
            .map(|l| l.parse::<Movement>())
            .collect::<Result<Vec<Movement>, ParseError>>()
            .expect("failed to parse movements");

        println!("Part 1 answer: {}", solve_part_1(&movements));
        println!("Part 2 answer: {}", solve_part_2(&movements));
    }
}

fn solve_part_1(movements: &Vec<Movement>) -> i32 {
    let (position, depth): (i32, i32) =
        movements
            .iter()
            .fold((0, 0), |(position, depth), movement| match *movement {
                Movement::Forward(count) => (position + count as i32, depth),
                Movement::Up(count) => (position, depth - count as i32),
                Movement::Down(count) => (position, depth + count as i32),
            });

    position * depth
}

fn solve_part_2(movements: &Vec<Movement>) -> i32 {
    let (position, depth, _aim): (i32, i32, i32) = movements.iter().fold(
        (0, 0, 0),
        |(position, depth, aim), movement| match *movement {
            Movement::Forward(count) => {
                (position + count as i32, depth + (aim * count as i32), aim)
            }

            Movement::Up(count) => (position, depth, aim - count as i32),
            Movement::Down(count) => (position, depth, aim + count as i32),
        },
    );

    position * depth
}
