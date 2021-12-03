use std::env;
use std::fs;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    // Trash command args
    args.remove(0);

    if let Some(filename) = args.pop() {
        let contents = fs::read_to_string(filename).expect("unable to read file");
        let input: Vec<u32> = contents
            .lines()
            .map(|l| l.parse::<u32>().expect("unable to parse input"))
            .collect();

        println!("Part 1 answer: {}", solve_part_1(&input));

        let input = input
            .iter()
            .zip(input.iter().skip(1))
            .zip(input.iter().skip(2))
            .map(|((a, b), c)| a + b + c)
            .collect::<Vec<u32>>();

        println!("Part 2 answer {}", solve_part_1(&input));
    } else {
        println!("No input file given");
    }
}

// Returns the number of times input increases from the previous value
fn solve_part_1(input: &Vec<u32>) -> u32 {
    let (_, acc) = input.into_iter().fold((None, 0), |(previous, acc), item| {
        let acc = match previous {
            Some(prev) if prev < item => acc + 1,
            _ => acc,
        };

        (Some(item), acc)
    });

    acc
}
