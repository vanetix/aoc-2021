use std::env;
use std::fs;

fn main() {
    let mut args: Vec<String> = env::args().collect();

    // Trash command args
    args.remove(0);

    if let Some(filename) = args.pop() {
        let contents = fs::read_to_string(filename).expect("unable to read file");
        let rows: Vec<Vec<String>> = contents
            .lines()
            .map(|l| l.trim().chars().map(|c| c.into()).collect())
            .collect();

        println!("Part 1 answer: {}", solve_part_1(&rows));
        // println!("Part 2 answer: {}", solve_part_2(&movements));
    }
}

fn solve_part_1(input: &Vec<Vec<String>>) -> i32 {
    let transposed: Vec<(u32, u32)> = transpose(input)
        .map(|row| {
            row.fold((0, 0), |(zeroes, ones), item| match item.as_ref() {
                "0" => (zeroes + 1, ones),
                "1" => (zeroes, ones + 1),
                _ => (zeroes, ones),
            })
        })
        .collect();

    let most = transposed
        .iter()
        .map(|(zeroes, ones)| if zeroes > ones { "0" } else { "1" })
        .collect::<Vec<&str>>()
        .join("");

    let least = transposed
        .iter()
        .map(|(zeroes, ones)| if zeroes < ones { "0" } else { "1" })
        .collect::<Vec<&str>>()
        .join("");

    (isize::from_str_radix(&most, 2).unwrap() * isize::from_str_radix(&least, 2).unwrap()) as i32
}

// fn solve_part_2(movements: &Vec<Movement>) -> i32 {}

// Taken from:
// https://users.rust-lang.org/t/is-there-a-better-way-to-implement-this-transposing-iterator/51025/2
fn transpose<'iter, I, Item: 'iter>(
    columns: &'iter [I],
) -> impl Iterator<Item = impl Iterator<Item = &'iter Item>>
where
    &'iter I: IntoIterator<Item = &'iter Item>,
{
    (0..).scan((), move |&mut (), row_idx| {
        Some({
            let mut columns_iterator = columns.iter();
            let first_column = columns_iterator.next()?;
            let first: &'iter Item = first_column.into_iter().nth(row_idx)?;

            Iterator::chain(
                ::core::iter::once(first),
                columns_iterator.map(move |column| {
                    column.into_iter().nth(row_idx).unwrap() // assumes the columns are of equal length
                }),
            )
        })
    })
}
