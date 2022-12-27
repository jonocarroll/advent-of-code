fn parse01(input: &str) -> Vec<u32> {
    let elf = input.split("\n\n").collect::<Vec<&str>>();
    let calories: Vec<u32> = elf
        .into_iter()
        .map(|x| x.lines().map(|l| l.parse::<u32>().unwrap()).sum())
        .collect();
    calories
}

pub fn part_one(input: &str) -> Option<u32> {
    let calories = parse01(input);
    calories.into_iter().max()
}

pub fn part_two(input: &str) -> Option<u32> {
    let mut calories = parse01(input);
    calories.sort();
    calories.reverse();
    let top3 = calories.iter().take(3);
    Some(top3.sum())
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 1);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 1);
        assert_eq!(part_one(&input), Some(24000));
    }

#[test]
fn test_part_two() {
    let input = advent_of_code::read_file("examples", 1);
    assert_eq!(part_two(&input), Some(45000));
}
}
