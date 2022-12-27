fn parse02(input: &str) -> Vec<String> {
    let guide = input.lines();
    guide.into_iter().map(|x| x.replace(" ", "")).collect()
}

pub fn part_one(input: &str) -> Option<u32> {
    let guide = parse02(input);
    let res: Vec<u32> = guide
        .into_iter()
        .map(|x| match x.as_str() {
            "AX" => 1 + 3,
            "AY" => 2 + 6,
            "AZ" => 3 + 0,
            "BX" => 1 + 0,
            "BY" => 2 + 3,
            "BZ" => 3 + 6,
            "CX" => 1 + 6,
            "CY" => 2 + 0,
            "CZ" => 3 + 3,
            _ => 0,
        })
        .collect();
    Some(res.iter().sum())
}

pub fn part_two(input: &str) -> Option<u32> {
    let guide = parse02(input);
    let res: Vec<u32> = guide
        .into_iter()
        .map(|x| match x.as_str() {
            "AX" => 3 + 0,
            "AY" => 1 + 3,
            "AZ" => 2 + 6,
            "BX" => 1 + 0,
            "BY" => 2 + 3,
            "BZ" => 3 + 6,
            "CX" => 2 + 0,
            "CY" => 3 + 3,
            "CZ" => 1 + 6,
            _ => 0,
        })
        .collect();
    Some(res.iter().sum())
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 2);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_one(&input), Some(15));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 2);
        assert_eq!(part_two(&input), Some(12));
    }
}
