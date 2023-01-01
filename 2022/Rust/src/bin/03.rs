use std::collections::HashSet;

fn parse03(input: &str) -> Vec<String> {
    input.lines().map(|x| x.to_string()).collect()
}

fn shared_item(rucksack: String) -> Vec<char> {
    let l = rucksack.len();
    let (str1, str2) = rucksack.split_at(l / 2);

    let comp1: HashSet<char> = HashSet::from_iter(str1.chars());
    let comp2: HashSet<char> = HashSet::from_iter(str2.chars());

    let common = comp1.intersection(&comp2);

    common.copied().collect()
}

fn badge(rucksacks: Vec<String>) -> Vec<char> {
    let mut badges = vec![];

    for group in rucksacks.chunks(3) {
        let h1: HashSet<char> = HashSet::from_iter(group[0].chars());
        let h2: HashSet<char> = HashSet::from_iter(group[1].chars());
        let h3: HashSet<char> = HashSet::from_iter(group[2].chars());

        // https://www.reddit.com/r/rust/comments/zbikje/comment/iys0sgr/?utm_source=share&utm_medium=web2x&context=3
        let common: Vec<_> = h1
            .iter()
            .filter(|e| h2.contains(e) && h3.contains(e))
            .collect();
        badges.push(*common[0]);
    }

    badges
}

fn priority(item: char) -> u32 {
    match item {
        lowercase @ 'a'..='z' => lowercase as u32 - ('a' as u32) + 1,
        uppercase @ 'A'..='Z' => uppercase as u32 - ('A' as u32) + 27,
        _ => 0,
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let parsed = parse03(&input);
    let repeated: Vec<_> = parsed.iter().map(|x| shared_item(x.to_owned())).collect();
    // println!("{:?}", repeated);
    let mut s = 0;
    for c in repeated {
        s += priority(c[0])
    }
    Some(s)
}

pub fn part_two(input: &str) -> Option<u32> {
    let parsed = parse03(&input);
    let badge: Vec<_> = badge(parsed);
    // println!("{:?}", repeated);
    let mut s = 0;
    for c in badge {
        s += priority(c)
    }
    Some(s)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 3);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        // let input = advent_of_code::read_file("examples", 3);
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_one(&input), Some(157));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 3);
        assert_eq!(part_two(&input), Some(70));
    }
}
