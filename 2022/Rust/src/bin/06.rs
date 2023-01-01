use std::collections::HashSet;

pub fn part_one(input: &str) -> Option<u32> {
    let mut i: u32 = 1;
    let mut recent = HashSet::new();
    let mut lastchars = vec![' '; 3];

    for c in input.chars() {
        for j in 0..3 {
            recent.insert(lastchars[j]);
        }
        recent.insert(c);
        if i > 3 && recent.len() == 4 {
            break
        };
        for i in 0..2 {
            lastchars[i] = lastchars[i+1];
        }
        lastchars[2] = c;
        recent.clear();
        i += 1;
    }
    Some(i)
}

pub fn part_two(input: &str) -> Option<u32> {
    let mut i: u32 = 1;
    let mut recent = HashSet::new();
    let mut lastchars = vec![' '; 13];

    for c in input.chars() {
        for j in 0..13 {
            recent.insert(lastchars[j]);
        }
        recent.insert(c);
        if i > 13 && recent.len() == 14 {
            break
        };
        for i in 0..12 {
            lastchars[i] = lastchars[i+1];
        }
        lastchars[12] = c;
        recent.clear();
        i += 1;
    }
    Some(i)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 6);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        assert_eq!(part_one(&"bvwbjplbgvbhsrlpgdmjqwftvncz"), Some(5));
        assert_eq!(part_one(&"nppdvjthqldpwncqszvftbrmjlhg"), Some(6));
        assert_eq!(part_one(&"mjqjpqmgbljsphdztnvjfqwrcgsmlb"), Some(7));
        assert_eq!(part_one(&"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), Some(10));
        assert_eq!(part_one(&"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), Some(11));
    }

    #[test]
    fn test_part_two() {
        assert_eq!(part_two(&"mjqjpqmgbljsphdztnvjfqwrcgsmlb"), Some(19));
        assert_eq!(part_two(&"bvwbjplbgvbhsrlpgdmjqwftvncz"), Some(23));
        assert_eq!(part_two(&"nppdvjthqldpwncqszvftbrmjlhg"), Some(23));
        assert_eq!(part_two(&"nznrnfrfntjfmvfwmzdfjlvtqnbhcprsg"), Some(29));
        assert_eq!(part_two(&"zcfzfwzzqfrljwzlrfnpqdbhtmscgvjw"), Some(26));
    }
}
