#[derive(Debug)]
struct Assignments {
    sections: String,
}

impl Assignments {
    fn ids(&self) -> std::ops::Range<u32> {
        let rangelimits = &self.sections.split_once('-').unwrap();
        let start = rangelimits.0.parse::<u32>().unwrap();
        let end = rangelimits.1.parse::<u32>().unwrap();
        start..end
    }
}

fn fully_contains(pairs: Vec<Assignments>) -> bool {
    let p1 = pairs[0].ids();
    let p2 = pairs[1].ids();
    if p1.len() >= p2.len() {
        return p1.start <= p2.start && p1.end >= p2.end;
    } else {
        return p2.start <= p1.start && p2.end >= p1.end;
    }
}

fn overlap_at_all(pairs: Vec<Assignments>) -> bool {
    let p1 = pairs[0].ids();
    let p2 = pairs[1].ids();
    // https://stackoverflow.com/a/325964/4168169
    // (StartA <= EndB) and (EndA >= StartB)
    p1.start <= p2.end && p1.end >= p2.start
}

fn create_assignments(line: &str) -> Vec<Assignments> {
    let pair = line.split_once(',').unwrap();
    let p1 = Assignments {
        sections: pair.0.to_string(),
    };
    let p2 = Assignments {
        sections: pair.1.to_string(),
    };
    vec![p1, p2]
}

fn parse04(input: &str) -> Vec<Vec<Assignments>> {
    let l = input.lines();
    l.into_iter().map(|x| create_assignments(x)).collect()
}

pub fn part_one(input: &str) -> Option<u32> {
    let all_assignments = parse04(input);
    let mut overlapping = 0;
    for ass in all_assignments {
        if fully_contains(ass) {
            overlapping += 1;
        }
    }
    Some(overlapping)
}

pub fn part_two(input: &str) -> Option<u32> {
    let all_assignments = parse04(input);
    let mut overlapping = 0;
    for ass in all_assignments {
        if overlap_at_all(ass) {
            overlapping += 1;
        }
    }
    Some(overlapping)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 4);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_one(&input), Some(2));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 4);
        assert_eq!(part_two(&input), Some(4));
    }
}
