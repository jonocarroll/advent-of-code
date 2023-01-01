use std::{collections::HashMap};

pub fn part_one(input: &str) -> Option<u32> {
    let mut dir_deque = vec![];
    let mut current_dir = String::from("");
    let mut filesystem = HashMap::new();
    for l in input.lines() {
        if l == "$ cd .." {
            dir_deque.pop().unwrap();
            current_dir = dir_deque.join("");
            continue;
        } else if l == "$ cd /" {
            current_dir = String::from("/");
            filesystem.insert(current_dir.clone(), 0);
            dir_deque = vec![String::from("/")];
            continue;
        } else if l.starts_with("dir") {
            continue;
        } else if l.starts_with("$ cd") {
            let new_dir = l.replace("$ cd ", "");
            dir_deque.push(new_dir.clone() + &"/");
            current_dir = current_dir + &new_dir.clone() + &"/";
            filesystem.insert(current_dir.clone(), 0);
            continue;
        } else if char::is_digit(l.chars().nth(1).unwrap(), 10) {
            let parts = l.split_whitespace().collect::<Vec<_>>();
            let dir_size = parts[0].parse::<u32>().unwrap();
            for d in 0..dir_deque.len() {
                let this_d = dir_deque[0..=d].join("");
                let known_size = filesystem.get(&this_d).unwrap();
                filesystem.insert(this_d, known_size + dir_size);
            }
            continue;
        }
    }

    let totalsize = filesystem.iter()
            .filter(|&(_k, v)| *v <= 1e5 as u32)
            .map(|(_k, v)| *v)
            .collect::<Vec<u32>>()
            .iter()
            .sum();

    Some(totalsize)
        
}

pub fn part_two(input: &str) -> Option<u32> {
    let mut dir_deque = vec![];
    let mut current_dir = String::from("");
    let mut filesystem = HashMap::new();
    for l in input.lines() {
        if l == "$ cd .." {
            dir_deque.pop().unwrap();
            current_dir = dir_deque.join("");
            continue;
        } else if l == "$ cd /" {
            current_dir = String::from("/");
            filesystem.insert(current_dir.clone(), 0);
            dir_deque = vec![String::from("/")];
            continue;
        } else if l.starts_with("dir") {
            continue;
        } else if l.starts_with("$ cd") {
            let new_dir = l.replace("$ cd ", "");
            dir_deque.push(new_dir.clone() + &"/");
            current_dir = current_dir + &new_dir.clone() + &"/";
            filesystem.insert(current_dir.clone(), 0);
            continue;
        } else if char::is_digit(l.chars().nth(1).unwrap(), 10) {
            let parts = l.split_whitespace().collect::<Vec<_>>();
            let dir_size = parts[0].parse::<u32>().unwrap();
            for d in 0..dir_deque.len() {
                let this_d = dir_deque[0..=d].join("");
                let known_size = filesystem.get(&this_d).unwrap();
                filesystem.insert(this_d, known_size + dir_size);
            }
            continue;
        }
    }

    let to_delete = filesystem.get("/").unwrap() - (4e7 as u32);
    let candidates = filesystem.iter()
        .filter(|&(_k, v)| *v >= to_delete)
        .map(|(_k, v)| *v)
        .collect::<Vec<u32>>();
    Some(*candidates.iter().min().unwrap())

}

fn main() {
    let input = &advent_of_code::read_file("inputs", 7);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_one(&input), Some(95437));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 7);
        assert_eq!(part_two(&input), Some(24933642));
    }
}
