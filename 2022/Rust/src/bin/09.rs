use std::collections::HashSet;
use std::ops::Add;

#[derive(Debug, Eq, PartialEq, Copy, Clone, Hash)]
struct Point<T>(T, T);

impl<T: Add<Output = T>> Add for Point<T> {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Self(self.0 + other.0, self.1 + other.1)
    }
}

pub fn part_one(input: &str) -> Option<u32> {
    let movements = input.lines();
    let mut visited: HashSet<Point<i32>> = HashSet::new();
    let mut head_pos = Point(0, 0);
    let mut tail_pos = Point(0, 0);

    visited.insert(tail_pos);
    for x in movements {
        (head_pos, tail_pos, visited) = follow(head_pos, tail_pos, x, visited);
    }

    Some(visited.len() as u32)
}


fn follow(mut head: Point<i32>, mut tail: Point<i32>, instr: &str, mut visited: HashSet<Point<i32>>) -> (Point<i32>, Point<i32>, HashSet<Point<i32>>) {
    let parts = instr.split_whitespace().collect::<Vec<_>>();
    let dir = parts[0];
    let dist = parts[1].parse::<usize>().unwrap();
    for _d in 0..dist {
        head = move_head(&head, dir);
        tail = move_tail(&head, &tail);
        visited.insert(tail);
    }
    (head, tail, visited)
}

fn follow_knots(mut knots: Vec<Point<i32>>, instr: &str, mut visited: HashSet<Point<i32>>) -> (Vec<Point<i32>>, HashSet<Point<i32>>) {
    let parts = instr.split_whitespace().collect::<Vec<_>>();
    let dir = parts[0];
    let dist = parts[1].parse::<usize>().unwrap();
    for _d in 0..dist {
        knots[0] = move_head(&knots[0], dir);
        for k in 1..knots.len() {
            knots[k] = move_tail(&knots[k-1], &knots[k]);
        }
        visited.insert(knots[knots.len()-1]);
    }
    (knots, visited)

}

fn move_head(head: &Point<i32>, dir: &str) -> Point<i32> {
    match dir {
        "L" => *head + Point(-1, 0),
        "R" => *head + Point(1, 0),
        "U" => *head + Point(0, -1),
        "D" => *head + Point(0, 1),
        _ => Point(0, 0)
    }
}

fn touching(head: &Point<i32>, tail: &Point<i32>) -> bool {
    head == tail || 
    ((head.0 - tail.0).abs() <= 1 && (head.1 - tail.1).abs() <= 1)
}

fn move_tail(head: &Point<i32>, tail: &Point<i32>) -> Point<i32> {
    if touching(&head, &tail) { return *tail }
    if tail.0 == head.0 { 
        return Point(tail.0, tail.1 + (head.1 - tail.1).signum())
    } else if tail.1 == head.1 {
        return Point(tail.0 + (head.0 - tail.0).signum(), tail.1)
    } else {
        return Point(
            tail.0 + (head.0 - tail.0).signum(), 
            tail.1 + (head.1 - tail.1).signum()
        )
    }
}

pub fn part_two(input: &str) -> Option<u32> {
    let movements = input.lines();
    let mut visited: HashSet<Point<i32>> = HashSet::new();
    let mut knots = vec![Point(0, 0); 10];
    
    visited.insert(knots[knots.len()-1]);
    for x in movements {
        (knots, visited) = follow_knots(knots, x, visited);
    }

    Some(visited.len() as u32)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 9);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 9);
        let input1 = input.split_once("\n\n").unwrap().0;
        assert_eq!(part_one(&input1), Some(13));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 9);
        let input2 = input.split_once("\n\n").unwrap().1;
        assert_eq!(part_two(&input2), Some(36));
    }
}
