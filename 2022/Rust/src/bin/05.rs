use regex::Regex;

pub fn part_one(input: &str) -> Option<String> {
    // let parts = 
    let (stacks, instr) = parse05(&input);
    
    println!("{:?}", stacks.crates());
    println!("{:?}", instr.parse());
    None
}

pub fn part_two(_input: &str) -> Option<String> {
    None
}

#[derive(Debug)]
struct Stacks {
    stacks: String
}

impl Stacks {
    fn crates(&self) -> Vec<Vec<char>> {
        let stacklines = &self.stacks
        .lines()
        .into_iter()
        .map(|x| x.chars().collect::<Vec<char>>())
        .collect::<Vec<Vec<char>>>();
    let mut stackentries = vec![];
    for l in stacklines.iter() {
        stackentries.push(l.iter().skip(1).step_by(4).collect::<Vec<&char>>());
    }
    // reshape to stacks
    let mut stack = vec![vec![' '; stackentries.len()-1]; stackentries[1].len()];
    for s in 0..stackentries.len()-1 {
        // let items = ;
        for el in 0..stackentries[s].len() {
            stack[el][s] = stackentries[s][el].to_owned()
        }
    }
    for s in 0..stack.len() {
        stack[s].reverse();
        stack[s].retain(|x| *x != ' ');
    }

    stack
    }
}


#[derive(Debug)]
struct Instructions {
    input: String
}

impl Instructions {
    fn parse(&self) -> (u32, u32, u32) {
        // if let [Ok(tomove), Ok(from), Ok(to)] = &self.input.split(" ")
        // .map(|a| a.parse::<u32>())
        // .collect::<Vec<_>>()[..] {
        //     (tomove.to_owned(), from.to_owned(), to.to_owned())
        // } else {
        //     (0, 0, 0)
        // }
        let re = Regex::new(r"move (\d) from (\d) to (\d)").unwrap();
        let caps = re.captures(&self.input).unwrap();
        let moveto = caps[1].parse::<u32>().unwrap();
        let from = caps[2].parse::<u32>().unwrap();
        let to = caps[3].parse::<u32>().unwrap();

        (moveto, from, to)
    }
}

fn parse05(input: &str) -> (Stacks, Instructions) {
    let parts = input.split_once("\n\n").unwrap();
    let stacks = Stacks { stacks: String::from(parts.0) };
    println!("{:?}", parts.1);
    let instr = Instructions { input: String::from(parts.1) };
    (stacks, instr)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 5);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 5);
        // println!("{:?}", parse05(input));
        assert_eq!(part_one(&input), None);
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), None);
    }
}
