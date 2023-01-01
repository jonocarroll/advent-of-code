pub fn part_one(input: &str) -> Option<String> {
    let (stacks, instr) = parse05(&input);
    let mut crates = stacks.crates();
    // println!("{:?}", instr);
    for i in 0..instr.len() {
        // println!("{:?}", instr[i].parse());
        // println!("{:?}", crates);
        crates = crane(crates, instr[i].parse());
        // println!("{:?}", crates);
    }
    let tops = crates.iter().map(|s| s.last().unwrap()).collect::<String>();
    Some(tops)
}

pub fn part_two(input: &str) -> Option<String> {
    let (stacks, instr) = parse05(&input);
    let mut crates = stacks.crates();

    for i in 0..instr.len() {
        // println!("{:?}", instr[i].parse());
        // println!("{:?}", crates);
        crates = crane9001(crates, instr[i].parse());
        // println!("{:?}", crates);
    }
    let tops = crates.iter().map(|s| s.last().unwrap()).collect::<String>();
    Some(tops)
}

fn crane(crates: Vec<Vec<char>>, instr: (usize, usize, usize)) -> Vec<Vec<char>> {
    // println!("n = {:?}", instr.0);
    let mut tmpcrates = crates.clone(); //TODO use references!
    for _i in 0..instr.0 {
        let tomove: char = tmpcrates[instr.1 - 1].pop().unwrap();
        tmpcrates[instr.2 - 1].push(tomove);
    }
    tmpcrates
}

fn crane9001(crates: Vec<Vec<char>>, instr: (usize, usize, usize)) -> Vec<Vec<char>> {
    let mut tmpcrates = crates.clone(); 
    let new_len = tmpcrates[instr.1 - 1].len();
    let mut tomove = vec![];
    for _i in 0..instr.0 {
        tomove.push(tmpcrates[instr.1 - 1].pop().unwrap());
    }
    tomove.reverse();
    tmpcrates[instr.1 - 1].truncate(new_len - instr.0);
    for x in tomove.into_iter() {
        tmpcrates[instr.2 - 1].push(x);
    }
    tmpcrates
}

#[derive(Debug)]
struct Stacks {
    stacks: String,
}

impl Stacks {
    fn crates(&self) -> Vec<Vec<char>> {
        let stacklines = &self
            .stacks
            .lines()
            .into_iter()
            .map(|x| x.chars().collect::<Vec<char>>())
            .collect::<Vec<Vec<char>>>();
        let mut stackentries = vec![];
        for l in stacklines.iter() {
            stackentries.push(l.iter().skip(1).step_by(4).collect::<Vec<&char>>());
        }
        // reshape to stacks
        let mut stack = vec![vec![' '; 50]; stackentries[1].len()];
        for s in 0..stackentries.len() - 1 {
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
    input: String,
}

impl Instructions {
    fn parse(&self) -> (usize, usize, usize) {
        // regex is slooooow!
        // let re = Regex::new(r"move (\d*) from (\d*) to (\d*)").unwrap();
        // let caps = re.captures(&self.input).unwrap();
        // p1 (regex): 89ms
        // p2 (regex): 73ms
        //
        // p1 (refactored): 403µs
        // p2 (refactored): 431µs
        //
        // p1 (R): 257ms
        // p2 (R): 248ms
        let instr = String::from(&self.input);
        let caps = instr
            .split_whitespace()
            .filter(|c| c.parse::<usize>().is_ok())
            .collect::<Vec<_>>();
        let moveto = caps[0].parse::<usize>().unwrap();
        let from = caps[1].parse::<usize>().unwrap();
        let to = caps[2].parse::<usize>().unwrap();

        (moveto, from, to)
    }
}

fn parse05(input: &str) -> (Stacks, Vec<Instructions>) {
    let parts = input.split_once("\n\n").unwrap();
    let stacks = Stacks {
        stacks: String::from(parts.0),
    };
    let instr = parts
        .1
        .lines()
        .map(|x| Instructions {
            input: String::from(x),
        })
        .collect::<Vec<_>>();
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
        assert_eq!(part_one(&input), Some(String::from("CMZ")));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 5);
        assert_eq!(part_two(&input), Some(String::from("MCD")));
    }
}
