pub fn part_one(input: &str) -> Option<u64> {
    let monkeys = parse11(input);
    let mut monkey_items: Vec<Vec<u64>> = vec![vec![]; monkeys.len()];
    let mut inspections: Vec<usize> = vec![0; monkeys.len()];

    for m in 0..monkeys.len() {
        for i in &monkeys[m].items {
            monkey_items[m].push(*i as u64)
        }
    }
    
    let mut this_monkey: Monkey;
    let mut this_monkey_items: Vec<u64>;
    let mut worry;
    let alltests = monkeys.iter().map(|x| x.test as u64).collect::<Vec<u64>>();
    let maxdiv: u64 = alltests.iter().product();
    for _round in 0..20 {
        for this_monkey_i in 0..monkeys.len() {
            this_monkey = monkeys[this_monkey_i].clone();
            this_monkey_items = monkey_items[this_monkey_i].clone();
            for item in &this_monkey_items {
                monkey_items[this_monkey_i].pop();
                inspections[this_monkey_i] += 1;
                worry = apply_fn(this_monkey.op.clone(), item) / 3;
                if worry % (this_monkey.test as u64) == 0 {
                    monkey_items[this_monkey.istrue].push((worry % maxdiv) as u64);
                } else {
                    monkey_items[this_monkey.isfalse].push((worry % maxdiv) as u64);
                }
            } // items
        } // monkeys
    } // rounds
    inspections.sort();
    inspections.reverse();
    
    Some((inspections[0] * inspections[1]) as u64)

}

pub fn part_two(input: &str) -> Option<u64> {
    let monkeys = parse11(input);
    let mut monkey_items: Vec<Vec<u64>> = vec![vec![]; monkeys.len()];
    let mut inspections: Vec<usize> = vec![0; monkeys.len()];

    for m in 0..monkeys.len() {
        for i in &monkeys[m].items {
            monkey_items[m].push(*i as u64)
        }
    }
    
    // let mut this_monkey: Monkey;
    // let mut this_monkey_items: Vec<u64>;
    let mut worry;
    let alltests = monkeys.iter().map(|x| x.test as u64).collect::<Vec<u64>>();
    let maxdiv: u64 = alltests.iter().product();
    for _round in 0..10_000 {
        for this_monkey_i in 0..monkeys.len() {
            // this_monkey = monkeys[this_monkey_i].clone();
            // this_monkey_items = monkey_items[this_monkey_i].clone();
            for item in monkey_items[this_monkey_i].clone() {
                monkey_items[this_monkey_i].pop();
                inspections[this_monkey_i] += 1;
                worry = apply_fn(monkeys[this_monkey_i].op.clone(), &item);
                if worry % (monkeys[this_monkey_i].test as u64) == 0 {
                    monkey_items[monkeys[this_monkey_i].istrue].push((worry % maxdiv) as u64);
                } else {
                    monkey_items[monkeys[this_monkey_i].isfalse].push((worry % maxdiv) as u64);
                }
            } // items
        } // monkeys
    } // rounds
    inspections.sort();
    inspections.reverse();

    Some((inspections[0] * inspections[1]) as u64)

}

#[derive(Debug, Clone)]
struct Monkey {
    _monkey: usize,
    items: Vec<u32>,
    test: u32,
    istrue: usize,
    isfalse: usize,
    op: Vec<char>,
}

fn parse11(input: &str) -> Vec<Monkey> {
    let input_lines = input.lines().collect::<Vec<_>>()
        .split(|s| s.is_empty())
        .filter(|s| !s.is_empty())
        .map(|s| s.join("\n"))
        .collect::<Vec<String>>();
    let mut monkeys = vec![];
    for l in input_lines.iter() {
        let m = &l.lines().collect::<Vec<&str>>();
        monkeys.push(
            Monkey { 
                _monkey: m[0].replace("Monkey ", "").replace(":", "").parse::<usize>().unwrap(),
                items: m[1].replace(" Starting items: ", "").split(",").map(|x| x.replace(" ", "").parse::<u32>().unwrap()).collect(),
                test: m[3].replace("  Test: divisible by ", "").parse::<u32>().unwrap(),
                istrue: m[4].replace("    If true: throw to monkey ", "").parse::<usize>().unwrap(),
                isfalse: m[5].replace("    If false: throw to monkey ", "").parse::<usize>().unwrap(),
                op: m[2].replace("  Operation: new = old ", "").chars().collect::<Vec<char>>()
            }
        )        
    }
    monkeys
}

fn apply_fn(f: Vec<char>, old: &u64) -> u64 {
    let op = &f[0];
    let val = &String::from(f[2..].iter().collect::<String>());
    let newval = if val == "old" {
        match op {
            '+' => *old + *old,
            '*' => *old * *old,
            _ => *old
        }
    } else {
        let ival = &val.parse::<u64>().unwrap();
        match op {
            '+' => *old + ival,
            '*' => *old * ival,
            _ => *old
        }
    };
    newval as u64
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 11);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 11);
        assert_eq!(part_one(&input), Some(10605));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 11);
        assert_eq!(part_two(&input), Some(2713310158));
    }
}
