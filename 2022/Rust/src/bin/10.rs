use std::str;

pub fn part_one(input: &str) -> Option<u32> {
    let mut register: isize = 1;
    let mut strength: isize = 0;
    let mut device_output: Vec<(&str, isize)>;
    let mut cycle: isize = 0;
    let cycles: Vec<isize> = (20..=220).step_by(40).collect();
    for s in input.lines() {
        device_output = add_signal(s);
        for x in device_output.iter() {
            cycle += 1;
            if cycles.contains(&cycle) {
                strength += register * cycle;
            }
            register += x.1;
        }
    }
    Some(strength as u32)
}

pub fn part_two(input: &str) -> Option<String> {
    let mut register: isize = 1;
    let mut device_output: Vec<(&str, isize)>;
    let mut cycle: isize = 1;
    let mut screen: Vec<&str> = vec![];
    for s in input.lines() {
        device_output = add_signal(s);
        for x in device_output.iter() {
            if (((cycle - 1) % 40) - register).abs() < 2  {
                screen.push("#") // "#" ~ 40micros
                // screen.push("█") // "#" ~ 100micros
            } else {
                screen.push(".") // .
                // screen.push(" ") // .
            }
            cycle += 1;
            register += x.1;
        }
    }
    
    for (i, _) in screen.iter().enumerate() {
        if i % 40 == 0 {
            println!("{:?}", &screen[i..i+40].join(""));
        }
    }

    Some("PCPBKAPJ".to_string())
}

fn add_signal(signal: &str) -> Vec<(&str, isize)> {
    if signal == "noop" {
        return vec![("noop", 0)]
    } else {
        let val = signal.replace("addx ", "").parse::<isize>().unwrap();
        return vec![(signal, 0), ("wait", val)]
    }
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 10);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 10);
        assert_eq!(part_one(&input), Some(13140));
    }

    // #[test]
    // fn test_part_two() {
    //     let input = advent_of_code::read_file("examples", 10);
    //     assert_eq!(part_two(&input), None);
    // }
}
