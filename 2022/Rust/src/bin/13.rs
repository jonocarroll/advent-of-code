use serde_json::Value;
use std::cmp::Ordering;

pub fn part_one(input: &str) -> Option<u32> {
    let pairs = input.split("\n\n").collect::<Vec<&str>>();
    let mut in_order = 0;
    for (i, pair) in pairs.iter().enumerate() {
        let (p1, p2) = parse_packets(pair);
        if p1.cmp(&p2) == Ordering::Less {
            in_order += i + 1;
        }
    }
    Some(in_order as u32)
}

pub fn part_two(input: &str) -> Option<u32> {
    let s = input.lines().filter(|&x| !x.is_empty()).collect::<Vec<&str>>();

    let mut decoder_key: u32 = 1;

    let mut allpackets: Vec<Packet> = vec![];
    for p in s {
        allpackets.push(parse_single_packet(p));
    }

    let div_pack1 = parse_single_packet("[[2]]");
    allpackets.push(div_pack1.clone());
    let div_pack2 = parse_single_packet("[[6]]");
    allpackets.push(div_pack2.clone());

    allpackets.sort();

    for (i, v) in allpackets.iter().enumerate() {
        if *v == div_pack1 || *v == div_pack2 {
            decoder_key *= (i + 1) as u32
        }
    }

    Some(decoder_key)
}

//Packet data consists of lists and integers
#[derive(Debug, Clone, Eq, PartialEq)]
enum Packet {
    Value(u32),
    List(Vec<Packet>)
}

impl Ord for Packet {
    fn cmp(&self, other: &Self) -> Ordering {
        // (self.value, &self.name).cmp(&(other.value, &other.name))
        match (self, other) {
            // * If *both values are integers*, the *lower integer* should come
            //   first. If the left integer is lower than the right integer, the
            //   inputs are in the right order. If the left integer is higher
            //   than the right integer, the inputs are not in the right order.
            //   Otherwise, the inputs are the same integer; continue checking
            //   the next part of the input.
            (Packet::Value(v1), Packet::Value(v2)) => v1.cmp(&v2),
            // * If *both values are lists*, compare the first value of each
            //   list, then the second value, and so on. If the left list runs
            //   out of items first, the inputs are in the right order. If the
            //   right list runs out of items first, the inputs are not in the
            //   right order. If the lists are the same length and no comparison
            //   makes a decision about the order, continue checking the next
            //   part of the input.
            (Packet::List(l1), Packet::List(l2)) => l1.cmp(&l2),
            // * If *exactly one value is an integer*, convert the integer to a
            //   list which contains that integer as its only value, then retry
            //   the comparison. For example, if comparing `[0,0,0]` and `2`,
            //   convert the right value to `[2]` (a list containing `2`); the
            //   result is then found by instead comparing `[0,0,0]` and `[2]`.
            (Packet::Value(v), Packet::List(l)) => {
                Packet::List(vec![Packet::Value(*v)]).cmp(&Packet::List(l.clone()))
            },
            (Packet::List(l), Packet::Value(v)) => {
                Packet::List(l.clone()).cmp(&Packet::List(vec![Packet::Value(*v)]))
            }
        }
    }
}

impl PartialOrd for Packet {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

fn create_packets(val: Value) -> Result<Packet, String> {
        match val {
            Value::Array(a) => {
                let mut contents = vec![];
                for i in a {
                    contents.push(create_packets(i).unwrap());
                }
                Ok(Packet::List(contents))
            },
            Value::Number(n) => {
                Ok(Packet::Value(n.as_u64().unwrap() as u32))
            },
            _ => Err("Failed to parse".to_string())
        }
}

fn parse_single_packet(s: &str) -> Packet {
    let val: Value = serde_json::from_str(s).unwrap();
    create_packets(val).unwrap()
}

fn parse_packets(pair: &str) -> (Packet, Packet) {
    let packets = pair.lines().collect::<Vec<&str>>();
    ( parse_single_packet(packets[0]), parse_single_packet(packets[1]) )
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 13);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 13);
        assert_eq!(part_one(&input), Some(13));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 13);
        assert_eq!(part_two(&input), Some(140));
    }
}
