
pub fn part_one(input: &str) -> Option<u32> {
    let rocks = input.lines().map(|l| l.split(" -> ").collect::<Vec<&str>>()).collect::<Vec<Vec<&str>>>();
    let mut cave = vec![vec!["."; 200]; 1500];
    for v in rocks {
        // take pairs
        for r in v.windows(2) {
            fill_rocks(r[0], r[1], &mut cave);
        }
    }

    // drop sand
    let mut is_done = false;
    while !is_done {
        fall((0,0), &mut cave)
    }

    for i in 0..=10 {
        let mut tmp = vec![""];
        for j in 493..504 {
            tmp.push(cave[j][i])
        }
        println!("{:?}", tmp.join(""));
    }

    None
}

fn fall(sand: (usize, usize), cave: &mut Vec<Vec<&str>>) {
    let mut down = (sand.0, sand.1 + 1);
}

fn fill_rocks(rocks1: &str, rocks2: &str, cave: &mut Vec<Vec<&str>>) {
    
    let rock_coords_1 = rocks1.split(",").map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();
    let rock_coords_2 = rocks2.split(",").map(|x| x.parse::<usize>().unwrap()).collect::<Vec<usize>>();
    if rock_coords_1[0] == rock_coords_2[0] {
        for i in rock_coords_1[1]..=rock_coords_2[1] {
            cave[rock_coords_1[0]][i] = "#";
        } 
        // do the reverse as well since Range can't go backwards
        for i in rock_coords_2[1]..=rock_coords_1[1] {
            cave[rock_coords_1[0]][i] = "#";
        } 
    } else {
        for i in rock_coords_1[0]..=rock_coords_2[0] {
            cave[i][rock_coords_1[1]] = "#";
        }
        // do the reverse as well since Range can't go backwards
        for i in rock_coords_2[0]..=rock_coords_1[0] {
            cave[i][rock_coords_1[1]] = "#";
        }
    }

    // for i in 0..=10 {
    //     let mut tmp = vec![""];
    //     for j in 493..504 {
    //         tmp.push(cave[j][i])
    //     }
    //     println!("{:?}", tmp.join(""));
    // }
    // cave is updated as a side-effect
}

pub fn part_two(_input: &str) -> Option<u32> {
    None
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 14);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 14);
        assert_eq!(part_one(&input), Some(24));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 14);
        assert_eq!(part_two(&input), Some(93));
    }
}
