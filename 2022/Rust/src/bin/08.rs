use std::ops::Index;

pub fn part_one(input: &str) -> Option<u32> {
    let forest = to_forest(input);
    let w = forest.width;
    let h = forest.height;
    let mut n_visible = 0;
    for r in 0..w {
        for c in 0..h {
            if is_visible(&forest, r, c) {
                n_visible += 1;
            }
        }
    }
    Some(n_visible)
}

pub fn part_two(input: &str) -> Option<u32> {
    let forest = to_forest(input);
    let w = forest.width;
    let h = forest.height;
    let mut scenic_scores: Vec<usize> = vec![];

    for r in 0..w {
        for c in 0..h {
            let this_dist = view_dist(&forest, r, c);
            scenic_scores.push(this_dist) 
        }
    }
    let best = scenic_scores.iter().max().unwrap();
    Some(*best as u32)
}

#[derive(Debug, Copy, Clone)]
struct Tree {
    height: u32
}

fn is_visible(forest: &Forest, row: usize, col: usize) -> bool {
    let this_tree_height: u32 = forest[row][col].height;

    if row == forest.width || 
        col == forest.height || 
        row == 0 || 
        col == 0 {
        return true
    };

    fn test_forest(forest: &Forest, r: usize, c: usize, h: u32) -> bool {
        forest[r][c].height < h
    }

    if (0..row).all(|x| test_forest(forest, x, col, this_tree_height)) {
        return true 
    }

    if (row+1..forest.height).all(|x| test_forest(forest, x, col, this_tree_height)) {
        return true 
    }
    
    if (0..col).all(|x| test_forest(forest, row, x, this_tree_height))  {
        return true 
    }

    if (col+1..forest.width).all(|x| test_forest(forest, row, x, this_tree_height))  {
        return true 
    }

    false

}

fn view_dist(forest: &Forest, row: usize, col: usize) -> usize {
    let this_tree_height: u32 = forest[row][col].height;
    let w = forest.width;
    let h = forest.height;

    let view_left;
    if col == 0 {
        view_left = 0;
    } else {
        let mut dist = 0;
        for i in (0..col).rev() {
            dist += 1;
            if forest[row][i].height >= this_tree_height {
                break;
            } 
        }
        view_left = dist;
    }

    let view_right;
    if col == w {
        view_right = 0;
    } else {
        let mut dist = 0;
        for i in col+1..w {
            dist += 1;
            if forest[row][i].height >= this_tree_height {
                break;
            }
        }
        view_right = dist;
    }

    let view_up;
    if row == 0 {
        view_up = 0;
    } else {
        let mut dist = 0;
        for i in (0..row).rev() {
            dist += 1;
            if forest[i][col].height >= this_tree_height {
                break;
            }
        }
        view_up = dist;
    }

    let view_down;
    if row == h {
        view_down = 0;
    } else {
        let mut dist = 0;
        for i in row+1..h {
            dist += 1;
            if forest[i][col].height >= this_tree_height {
                break;
            }
        }
        view_down = dist;
    }

    view_left * view_right * view_up * view_down
}

#[derive(Debug)]
struct Forest {
    trees: Vec<Tree>,
    width: usize,
    height: usize
}

impl Index<usize> for Forest {
    type Output = [Tree];
    fn index(&self, index: usize) -> &Self::Output {
        &self.trees[index * self.width .. (index+1) * self.width]
    }
}


fn to_forest(input: &str) -> Forest {
    let lines = input.lines().collect::<Vec<&str>>();
    let width = lines[0].chars().count();
    let height = lines.len();
    let vals = input.replace("\n", "").chars()
        .map(|c| c.to_digit(10).unwrap())
        .collect::<Vec<u32>>();
    let trees = vals.iter().map(|v| Tree { height: *v }).collect();
    Forest { trees: trees, width: width, height: height }
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 8);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_one(&input), Some(21));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 8);
        assert_eq!(part_two(&input), Some(8));
    }
}
