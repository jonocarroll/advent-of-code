pub fn part_one(input: &str) -> Option<u32> {
    let (ngrid, startpt, endpt) = parse12(input);

    let endpos = endpt.0 * ngrid[0].len() + endpt.1;
    let startpos = startpt.0 * ngrid[0].len() + startpt.1;
    let min_paths = dijkstra(ngrid, endpos, -1);

    Some(min_paths[startpos] as u32)
}

pub fn part_two(input: &str) -> Option<u32> {
    let (ngrid, _, endpt) = parse12(input);

    let endpos = endpt.0 * ngrid[0].len() + endpt.1;
    let allstarts: Vec<_> =  ngrid.clone()
    .into_iter()
    .flatten()
    .collect::<Vec<usize>>()
    .into_iter()
    .enumerate()
    .filter_map(|(i, x)| if x == 1 { Some(i) } else { None })
    .collect();

    let min_paths = dijkstra(ngrid, endpos, -1);

    let mut best_path = 1000;
    for i in allstarts {
        best_path = best_path.min(min_paths[i])
    }

    Some(best_path as u32)
}

fn can_reach(ngrid: &Vec<Vec<usize>>, pos: usize, dir: isize) -> Vec<usize> {
    let gridwidth = ngrid[0].len();
    let c = ((pos as usize) % gridwidth) as isize;
    let r = ((pos as usize) / gridwidth) as isize;
    let mut nearestpt: Vec<Vec<usize>> = vec![vec![0; ngrid[0].len()]; ngrid.len()];
    let mut nearby: bool;
    let mut offset: isize;
    for i in 0..nearestpt.len() as isize {
        for j in 0..nearestpt[0].len() as isize {
            nearby = ((r - i) as i32).abs() <= 1 && 
                        ((c - j) as i32).abs() <= 1 &&
                        ((r - i) as i32).abs() + ((c - j) as i32).abs() == 1;
            offset = (ngrid[i as usize][j as usize] as isize) - (ngrid[r as usize][c as usize] as isize);
            nearestpt[i as usize][j as usize] = (nearby && (dir * offset <= 1)) as usize;
        }
    }
    
    nearestpt.into_iter().flatten().collect::<Vec<usize>>()

}

fn dijkstra(ngrid: Vec<Vec<usize>>, startpos: usize, dir: isize) -> Vec<usize> {
    let gridsize = ngrid.len() * ngrid[0].len();
    let mut distances = vec![1e3 as usize; gridsize];
    let mut visited = vec![false; gridsize];
    let mut shortest_distance: usize;
    let mut shortest_index: isize;
    let mut g: Vec<usize>;
    
    distances[startpos] = 0;

    loop {
        shortest_distance = 1e3 as usize;
        shortest_index = -1;
        for i in 0..gridsize {
            if (distances[i] < shortest_distance) && !visited[i] {
                shortest_distance = distances[i];
                shortest_index = i as isize;
            }
        }
        if shortest_index == -1 {
            return distances
        }

        g = can_reach(&ngrid, shortest_index as usize, dir);

        for i in 0..g.len() {
            if g[i] != 0 && (distances[i] > (distances[shortest_index as usize] + g[i])) {
                distances[i] = distances[shortest_index as usize] + g[i];
            }
        }
        visited[shortest_index as usize] = true;
    }
}

fn parse12(input: &str) -> (Vec<Vec<usize>>, (usize, usize), (usize, usize)) {
    let mut grid = input.lines().map(|x| x.chars().collect::<Vec<char>>()).collect::<Vec<Vec<char>>>();
    let mut ngrid: Vec<Vec<usize>> = vec![vec![0; grid[0].len()]; grid.len()];
    let mut startpt = (0, 0);
    let mut endpt = (0, 0);
    for i in 0..grid.len() {
        for j in 0..grid[0].len() {
            if grid[i][j] == 'S' {
                startpt = (i, j);
                grid[i][j] = 'a';
            }
            if grid[i][j] == 'E' {
                endpt = (i, j);
                grid[i][j] = 'z';
            }
            ngrid[i][j] = grid[i][j] as usize - ('a' as usize) + 1
        }
    }
    (ngrid, startpt, endpt)
}

fn main() {
    let input = &advent_of_code::read_file("inputs", 12);
    advent_of_code::solve!(1, part_one, input);
    advent_of_code::solve!(2, part_two, input);
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_part_one() {
        let input = advent_of_code::read_file("examples", 12);
        assert_eq!(part_one(&input), Some(31));
    }

    #[test]
    fn test_part_two() {
        let input = advent_of_code::read_file("examples", 12);
        assert_eq!(part_two(&input), Some(29));
    }
}
