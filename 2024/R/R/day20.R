#' Day 20: Race Condition
#'
#' [Race Condition](https://adventofcode.com/2024/day/20)
#'
#' @name day20
#' @rdname day20
#' @details
#'
#' **Part One**
#'
#' The Historians are quite pixelated again. This time, a massive, black
#' building looms over you - you\'re [right outside](/2017/day/24) the CPU!
#' 
#' While The Historians get to work, a nearby program sees that you\'re
#' idle and challenges you to a *race*. Apparently, you\'ve arrived just in
#' time for the frequently-held *race condition* festival!
#' 
#' The race takes place on a particularly long and twisting code path;
#' programs compete to see who can finish in the *fewest picoseconds*. The
#' [winner]{title="If we give away enough mutexes, maybe someone will use one of them to fix the race condition!"}
#' even gets their very own
#' [mutex](https://en.wikipedia.org/wiki/Lock_(computer_science)){target="_blank"}!
#' 
#' They hand you a *map of the racetrack* (your puzzle input). For example:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #######.#.#.###
#'     #######.#.#...#
#'     #######.#.###.#
#'     ###..E#...#...#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' The map consists of track (`.`) - including the *start* (`S`) and *end*
#' (`E`) positions (both of which also count as track) - and *walls* (`#`).
#' 
#' When a program runs through the racetrack, it starts at the start
#' position. Then, it is allowed to move up, down, left, or right; each
#' such move takes *1 picosecond*. The goal is to reach the end position as
#' quickly as possible. In this example racetrack, the fastest time is `84`
#' picoseconds.
#' 
#' Because there is only a single path from the start to the end and the
#' programs all go the same speed, the races used to be pretty boring. To
#' make things more interesting, they introduced a new rule to the races:
#' programs are allowed to *cheat*.
#' 
#' The rules for cheating are very strict. *Exactly once* during a race, a
#' program may *disable collision* for up to *2 picoseconds*. This allows
#' the program to *pass through walls* as if they were regular track. At
#' the end of the cheat, the program must be back on normal track again;
#' otherwise, it will receive a [segmentation
#' fault](https://en.wikipedia.org/wiki/Segmentation_fault){target="_blank"}
#' and get disqualified.
#' 
#' So, a program could complete the course in 72 picoseconds (saving *12
#' picoseconds*) by cheating for the two moves marked `1` and `2`:
#' 
#'     ###############
#'     #...#...12....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #######.#.#.###
#'     #######.#.#...#
#'     #######.#.###.#
#'     ###..E#...#...#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' Or, a program could complete the course in 64 picoseconds (saving *20
#' picoseconds*) by cheating for the two moves marked `1` and `2`:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #######.#.#.###
#'     #######.#.#...#
#'     #######.#.###.#
#'     ###..E#...12..#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' This cheat saves *38 picoseconds*:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #######.#.#.###
#'     #######.#.#...#
#'     #######.#.###.#
#'     ###..E#...#...#
#'     ###.####1##.###
#'     #...###.2.#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' This cheat saves *64 picoseconds* and takes the program directly to the
#' end:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #######.#.#.###
#'     #######.#.#...#
#'     #######.#.###.#
#'     ###..21...#...#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' Each cheat has a distinct *start position* (the position where the cheat
#' is activated, just before the first move that is allowed to go through
#' walls) and *end position*; cheats are uniquely identified by their start
#' position and end position.
#' 
#' In this example, the total number of cheats (grouped by the amount of
#' time they save) are as follows:
#' 
#' -   There are 14 cheats that save 2 picoseconds.
#' -   There are 14 cheats that save 4 picoseconds.
#' -   There are 2 cheats that save 6 picoseconds.
#' -   There are 4 cheats that save 8 picoseconds.
#' -   There are 2 cheats that save 10 picoseconds.
#' -   There are 3 cheats that save 12 picoseconds.
#' -   There is one cheat that saves 20 picoseconds.
#' -   There is one cheat that saves 36 picoseconds.
#' -   There is one cheat that saves 38 picoseconds.
#' -   There is one cheat that saves 40 picoseconds.
#' -   There is one cheat that saves 64 picoseconds.
#' 
#' You aren\'t sure what the conditions of the racetrack will be like, so
#' to give yourself as many options as possible, you\'ll need a list of the
#' best cheats. *How many cheats would save you at least 100 picoseconds?*
#'
#' **Part Two**
#' 
#' The programs seem perplexed by your list of cheats. Apparently, the
#' two-picosecond cheating rule was deprecated several milliseconds ago!
#' The latest version of the cheating rule permits a single cheat that
#' instead lasts at most *20 picoseconds*.
#' 
#' Now, in addition to all the cheats that were possible in just two
#' picoseconds, many more cheats are possible. This six-picosecond cheat
#' saves *76 picoseconds*:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S#...#.#.#...#
#'     #1#####.#.#.###
#'     #2#####.#.#...#
#'     #3#####.#.###.#
#'     #456.E#...#...#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' Because this cheat has the same start and end positions as the one
#' above, it\'s the *same cheat*, even though the path taken during the
#' cheat is different:
#' 
#'     ###############
#'     #...#...#.....#
#'     #.#.#.#.#.###.#
#'     #S12..#.#.#...#
#'     ###3###.#.#.###
#'     ###4###.#.#...#
#'     ###5###.#.###.#
#'     ###6.E#...#...#
#'     ###.#######.###
#'     #...###...#...#
#'     #.#####.#.###.#
#'     #.#...#.#.#...#
#'     #.#.#.#.#.#.###
#'     #...#...#...###
#'     ###############
#' 
#' Cheats don\'t need to use all 20 picoseconds; cheats can last any amount
#' of time up to and including 20 picoseconds (but can still only end when
#' the program is on normal track). Any cheat time not used is lost; it
#' can\'t be saved for another cheat later.
#' 
#' You\'ll still need a list of the best cheats, but now there are even
#' more to choose between. Here are the quantities of cheats in this
#' example that save *50 picoseconds or more*:
#' 
#' -   There are 32 cheats that save 50 picoseconds.
#' -   There are 31 cheats that save 52 picoseconds.
#' -   There are 29 cheats that save 54 picoseconds.
#' -   There are 39 cheats that save 56 picoseconds.
#' -   There are 25 cheats that save 58 picoseconds.
#' -   There are 23 cheats that save 60 picoseconds.
#' -   There are 20 cheats that save 62 picoseconds.
#' -   There are 19 cheats that save 64 picoseconds.
#' -   There are 12 cheats that save 66 picoseconds.
#' -   There are 14 cheats that save 68 picoseconds.
#' -   There are 12 cheats that save 70 picoseconds.
#' -   There are 22 cheats that save 72 picoseconds.
#' -   There are 4 cheats that save 74 picoseconds.
#' -   There are 3 cheats that save 76 picoseconds.
#' 
#' Find the best cheats using the updated cheating rules. *How many cheats
#' would save you at least 100 picoseconds?*
#'
#' @param x some data
#' @return For Part One, `f20a(x)` returns .... For Part Two,
#'   `f20b(x)` returns ....
#' @export
#' @examples
#' f20a(example_data_20())
#' f20b()
f20a <- function(x) {
  x <- readLines("../tmp.txt")
  x <- readLines("inst/input20.txt")
  x <- matrix(strsplit(paste0(x, collapse = "")[[1]], "")[[1]], ncol = length(x), byrow = TRUE)

  start <- which(t(x) == "S")
  end <- which(t(x) == "E")
  
  adj <- adj_mat(x)
  library(igraph)
  g <- graph_from_adjacency_matrix(adj)
  sp <- shortest_paths(g, from = start, to = end)
  on_path <- sp$vpath[[1]]
  fastest <- length(on_path)-1
  
  xy <- function(x, v) {
    c(floor(v / nrow(x)) + 1, v %% ncol(x))
  }
  
  pathsteps <- cbind(t(sapply(on_path, \(z) xy(x, z))), seq_along(on_path) - 1)
  
  mdist <- function(z1, z2) {
    abs(z1[1] - z2[1]) + abs(z1[2] - z2[2])
  }
  
  part1 <- 0
  part2 <- 0
  for (i in seq_len(nrow(pathsteps))) {
    for (j in seq_len(nrow(pathsteps))) {
      if (j <= i) next 
      d <- mdist(pathsteps[i, c(1, 2)], pathsteps[j, c(1, 2)])
      if (d == 2 && pathsteps[j, 3] - (pathsteps[i, 3] + d) >= 100) {
        part1 <- part1 + 1
      }
      if (d <= 20 && pathsteps[j, 3] - (pathsteps[i, 3] + d) >= 100) {
        part2 <- part2 + 1
      }
    }
  }
  part1
  part2

}

adj_mat <- function(grid, step_size = 1, allow_diagonals = FALSE) {
  # Get the dimensions of the grid
  n_rows <- nrow(grid)
  n_cols <- ncol(grid)
  
  # Initialize the adjacency matrix
  adj_matrix <- matrix(0, nrow = n_rows * n_cols, ncol = n_rows * n_cols)
  
  # Define cardinal directions (up, down, left, right)
  directions <- list(
    up    = c(-step_size, 0),
    down  = c(step_size, 0),
    left  = c(0, -step_size),
    right = c(0, step_size)
  )
  
  # Add diagonals if allowed
  if (allow_diagonals) {
    diagonals <- list(
      up_left    = c(-step_size, -step_size),
      up_right   = c(-step_size, step_size),
      down_left  = c(step_size, -step_size),
      down_right = c(step_size, step_size)
    )
    directions <- c(directions, diagonals)
  }
  
  # Function to convert 2D grid indices to linear indices
  get_linear_index <- function(row, col) {
    return((row - 1) * n_cols + col)
  }
  
  # Iterate over each cell in the grid
  for (row in 1:n_rows) {
    for (col in 1:n_cols) {
      # Only consider reachable cells (not walls)
      if (grid[row, col] == "#") {
        next
      }
      
      # Check each direction
      for (direction in directions) {
        new_row <- row + direction[1]
        new_col <- col + direction[2]
        
        # Check if the new position is within bounds and reachable
        if (new_row >= 1 && new_row <= n_rows && new_col >= 1 && new_col <= n_cols && grid[new_row, new_col] != "#") {
          # Get linear indices for the current cell and the neighbor
          current_idx <- get_linear_index(row, col)
          neighbor_idx <- get_linear_index(new_row, new_col)
          
          # Set the corresponding positions in the adjacency matrix to 1 (connected)
          adj_matrix[current_idx, neighbor_idx] <- 1
          adj_matrix[neighbor_idx, current_idx] <- 1  # Since the graph is undirected
        }
      }
    }
  }
  
  return(adj_matrix)
}



#' @rdname day20
#' @export
f20b <- function(x) {

}


f20_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day20
#' @export
example_data_20 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
