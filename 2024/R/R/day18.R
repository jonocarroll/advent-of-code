#' Day 18: RAM Run
#'
#' [RAM Run](https://adventofcode.com/2024/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' You and The Historians look a lot more pixelated than you remember.
#' You\'re [inside a computer](/2017/day/2) at the North Pole!
#' 
#' Just as you\'re about to check out your surroundings, a program runs up
#' to you. \"This region of memory isn\'t safe! The User misunderstood what
#' a [pushdown
#' automaton](https://en.wikipedia.org/wiki/Pushdown_automaton){target="_blank"}
#' is and their algorithm is pushing whole *bytes* down on top of us!
#' [Run]{title="Pun intended."}!\"
#' 
#' The algorithm is fast - it\'s going to cause a byte to fall into your
#' memory space once every
#' [nanosecond](https://www.youtube.com/watch?v=9eyFDBPk4Yw){target="_blank"}!
#' Fortunately, you\'re *faster*, and by quickly scanning the algorithm,
#' you create a *list of which bytes will fall* (your puzzle input) in the
#' order they\'ll land in your memory space.
#' 
#' Your memory space is a two-dimensional grid with coordinates that range
#' from `0` to `70` both horizontally and vertically. However, for the sake
#' of example, suppose you\'re on a smaller grid with coordinates that
#' range from `0` to `6` and the following list of incoming byte positions:
#' 
#'     5,4
#'     4,2
#'     4,5
#'     3,0
#'     2,1
#'     6,3
#'     2,4
#'     1,5
#'     0,6
#'     3,3
#'     2,6
#'     5,1
#'     1,2
#'     5,5
#'     2,5
#'     6,5
#'     1,4
#'     0,4
#'     6,4
#'     1,1
#'     6,1
#'     1,0
#'     0,5
#'     1,6
#'     2,0
#' 
#' Each byte position is given as an `X,Y` coordinate, where `X` is the
#' distance from the left edge of your memory space and `Y` is the distance
#' from the top edge of your memory space.
#' 
#' You and The Historians are currently in the top left corner of the
#' memory space (at `0,0`) and need to reach the exit in the bottom right
#' corner (at `70,70` in your memory space, but at `6,6` in this example).
#' You\'ll need to simulate the falling bytes to plan out where it will be
#' safe to run; for now, simulate just the first few bytes falling into
#' your memory space.
#' 
#' As bytes fall into your memory space, they make that coordinate
#' *corrupted*. Corrupted memory coordinates cannot be entered by you or
#' The Historians, so you\'ll need to plan your route carefully. You also
#' cannot leave the boundaries of the memory space; your only hope is to
#' reach the exit.
#' 
#' In the above example, if you were to draw the memory space after the
#' first `12` bytes have fallen (using `.` for safe and `#` for corrupted),
#' it would look like this:
#' 
#'     ...#...
#'     ..#..#.
#'     ....#..
#'     ...#..#
#'     ..#..#.
#'     .#..#..
#'     #.#....
#' 
#' You can take steps up, down, left, or right. After just 12 bytes have
#' corrupted locations in your memory space, the shortest path from the top
#' left corner to the exit would take *`22`* steps. Here (marked with `O`)
#' is one such path:
#' 
#'     OO.#OOO
#'     .O#OO#O
#'     .OOO#OO
#'     ...#OO#
#'     ..#OO#.
#'     .#.O#..
#'     #.#OOOO
#' 
#' Simulate the first kilobyte (`1024` bytes) falling onto your memory
#' space. Afterward, *what is the minimum number of steps needed to reach
#' the exit?*
#'
#' **Part Two**
#' 
#' The Historians aren\'t as used to moving around in this pixelated
#' universe as you are. You\'re afraid they\'re not going to be fast enough
#' to make it to the exit before the path is completely blocked.
#' 
#' To determine how fast everyone needs to go, you need to determine *the
#' first byte that will cut off the path to the exit*.
#' 
#' In the above example, after the byte at `1,1` falls, there is still a
#' path to the exit:
#' 
#'     O..#OOO
#'     O##OO#O
#'     O#OO#OO
#'     OOO#OO#
#'     ###OO##
#'     .##O###
#'     #.#OOOO
#' 
#' However, after adding the very next byte (at `6,1`), there is no longer
#' a path to the exit:
#' 
#'     ...#...
#'     .##..##
#'     .#..#..
#'     ...#..#
#'     ###..##
#'     .##.###
#'     #.#....
#' 
#' So, in this example, the coordinates of the first byte that prevents the
#' exit from being reachable are *`6,1`*.
#' 
#' Simulate more of the bytes that are about to corrupt your memory space.
#' *What are the coordinates of the first byte that will prevent the exit
#' from being reachable from your starting position?* (Provide the answer
#' as two integers separated by a comma with no other characters.)
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns .... For Part Two,
#'   `f18b(x)` returns ....
#' @export
#' @examples
#' f18a(example_data_18())
#' f18b()
f18a <- function(x) {
  x <- readLines("inst/input18.txt")
  x <- x[1:1024]
  b <- strsplit(x, ",")
  x <- matrix(".", ncol = 71, nrow = 71)

  for (i in seq_along(b)) {
    x[as.integer(b[[i]][2]) + 1, as.integer(b[[i]][1]) + 1] <- "#"
  }

  adj <- adj_mat(x)
  library(igraph)
  g <- graph_from_adjacency_matrix(adj)
  sp <- shortest_paths(g, from = 1, to = length(c(x)))
  length(sp$vpath[[1]])-1
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


#' @rdname day18
#' @export
f18b <- function(x) {
  x <- readLines("inst/input18.txt")
  b <- strsplit(x, ",")

  lower <- 1024
  upper <- length(b)
  val <- floor((upper - lower) / 2)
  done <- FALSE
  critical <- 0
  
  while (!done) {
    message("trying ", val)
    l <- test_n_blocks(lower)
    v <- test_n_blocks(val)
    u <- test_n_blocks(upper)
    if (upper - val == 1 && v && !u) {
      critical <- upper
      break
    }
    
    if (!v) {
      newval <- val - floor((val - lower) / 2)
      upper <- val
      val <- newval
      next
    }
    
    if (v) {
      newval <- val + floor((upper - val) / 2)
      lower <- val
      val <- newval
      next
    }
    
  }
  
  critical
  paste(unlist(b[critical]), collapse = ",")
  
  # test_n_blocks(2861) #T
  # test_n_blocks(2862) #F
  # b[2862]

}


test_n_blocks <- function(n) {
  library(igraph)
  x <- matrix(".", ncol = 71, nrow = 71)
  for (i in 1:n) {
    x[as.integer(b[[i]][1]) + 1, as.integer(b[[i]][2]) + 1] <- "#"
  }
  adj <- adj_mat(x)
  g <- graph_from_adjacency_matrix(adj)
  sp <- suppressWarnings(shortest_paths(g, from = 1, to = length(c(x))))
  suppressWarnings(length(sp$vpath[[1]]) > 0)
}

f18_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
