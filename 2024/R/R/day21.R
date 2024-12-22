#' Day 21: Keypad Conundrum
#'
#' [Keypad Conundrum](https://adventofcode.com/2024/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' As you teleport onto Santa\'s [Reindeer-class starship](/2019/day/25),
#' The Historians begin to panic: someone from their search party is
#' *missing*. A quick life-form scan by the ship\'s computer reveals that
#' when the missing Historian teleported, he arrived in another part of the
#' ship.
#' 
#' The door to that area is locked, but the computer can\'t open it; it can
#' only be opened by *physically typing* the door codes (your puzzle input)
#' on the numeric keypad on the door.
#' 
#' The numeric keypad has four rows of buttons: `789`, `456`, `123`, and
#' finally an empty gap followed by `0A`. Visually, they are arranged like
#' this:
#' 
#'     +---+---+---+
#'     | 7 | 8 | 9 |
#'     +---+---+---+
#'     | 4 | 5 | 6 |
#'     +---+---+---+
#'     | 1 | 2 | 3 |
#'     +---+---+---+
#'         | 0 | A |
#'         +---+---+
#' 
#' Unfortunately, the area outside the door is currently *depressurized*
#' and nobody can go near the door. A robot needs to be sent instead.
#' 
#' The robot has no problem navigating the ship and finding the numeric
#' keypad, but it\'s not designed for button pushing: it can\'t be told to
#' push a specific button directly. Instead, it has a robotic arm that can
#' be controlled remotely via a *directional keypad*.
#' 
#' The directional keypad has two rows of buttons: a gap / `^` (up) / `A`
#' (activate) on the first row and `<` (left) / `v` (down) / `>` (right) on
#' the second row. Visually, they are arranged like this:
#' 
#'         +---+---+
#'         | ^ | A |
#'     +---+---+---+
#'     | < | v | > |
#'     +---+---+---+
#' 
#' When the robot arrives at the numeric keypad, its robotic arm is pointed
#' at the `A` button in the bottom right corner. After that, this
#' directional keypad remote control must be used to maneuver the robotic
#' arm: the up / down / left / right buttons cause it to move its arm one
#' button in that direction, and the `A` button causes the robot to briefly
#' move forward, pressing the button being aimed at by the robotic arm.
#' 
#' For example, to make the robot type `029A` on the numeric keypad, one
#' sequence of inputs on the directional keypad you could use is:
#' 
#' -   `<` to move the arm from `A` (its initial position) to `0`.
#' -   `A` to push the `0` button.
#' -   `^A` to move the arm to the `2` button and push it.
#' -   `>^^A` to move the arm to the `9` button and push it.
#' -   `vvvA` to move the arm to the `A` button and push it.
#' 
#' In total, there are three shortest possible sequences of button presses
#' on this directional keypad that would cause the robot to type `029A`:
#' `<A^A>^^AvvvA`, `<A^A^>^AvvvA`, and `<A^A^^>AvvvA`.
#' 
#' Unfortunately, the area containing this directional keypad remote
#' control is currently experiencing *high levels of radiation* and nobody
#' can go near it. A robot needs to be sent instead.
#' 
#' When the robot arrives at the directional keypad, its robot arm is
#' pointed at the `A` button in the upper right corner. After that, a
#' *second, different* directional keypad remote control is used to control
#' this robot (in the same way as the first robot, except that this one is
#' typing on a directional keypad instead of a numeric keypad).
#' 
#' There are multiple shortest possible sequences of directional keypad
#' button presses that would cause this robot to tell the first robot to
#' type `029A` on the door. One such sequence is
#' `v<<A>>^A<A>AvA<^AA>A<vAAA>^A`.
#' 
#' Unfortunately, the area containing this second directional keypad remote
#' control is currently *`-40` degrees*! Another robot will need to be sent
#' to type on that directional keypad, too.
#' 
#' There are many shortest possible sequences of directional keypad button
#' presses that would cause this robot to tell the second robot to tell the
#' first robot to eventually type `029A` on the door. One such sequence is
#' `<vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A`.
#' 
#' Unfortunately, the area containing this third directional keypad remote
#' control is currently *full of Historians*, so no robots can find a clear
#' path there. Instead, *you* will have to type this sequence yourself.
#' 
#' Were you to choose this sequence of button presses, here are all of the
#' buttons that would be pressed on your directional keypad, the two
#' robots\' directional keypads, and the numeric keypad:
#' 
#'     <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
#'     v<<A>>^A<A>AvA<^AA>A<vAAA>^A
#'     <A^A>^^AvvvA
#'     029A
#' 
#' In summary, there are the following keypads:
#' 
#' -   One directional keypad that *you* are using.
#' -   Two directional keypads that *robots* are using.
#' -   One numeric keypad (on a door) that a *robot* is using.
#' 
#' It is important to remember that these robots are not designed for
#' button pushing. In particular, if a robot arm is ever aimed at a *gap*
#' where no button is present on the keypad, even for an instant, the robot
#' will *panic* unrecoverably. So, don\'t do that. All robots will
#' initially aim at the keypad\'s `A` key, wherever it is.
#' 
#' To unlock the door, *five* codes will need to be typed on its numeric
#' keypad. For example:
#' 
#'     029A
#'     980A
#'     179A
#'     456A
#'     379A
#' 
#' For each of these, here is a shortest sequence of button presses you
#' could type to cause the desired code to be typed on the numeric keypad:
#' 
#'     029A: <vA<AA>>^AvAA<^A>A<v<A>>^AvA^A<vA>^A<v<A>^A>AAvA^A<v<A>A>^AAAvA<^A>A
#'     980A: <v<A>>^AAAvA^A<vA<AA>>^AvAA<^A>A<v<A>A>^AAAvA<^A>A<vA>^A<A>A
#'     179A: <v<A>>^A<vA<A>>^AAvAA<^A>A<v<A>>^AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
#'     456A: <v<A>>^AA<vA<A>>^AAvAA<^A>A<vA>^A<A>A<vA>^A<A>A<v<A>A>^AAvA<^A>A
#'     379A: <v<A>>^AvA^A<vA<AA>>^AAvA<^A>AAvA^A<vA>^AA<A>A<v<A>A>^AAAvA<^A>A
#' 
#' The Historians are getting nervous; the ship computer doesn\'t remember
#' whether the missing Historian is trapped in the area containing a *giant
#' electromagnet* or *molten lava*. You\'ll need to make sure that for each
#' of the five codes, you find the *shortest sequence* of button presses
#' necessary.
#' 
#' The *complexity* of a single code (like `029A`) is equal to the result
#' of multiplying these two values:
#' 
#' -   The *length of the shortest sequence* of button presses you need to
#'     type on your directional keypad in order to cause the code to be
#'     typed on the numeric keypad; for `029A`, this would be `68`.
#' -   The *numeric part of the code* (ignoring leading zeroes); for
#'     `029A`, this would be `29`.
#' 
#' In the above example, complexity of the five codes can be found by
#' calculating `68 * 29`, `60 * 980`, `68 * 179`, `64 * 456`, and
#' `64 * 379`. Adding these together produces *`126384`*.
#' 
#' Find the fewest number of button presses you\'ll need to perform in
#' order to cause the robot in front of the door to type each code. *What
#' is the sum of the complexities of the five codes on your list?*
#'
#' **Part Two**
#' 
#' Just as the missing Historian is released, The Historians realize that a
#' *second* member of their search party has also been missing [this entire
#' time]{title="bum bum BUUUUUM"}!
#' 
#' A quick life-form scan reveals the Historian is also trapped in a locked
#' area of the ship. Due to a variety of hazards, robots are once again
#' dispatched, forming another chain of remote control keypads managing
#' robotic-arm-wielding robots.
#' 
#' This time, many more robots are involved. In summary, there are the
#' following keypads:
#' 
#' -   One directional keypad that *you* are using.
#' -   *25* directional keypads that *robots* are using.
#' -   One numeric keypad (on a door) that a *robot* is using.
#' 
#' The keypads form a chain, just like before: your directional keypad
#' controls a robot which is typing on a directional keypad which controls
#' a robot which is typing on a directional keypad\... and so on, ending
#' with the robot which is typing on the numeric keypad.
#' 
#' The door codes are the same this time around; only the number of robots
#' and directional keypads has changed.
#' 
#' Find the fewest number of button presses you\'ll need to perform in
#' order to cause the robot in front of the door to type each code. *What
#' is the sum of the complexities of the five codes on your list?*
#'
#' @param x some data
#' @return For Part One, `f21a(x)` returns .... For Part Two,
#'   `f21b(x)` returns ....
#' @export
#' @examples
#' f21a(example_data_21())
#' f21b()
f21a <- function(x) {
  # x <- readLines("../tmp2.txt")
  x <- readLines("inst/input21.txt")
  
  codes <- strsplit(x, "")
  
  buttons <- c(7:9, 4:6, 1:3, "#", "0", "A")
  numpad <- matrix(buttons, nrow = 4, ncol = 3, byrow = TRUE)

  keys <- c("#", "^", "A", "<", "v", ">")
  keypad <- matrix(keys, nrow = 2, byrow = TRUE)

  pos_to_code <- function(depth, x) {
    vals <- if (depth == N_KEYPADS) buttons else keys
    vals[x]
  }
  
  code_to_pos <- function(depth, x) {
    vals <- if (depth == N_KEYPADS) buttons else keys
    match(x, vals)
  }
  
  #'     +---+---+---+
  #'     | 7 | 8 | 9 |
  #'     +---+---+---+
  #'     | 4 | 5 | 6 |
  #'     +---+---+---+
  #'     | 1 | 2 | 3 |
  #'     +---+---+---+
  #'         | 0 | A |
  #'         +---+---+
  np_dirs <- function(a, b) {
    if (a == 1 && b == 2) return(">")
    if (a == 1 && b == 4) return("v")
    if (a == 2 && b == 1) return("<")
    if (a == 2 && b == 5) return("v")
    if (a == 2 && b == 3) return(">")
    if (a == 3 && b == 2) return("<")
    if (a == 3 && b == 6) return("v")
    if (a == 4 && b == 1) return("^")
    if (a == 4 && b == 5) return(">")
    if (a == 4 && b == 7) return("v")
    if (a == 5 && b == 2) return("^")
    if (a == 5 && b == 4) return("<")
    if (a == 5 && b == 8) return("v")
    if (a == 5 && b == 6) return(">")
    if (a == 6 && b == 3) return("^")
    if (a == 6 && b == 5) return("<")
    if (a == 6 && b == 9) return("v")
    if (a == 7 && b == 8) return(">")
    if (a == 7 && b == 4) return("^")
    if (a == 8 && b == 5) return("^")
    if (a == 8 && b == 9) return(">")
    if (a == 8 && b == 7) return("<")
    if (a == 8 && b == 11) return("v")
    if (a == 9 && b == 6) return("^")
    if (a == 9 && b == 8) return("<")
    if (a == 9 && b == 12) return("v")
    if (a == 11 && b == 8) return("^")
    if (a == 11 && b == 12) return(">")
    if (a == 12 && b == 11) return("<")
    if (a == 12 && b == 9) return("^")
    stop("Are you sure?")
  }

  #'         +---+---+
  #'         | ^ | A |
  #'     +---+---+---+
  #'     | < | v | > |
  #'     +---+---+---+
  kp_dirs <- function(a, b) {
    if (a == 2 && b == 3) return(">")
    if (a == 2 && b == 5) return("v")
    if (a == 3 && b == 2) return("<")
    if (a == 3 && b == 6) return("v")
    if (a == 4 && b == 5) return(">")
    if (a == 5 && b == 2) return("^")
    if (a == 5 && b == 4) return("<")
    if (a == 5 && b == 6) return(">")
    if (a == 6 && b == 5) return("<")
    if (a == 6 && b == 3) return("^")
    stop("Really sure?")
  }
  
  adj_np <- adj_mat(numpad)
  adj_kp <- adj_mat(keypad)
  
  library(igraph)
  
  g_np <- graph_from_adjacency_matrix(adj_np)
  g_kp <- graph_from_adjacency_matrix(adj_kp)

  to_from <- memoise::memoise(function(depth, a, b) {
    g <- if (depth == N_KEYPADS) g_np else g_kp
    dirs <- if (depth == N_KEYPADS) np_dirs else kp_dirs
    sp <- all_shortest_paths(g, from = code_to_pos(depth, a), to = code_to_pos(depth, b))
    # translate nodes back to buttons/keys
    lapply(
      unclass(sp$vpaths), 
      \(p) purrr::pmap(list(head(p, -1), tail(p, -1)), dirs)
    )
  })
  
  min_presses <- memoise::memoise(function(depth, a, b) {
    if (depth == 0) return(1)
    min(
      sapply(to_from(depth, a, b), 
             \(newcode) run_code(depth - 1, c(newcode, "A"))
      )
    )
  })
  
  run_code <- function(depth, code) {
    code <- c("A", code)
    sum(
      purrr::pmap_dbl(
        list(
          head(code, -1), 
          tail(code, -1)), 
        \(.x, .y) min_presses(depth, .x, .y)
      )
    )
  }
  
  # Part 1
  N_KEYPADS <- 3
  tot <- 0
  for (i in seq_along(codes)) {
    num_part <- as.integer(paste0(head(codes[[i]], -1), collapse = ""))
    complexity <- num_part * run_code(N_KEYPADS, codes[[i]])
    tot <- tot + complexity
  }
  tot
  
  # Part 2
  N_KEYPADS <- 26
  tot <- 0
  for (i in seq_along(codes)) {
    num_part <- as.integer(paste0(head(codes[[i]], -1), collapse = ""))
    complexity <- num_part * run_code(N_KEYPADS, codes[[i]])
    tot <- tot + complexity
  }
  print(tot, digits = 22)

}


# this version fills by row
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
    # return((col - 1) * n_rows + row)
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


#' @rdname day21
#' @export
f21b <- function(x) {

}


f21_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day21
#' @export
example_data_21 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
