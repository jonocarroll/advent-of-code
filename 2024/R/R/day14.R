#' Day 14: Restroom Redoubt
#'
#' [Restroom Redoubt](https://adventofcode.com/2024/day/14)
#'
#' @name day14
#' @rdname day14
#' @details
#'
#' **Part One**
#'
#' One of The Historians needs to use the bathroom; fortunately, you know
#' there\'s a bathroom near an unvisited location on their list, and so
#' you\'re all quickly teleported directly to the lobby of Easter Bunny
#' Headquarters.
#' 
#' Unfortunately, EBHQ seems to have \"improved\" bathroom security *again*
#' after your last [visit](/2016/day/2). The area outside the bathroom is
#' swarming with robots!
#' 
#' To get The Historian safely to the bathroom, you\'ll need a way to
#' predict where the robots will be in the future. Fortunately, they all
#' seem to be moving on the tile floor in predictable *straight lines*.
#' 
#' You make a list (your puzzle input) of all of the robots\' current
#' *positions* (`p`) and *velocities* (`v`), one robot per line. For
#' example:
#' 
#'     p=0,4 v=3,-3
#'     p=6,3 v=-1,-3
#'     p=10,3 v=-1,2
#'     p=2,0 v=2,-1
#'     p=0,0 v=1,3
#'     p=3,0 v=-2,-2
#'     p=7,6 v=-1,-3
#'     p=3,0 v=-1,-2
#'     p=9,3 v=2,3
#'     p=7,3 v=-1,2
#'     p=2,4 v=2,-3
#'     p=9,5 v=-3,-3
#' 
#' Each robot\'s position is given as `p=x,y` where `x` represents the
#' number of tiles the robot is from the left wall and `y` represents the
#' number of tiles from the top wall (when viewed from above). So, a
#' position of `p=0,0` means the robot is all the way in the top-left
#' corner.
#' 
#' Each robot\'s velocity is given as `v=x,y` where `x` and `y` are given
#' in *tiles per second*. Positive `x` means the robot is moving to the
#' *right*, and positive `y` means the robot is moving *down*. So, a
#' velocity of `v=1,-2` means that each second, the robot moves `1` tile to
#' the right and `2` tiles up.
#' 
#' The robots outside the actual bathroom are in a space which is `101`
#' tiles wide and `103` tiles tall (when viewed from above). However, in
#' this example, the robots are in a space which is only `11` tiles wide
#' and `7` tiles tall.
#' 
#' The robots are good at navigating over/under each other (due to a
#' combination of springs, extendable legs, and quadcopters), so they can
#' share the same tile and don\'t interact with each other. Visually, the
#' number of robots on each tile in this example looks like this:
#' 
#'     1.12.......
#'     ...........
#'     ...........
#'     ......11.11
#'     1.1........
#'     .........1.
#'     .......1...
#' 
#' These robots have a unique feature for maximum bathroom security: they
#' can *teleport*. When a robot would run into an edge of the space
#' they\'re in, they instead *teleport to the other side*, effectively
#' wrapping around the edges. Here is what robot `p=2,4 v=2,-3` does for
#' the first few seconds:
#' 
#'     Initial state:
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ..1........
#'     ...........
#'     ...........
#' 
#'     After 1 second:
#'     ...........
#'     ....1......
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#' 
#'     After 2 seconds:
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ......1....
#'     ...........
#' 
#'     After 3 seconds:
#'     ...........
#'     ...........
#'     ........1..
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#' 
#'     After 4 seconds:
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ...........
#'     ..........1
#' 
#'     After 5 seconds:
#'     ...........
#'     ...........
#'     ...........
#'     .1.........
#'     ...........
#'     ...........
#'     ...........
#' 
#' The Historian can\'t wait much longer, so you don\'t have to simulate
#' the robots for very long. Where will the robots be after `100` seconds?
#' 
#' In the above example, the number of robots on each tile after 100
#' seconds has elapsed looks like this:
#' 
#'     ......2..1.
#'     ...........
#'     1..........
#'     .11........
#'     .....1.....
#'     ...12......
#'     .1....1....
#' 
#' To determine the safest area, count the *number of robots in each
#' quadrant* after 100 seconds. Robots that are exactly in the middle
#' (horizontally or vertically) don\'t count as being in any quadrant, so
#' the only relevant robots are:
#' 
#'     ..... 2..1.
#'     ..... .....
#'     1.... .....
#'                
#'     ..... .....
#'     ...12 .....
#'     .1... 1....
#' 
#' In this example, the quadrants contain `1`, `3`, `4`, and `1` robot.
#' Multiplying these together gives a total *safety factor* of *`12`*.
#' 
#' Predict the motion of the robots in your list within a space which is
#' `101` tiles wide and `103` tiles tall. *What will the safety factor be
#' after exactly 100 seconds have elapsed?*
#'
#' **Part Two**
#' 
#' During the bathroom break, someone notices that these robots seem
#' awfully similar to ones built and used at the North Pole. If they\'re
#' the same type of robots, they should have a hard-coded [Easter
#' egg]{title="This puzzle was originally going to be about the motion of space rocks in a fictitious arcade game called Meteoroids, but we just had an arcade puzzle."}:
#' very rarely, most of the robots should arrange themselves into *a
#' picture of a Christmas tree*.
#' 
#' *What is the fewest number of seconds that must elapse for the robots to
#' display the Easter egg?*
#'
#' @param x some data
#' @return For Part One, `f14a(x)` returns .... For Part Two,
#'   `f14b(x)` returns ....
#' @export
#' @examples
#' f14a(example_data_14())
#' f14b()
f14a <- function(x) {
  d <- readLines("../tmp.txt")
  d <- readLines("inst/input14.txt")
  dd <- lapply(d, parse_line)
  
  # abusing scoping
  NC <- 101
  NR <- 103
  HC1 <- 1:floor(NC/2)-1
  HC2 <- (((NC+1)/2)+1):NC-1
  HR1 <- 1:floor(NR/2)-1
  HR2 <- (((NR+1)/2)+1):NR-1

  for (i in 1:100) {
    for (di in 1:length(dd)) {
      dd[[di]] <- step(dd[[di]])
    }
  }
  
  score(dd)
  
}

plotgrid <- function(robots, num = FALSE) {
  grid <- matrix(0, nrow = NC, ncol = NR)
  for (di in 1:length(robots)) {
    pos <- robots[[di]][[1]] + 1
    grid[pos[1], pos[2]] <- grid[pos[1], pos[2]] + 1 
  }
  if (!num) {
    grid[grid != 0] <- "#"
    grid[grid == 0] <- " "
  }
  cat(paste(apply(t(grid), 1, paste, collapse = ""), collapse = "\n"))
}

score <- function(robots, fun = `*`) {
  pos <- lapply(robots, \(x) x[[1]])
  q1 <- sum(unlist(lapply(pos, \(x) x[1] %in% HC1 & x[2] %in% HR1)))
  q2 <- sum(unlist(lapply(pos, \(x) x[1] %in% HC1 & x[2] %in% HR2)))
  q3 <- sum(unlist(lapply(pos, \(x) x[1] %in% HC2 & x[2] %in% HR1)))
  q4 <- sum(unlist(lapply(pos, \(x) x[1] %in% HC2 & x[2] %in% HR2)))
  prod(c(q1, q2, q3, q4))
}

step <- function(r) {
  pos <- r[[1]]
  vel <- r[[2]]
  pos <- (pos + vel) 
  while (pos[1] < 0) { pos[1] <- pos[1] + NC}
  while (pos[2] < 0) { pos[2] <- pos[2] + NR}
  while (pos[1] > NC - 1) { pos[1] <- pos[1] - NC}
  while (pos[2] > NR - 1) { pos[2] <- pos[2] - NR}
  #%% c(NC+1, NR+1)
  list(pos, vel)
}

parse_line <- function(x) {
  lapply(strsplit(gsub('[pv=)(]', "", (strsplit(x, " ")[[1]])), ","), as.integer)
}


#' @rdname day14
#' @export
f14b <- function(x) {

  d <- readLines("../tmp.txt")
  d <- readLines("inst/input14.txt")
  dd <- lapply(d, parse_line)
  
  NC <- 101
  NR <- 103
  
  # just focus on the centre of the image; assume it's filled
  HC1 <- 25:50
  HC2 <- 50:75
  HR1 <- 25:50
  HR2 <- 50:75

  highest_score <- 0
  at_time <- c()
  for (ti in 1:10000) { # insert largest at_time values here
    for (di in 1:length(dd)) {
      dd[[di]] <- step(dd[[di]])
    }
    this_score <- score(dd)
    if (this_score >= highest_score) {
      message("new score: ", this_score, ", it: ", ti)
      highest_score <- this_score
      at_time <- c(at_time, ti)
    }
  }
}


f14_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day14
#' @export
example_data_14 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
