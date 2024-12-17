#' Day 16: Reindeer Maze
#'
#' [Reindeer Maze](https://adventofcode.com/2024/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' It\'s time again for the [Reindeer Olympics](/2015/day/14)! This year,
#' the big event is the *Reindeer Maze*, where the Reindeer compete for the
#' *[lowest
#' score]{title="I would say it's like Reindeer Golf, but knowing Reindeer, it's almost certainly nothing like Reindeer Golf."}*.
#' 
#' You and The Historians arrive to search for the Chief right as the event
#' is about to start. It wouldn\'t hurt to watch a little, right?
#' 
#' The Reindeer start on the Start Tile (marked `S`) facing *East* and need
#' to reach the End Tile (marked `E`). They can move forward one tile at a
#' time (increasing their score by `1` point), but never into a wall (`#`).
#' They can also rotate clockwise or counterclockwise 90 degrees at a time
#' (increasing their score by `1000` points).
#' 
#' To figure out the best place to sit, you start by grabbing a map (your
#' puzzle input) from a nearby kiosk. For example:
#' 
#'     ###############
#'     #.......#....E#
#'     #.#.###.#.###.#
#'     #.....#.#...#.#
#'     #.###.#####.#.#
#'     #.#.#.......#.#
#'     #.#.#####.###.#
#'     #...........#.#
#'     ###.#.#####.#.#
#'     #...#.....#.#.#
#'     #.#.#.###.#.#.#
#'     #.....#...#.#.#
#'     #.###.#.#.#.#.#
#'     #S..#.....#...#
#'     ###############
#' 
#' There are many paths through this maze, but taking any of the best paths
#' would incur a score of only *`7036`*. This can be achieved by taking a
#' total of `36` steps forward and turning 90 degrees a total of `7` times:
#' 
#'     ###############
#'     #.......#....E#
#'     #.#.###.#.###^#
#'     #.....#.#...#^#
#'     #.###.#####.#^#
#'     #.#.#.......#^#
#'     #.#.#####.###^#
#'     #..>>>>>>>>v#^#
#'     ###^#.#####v#^#
#'     #>>^#.....#v#^#
#'     #^#.#.###.#v#^#
#'     #^....#...#v#^#
#'     #^###.#.#.#v#^#
#'     #S..#.....#>>^#
#'     ###############
#' 
#' Here\'s a second example:
#' 
#'     #################
#'     #...#...#...#..E#
#'     #.#.#.#.#.#.#.#.#
#'     #.#.#.#...#...#.#
#'     #.#.#.#.###.#.#.#
#'     #...#.#.#.....#.#
#'     #.#.#.#.#.#####.#
#'     #.#...#.#.#.....#
#'     #.#.#####.#.###.#
#'     #.#.#.......#...#
#'     #.#.###.#####.###
#'     #.#.#...#.....#.#
#'     #.#.#.#####.###.#
#'     #.#.#.........#.#
#'     #.#.#.#########.#
#'     #S#.............#
#'     #################
#' 
#' In this maze, the best paths cost *`11048`* points; following one such
#' path would look like this:
#' 
#'     #################
#'     #...#...#...#..E#
#'     #.#.#.#.#.#.#.#^#
#'     #.#.#.#...#...#^#
#'     #.#.#.#.###.#.#^#
#'     #>>v#.#.#.....#^#
#'     #^#v#.#.#.#####^#
#'     #^#v..#.#.#>>>>^#
#'     #^#v#####.#^###.#
#'     #^#v#..>>>>^#...#
#'     #^#v###^#####.###
#'     #^#v#>>^#.....#.#
#'     #^#v#^#####.###.#
#'     #^#v#^........#.#
#'     #^#v#^#########.#
#'     #S#>>^..........#
#'     #################
#' 
#' Note that the path shown above includes one 90 degree turn as the very
#' first move, rotating the Reindeer from facing East to facing North.
#' 
#' Analyze your map carefully. *What is the lowest score a Reindeer could
#' possibly get?*
#'
#' **Part Two**
#' 
#' Now that you know what the best paths look like, you can figure out the
#' best spot to sit.
#' 
#' Every non-wall tile (`S`, `.`, or `E`) is equipped with places to sit
#' along the edges of the tile. While determining which of these tiles
#' would be the best spot to sit depends on a whole bunch of factors (how
#' comfortable the seats are, how far away the bathrooms are, whether
#' there\'s a pillar blocking your view, etc.), the most important factor
#' is *whether the tile is on one of the best paths through the maze*. If
#' you sit somewhere else, you\'d miss all the action!
#' 
#' So, you\'ll need to determine which tiles are part of *any* best path
#' through the maze, including the `S` and `E` tiles.
#' 
#' In the first example, there are *`45`* tiles (marked `O`) that are part
#' of at least one of the various best paths through the maze:
#' 
#'     ###############
#'     #.......#....O#
#'     #.#.###.#.###O#
#'     #.....#.#...#O#
#'     #.###.#####.#O#
#'     #.#.#.......#O#
#'     #.#.#####.###O#
#'     #..OOOOOOOOO#O#
#'     ###O#O#####O#O#
#'     #OOO#O....#O#O#
#'     #O#O#O###.#O#O#
#'     #OOOOO#...#O#O#
#'     #O###.#.#.#O#O#
#'     #O..#.....#OOO#
#'     ###############
#' 
#' In the second example, there are *`64`* tiles that are part of at least
#' one of the best paths:
#' 
#'     #################
#'     #...#...#...#..O#
#'     #.#.#.#.#.#.#.#O#
#'     #.#.#.#...#...#O#
#'     #.#.#.#.###.#.#O#
#'     #OOO#.#.#.....#O#
#'     #O#O#.#.#.#####O#
#'     #O#O..#.#.#OOOOO#
#'     #O#O#####.#O###O#
#'     #O#O#..OOOOO#OOO#
#'     #O#O###O#####O###
#'     #O#O#OOO#..OOO#.#
#'     #O#O#O#####O###.#
#'     #O#O#OOOOOOO..#.#
#'     #O#O#O#########.#
#'     #O#OOO..........#
#'     #################
#' 
#' Analyze your map further. *How many tiles are part of at least one of
#' the best paths through the maze?*
#'
#' @param x some data
#' @return For Part One, `f16a(x)` returns .... For Part Two,
#'   `f16b(x)` returns ....
#' @export
#' @examples
#' f16a(example_data_16())
#' f16b()
f16a <- function(x) {
  x <- readLines("../tmp.txt")
  x <- readLines("../tmp2.txt")
  x <- readLines("inst/input16.txt")
  m <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], nrow = length(x), byrow = TRUE)
  
  start <- which(m == "S", arr.ind = TRUE)
  start <- complex(real = start[1], imaginary = start[2])
  h <- 0+1i
  res <- walkmaze(start, h)
  res[[1]]
  
}

get_el <- function(m, p) {
  m[Re(p), Im(p)]
}

walkmaze <- function(s, h, best_only = TRUE) {
 
  path <- c()
  queue <- list(list(s, h, 0, path))
  best_res <- Inf
  vp <- complex()
  vr <- integer()
  vh <- h
  all_paths <- list()
  path_score <- c()
  
  while (length(queue) > 0) {
    # try the best so far
    nextroute <- which.min(lapply(queue, \(z) z[[3]]))[1]
    x <- queue[nextroute][[1]]
    pos <- x[[1]]
    h <- x[[2]]
    res <- x[[3]]
    path <- x[[4]]
    queue <- queue[-nextroute]
    if (length(queue) == 0) queue <- list()
    this_el <- get_el(m, pos)
    if (this_el == "#") {
      next
    }
    if (this_el == "E") {
      all_paths[[length(all_paths) + 1]] <- c(path, pos)
      path_score <- c(path_score, res)
      best_res <- min(best_res, res)
      next
    }
    vp <- c(vp, pos)
    vr <- c(vr, res)
    vh <- c(vh, h)
    
    np <- pos + h
    if ((get_el(m, np) %in% c("E", "."))) { 
      if (!(np %in% vp) || all(h != vh[vp == np])) {#|| ) {
        if (!best_only || (!(np %in% vp) || (res + 1 <= min(vr[vp == np])))) {
          queue <- c(queue, list(list(pos + h, h, res + 1, c(path, pos))))  # straight
        }
      }
    }
    np <- pos + h*-1i
    if ((get_el(m, np) %in% c("E", "."))) { 
      if (!(np %in% vp) || all(h != vh[vp == np])) {
        if (!best_only || (!(np %in% vp) || (res + 1 <= min(vr[vp == np])))) {
          queue <- c(queue, list(list(pos + h*-1i, h*-1i, res + 1001, c(path, pos)))) # turn right
        }
      }
    }
    np <- pos + h*1i
    if ((get_el(m, np) %in% c("E", "."))) { 
      if (!(np %in% vp) || all(h != vh[vp == np])) {
        if (!best_only || (!(np %in% vp) || (res + 1 <= min(vr[vp == np])))) {
          queue <- c(queue, list(list(pos + h*1i, h*1i, res + 1001, c(path, pos))))   # turn left
        }
      }
    }
  }
  list(best_res, path_score, all_paths)
  
}

#' @rdname day16
#' @export
f16b <- function(x) {
  x <- readLines("../tmp.txt")
  x <- readLines("../tmp2.txt")
  x <- readLines("inst/input16.txt")
  m <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], nrow = length(x), byrow = TRUE)
  
  start <- which(m == "S", arr.ind = TRUE)
  start <- complex(real = start[1], imaginary = start[2])
  h <- 0+1i
  res <- walkmaze(start, h, best_only = FALSE)
  length(unique(unlist(res[[3]][(res[[2]] == min(res[[2]]))])))
}


f16_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day16
#' @export
example_data_16 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
