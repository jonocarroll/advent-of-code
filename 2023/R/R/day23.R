#' Day 23: A Long Walk
#'
#' [A Long Walk](https://adventofcode.com/2023/day/23)
#'
#' @name day23
#' @rdname day23
#' @details
#'
#' **Part One**
#'
#' The Elves resume water filtering operations! Clean water starts flowing
#' over the edge of Island Island.
#' 
#' They offer to help *you* go over the edge of Island Island, too! Just
#' [hold on tight]{title="It'll be fiiiiiiiine."} to one end of this
#' impossibly long rope and they\'ll lower you down a safe distance from
#' the massive waterfall you just created.
#' 
#' As you finally reach Snow Island, you see that the water isn\'t really
#' reaching the ground: it\'s being *absorbed by the air* itself. It looks
#' like you\'ll finally have a little downtime while the moisture builds up
#' to snow-producing levels. Snow Island is pretty scenic, even without any
#' snow; why not take a walk?
#' 
#' There\'s a map of nearby hiking trails (your puzzle input) that
#' indicates *paths* (`.`), *forest* (`#`), and steep *slopes* (`^`, `>`,
#' `v`, and `<`).
#' 
#' For example:
#' 
#'     #.#####################
#'     #.......#########...###
#'     #######.#########.#.###
#'     ###.....#.>.>.###.#.###
#'     ###v#####.#v#.###.#.###
#'     ###.>...#.#.#.....#...#
#'     ###v###.#.#.#########.#
#'     ###...#.#.#.......#...#
#'     #####.#.#.#######.#.###
#'     #.....#.#.#.......#...#
#'     #.#####.#.#.#########v#
#'     #.#...#...#...###...>.#
#'     #.#.#v#######v###.###v#
#'     #...#.>.#...>.>.#.###.#
#'     #####v#.#.###v#.#.###.#
#'     #.....#...#...#.#.#...#
#'     #.#########.###.#.#.###
#'     #...###...#...#...#.###
#'     ###.###.#.###v#####v###
#'     #...#...#.#.>.>.#.>.###
#'     #.###.###.#.###.#.#v###
#'     #.....###...###...#...#
#'     #####################.#
#' 
#' You\'re currently on the single path tile in the top row; your goal is
#' to reach the single path tile in the bottom row. Because of all the mist
#' from the waterfall, the slopes are probably quite *icy*; if you step
#' onto a slope tile, your next step must be *downhill* (in the direction
#' the arrow is pointing). To make sure you have the most scenic hike
#' possible, *never step onto the same tile twice*. What is the longest
#' hike you can take?
#' 
#' In the example above, the longest hike you can take is marked with `O`,
#' and your starting position is marked `S`:
#' 
#'     #S#####################
#'     #OOOOOOO#########...###
#'     #######O#########.#.###
#'     ###OOOOO#OOO>.###.#.###
#'     ###O#####O#O#.###.#.###
#'     ###OOOOO#O#O#.....#...#
#'     ###v###O#O#O#########.#
#'     ###...#O#O#OOOOOOO#...#
#'     #####.#O#O#######O#.###
#'     #.....#O#O#OOOOOOO#...#
#'     #.#####O#O#O#########v#
#'     #.#...#OOO#OOO###OOOOO#
#'     #.#.#v#######O###O###O#
#'     #...#.>.#...>OOO#O###O#
#'     #####v#.#.###v#O#O###O#
#'     #.....#...#...#O#O#OOO#
#'     #.#########.###O#O#O###
#'     #...###...#...#OOO#O###
#'     ###.###.#.###v#####O###
#'     #...#...#.#.>.>.#.>O###
#'     #.###.###.#.###.#.#O###
#'     #.....###...###...#OOO#
#'     #####################O#
#' 
#' This hike contains *`94`* steps. (The other possible hikes you could
#' have taken were `90`, `86`, `82`, `82`, and `74` steps long.)
#' 
#' Find the longest hike you can take through the hiking trails listed on
#' your map. *How many steps long is the longest hike?*
#'
#' **Part Two**
#' 
#' As you reach the trailhead, you realize that the ground isn\'t as
#' slippery as you expected; you\'ll have *no problem* climbing up the
#' steep slopes.
#' 
#' Now, treat all *slopes* as if they were normal *paths* (`.`). You still
#' want to make sure you have the most scenic hike possible, so continue to
#' ensure that you *never step onto the same tile twice*. What is the
#' longest hike you can take?
#' 
#' In the example above, this increases the longest hike to *`154`* steps:
#' 
#'     #S#####################
#'     #OOOOOOO#########OOO###
#'     #######O#########O#O###
#'     ###OOOOO#.>OOO###O#O###
#'     ###O#####.#O#O###O#O###
#'     ###O>...#.#O#OOOOO#OOO#
#'     ###O###.#.#O#########O#
#'     ###OOO#.#.#OOOOOOO#OOO#
#'     #####O#.#.#######O#O###
#'     #OOOOO#.#.#OOOOOOO#OOO#
#'     #O#####.#.#O#########O#
#'     #O#OOO#...#OOO###...>O#
#'     #O#O#O#######O###.###O#
#'     #OOO#O>.#...>O>.#.###O#
#'     #####O#.#.###O#.#.###O#
#'     #OOOOO#...#OOO#.#.#OOO#
#'     #O#########O###.#.#O###
#'     #OOO###OOO#OOO#...#O###
#'     ###O###O#O###O#####O###
#'     #OOO#OOO#O#OOO>.#.>O###
#'     #O###O###O#O###.#.#O###
#'     #OOOOO###OOO###...#OOO#
#'     #####################O#
#' 
#' Find the longest hike you can take through the surprisingly dry hiking
#' trails listed on your map. *How many steps long is the longest hike?*
#'
#' @param x some data
#' @return For Part One, `f23a(x)` returns .... For Part Two,
#'   `f23b(x)` returns ....
#' @export
#' @examples
#' f23a(example_data_23())
#' f23b()
f23a <- function(x) {
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  start <- which(x[1, ] == ".")
  start <- 1 + (start-1)*nrow(x)
  end <- which(x[nrow(x), ] == ".")
  end <- end*nrow(x)
  adjacency_matrix <- can_reach(x)
  graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
  dists <- igraph::all_simple_paths(graph, start, end, mode = "out")
  max(sapply(dists, length) - 1)
}

get_pos <- function(grid, v) {
  i <- ((v-1) %% nrow(grid))+1
  j <- floor((v-1)/nrow(grid))+1
  return(c(i, j))
}

can_reach <- function(g) {
  nrows <- nrow(g)
  ncols <- ncol(g)
  adjacency_matrix <- matrix(0, nrow = nrows * ncols, ncol = nrows * ncols)
  
  for (v in 1:(nrows * ncols)) {
    x <- get_pos(g, v)
    i <- x[1]
    j <- x[2]
    
    for (w in 1:(nrows * ncols)) {
      y <- get_pos(g, w)
      row_diff <- y[1] - i
      col_diff <- y[2] - j
      
      # Check if the move is within the allowed range
      if (abs(row_diff) <= 1 && abs(col_diff) <= 1 && abs(row_diff) + abs(col_diff) == 1) {
        vchar <- c(g)[v]
        wchar <- c(g)[w]
        if (! wchar %in% c(".", "#", ">", "v")) stop("other char")
        if (vchar == ">" & wchar %in% c(".", ">", "v") & col_diff > 0) {
          adjacency_matrix[v, w] <- 1
        } else if (vchar == "v" & wchar %in% c(".", ">", "v") & row_diff > 0) {
          adjacency_matrix[v, w] <- 1
        } else if (vchar == ".") {
          adjacency_matrix[v, w] <- 1 & wchar != "#"
        }
      }
    }
  }
  
  return(adjacency_matrix)
}


#' @rdname day23
#' @export
f23b <- function(x) {
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  x[x != "#"] <- "."
  start <- which(x[1, ] == ".")
  start <- 1 + (start-1)*nrow(x)
  end <- which(x[nrow(x), ] == ".")
  end <- end*nrow(x)
  j <- junctions(x, start, end)
  x[t(sapply(j, \(w) get_pos(x, w)))] <- "J"
  adjacency_matrix <- get_weights(x, j)
  message("adj done")
  graph <- igraph::graph_from_adjacency_matrix(adjacency_matrix, mode = "directed", weighted = TRUE)
  message("graph done")
  dists <- igraph::all_simple_paths(graph, 1, nrow(adjacency_matrix), mode = "out")
  message("paths done")
  ans <- 0
  for (d in dists) {
    dd <- sum(E(graph, path = d)$weight)
    ans <- max(ans, dd)
  }
  ans
}

junctions <- function(g, start, end) {
  dir1 <- list(c(1,0),c(-1,0))
  dir2 <- list(c(0,1),c(0,-1))
  juncts <- vector("integer", length(g))
  for (v in 1:length(g)) {
    x <- get_pos(g, v)
    i <- x[1]
    j <- x[2]
    if (g[v] == "." & i > 0 & i < nrow(g) & j > 0 & j < ncol(g)) {
      juncts[v] <- sum(unlist(lapply(dir1, \(w) g[matrix(c(i, j) + w, ncol = 2)] == "."))) >= 1 & 
        sum(unlist(lapply(dir2, \(w) g[matrix(c(i, j) + w, ncol = 2)] == "."))) >= 1
    } else {
      juncts[v] <- FALSE
    }
  }
  jpos <- which(juncts == 1)
  jpos <- c(start, jpos, end)
  jpos
}

get_weights <- function(g, jpos) {
  alljpos <- sapply(jpos, \(w) get_pos(g, w))
  ajdf <- as.data.frame(t(alljpos))
  weight <- matrix(0, nrow = length(jpos), ncol = length(jpos))
  for (vv in 1:length(jpos)) {
    # x <- get_pos(g, jpos[vv])
    x <- alljpos[,vv]
    for (ww in 1:length(jpos)) {
      # y <- get_pos(g, jpos[ww])
      y <- alljpos[,ww]
      weight[vv, ww] <- n_along(g, x, y)
    }
  }
  
  weight
}

n_along <- function(g, a, b) {
  res <- 0
  if (a[1] == b[1]) {
    if (sum(g[a[1], a[2]:b[2]] == "J") >= 3) return(0)
    if (all(g[a[1], a[2]:b[2]] %in% c(".", "J"))) {
      res <- abs(b[2]-a[2])
    }
  } else if (a[2] == b[2]) {
    if (sum(g[a[1]:b[1], a[2]] == "J") >= 3) return(0)
    if (all(g[a[1]:b[1], a[2]] %in% c(".", "J"))) {
      res <- abs(b[1]-a[1])
    }
  } 
  res
}

f23_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day23
#' @export
example_data_23 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
