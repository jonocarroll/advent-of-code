#' Day 12: Hill Climbing Algorithm
#'
#' [Hill Climbing Algorithm](https://adventofcode.com/2022/day/12)
#'
#' @name day12
#' @rdname day12
#' @details
#'
#' **Part One**
#'
#' You try contacting the Elves using your [handheld
#' device]{title="When you look up the specs for your handheld device, every field just says \"plot\"."},
#' but the river you\'re following must be too low to get a decent signal.
#'
#' You ask the device for a heightmap of the surrounding area (your puzzle
#' input). The heightmap shows the local area from above broken into a
#' grid; the elevation of each square of the grid is given by a single
#' lowercase letter, where `a` is the lowest elevation, `b` is the
#' next-lowest, and so on up to the highest elevation, `z`.
#'
#' Also included on the heightmap are marks for your current position (`S`)
#' and the location that should get the best signal (`E`). Your current
#' position (`S`) has elevation `a`, and the location that should get the
#' best signal (`E`) has elevation `z`.
#'
#' You\'d like to reach `E`, but to save energy, you should do it in *as
#' few steps as possible*. During each step, you can move exactly one
#' square up, down, left, or right. To avoid needing to get out your
#' climbing gear, the elevation of the destination square can be *at most
#' one higher* than the elevation of your current square; that is, if your
#' current elevation is `m`, you could step to elevation `n`, but not to
#' elevation `o`. (This also means that the elevation of the destination
#' square can be much lower than the elevation of your current square.)
#'
#' For example:
#'
#'     Sabqponm
#'     abcryxxl
#'     accszExk
#'     acctuvwj
#'     abdefghi
#'
#' Here, you start in the top-left corner; your goal is near the middle.
#' You could start by moving down or right, but eventually you\'ll need to
#' head toward the `e` at the bottom. From there, you can spiral around to
#' the goal:
#'
#'     v..v<<<<
#'     >v.vv<<^
#'     .>vv>E^^
#'     ..v>>>^^
#'     ..>>>>>^
#'
#' In the above diagram, the symbols indicate whether the path exits each
#' square moving up (`^`), down (`v`), left (`<`), or right (`>`). The
#' location that should get the best signal is still `E`, and `.` marks
#' unvisited squares.
#'
#' This path reaches the goal in *`31`* steps, the fewest possible.
#'
#' *What is the fewest steps required to move from your current position to
#' the location that should get the best signal?*
#'
#' **Part Two**
#'
#' As you walk up the hill, you suspect that the Elves will want to turn
#' this into a hiking trail. The beginning isn\'t very scenic, though;
#' perhaps you can find a better starting point.
#'
#' To maximize exercise while hiking, the trail should start as low as
#' possible: elevation `a`. The goal is still the square marked `E`.
#' However, the trail should still be direct, taking the fewest steps to
#' reach its goal. So, you\'ll need to find the shortest path from *any
#' square at elevation `a`* to the square marked `E`.
#'
#' Again consider the example from above:
#'
#'     Sabqponm
#'     abcryxxl
#'     accszExk
#'     acctuvwj
#'     abdefghi
#'
#' Now, there are six choices for starting position (five marked `a`, plus
#' the square marked `S` that counts as being at elevation `a`). If you
#' start at the bottom-left square, you can reach the goal most quickly:
#'
#'     ...v<<<<
#'     ...vv<<^
#'     ...v>E^^
#'     .>v>>>^^
#'     >^>>>>>^
#'
#' This path reaches the goal in only *`29`* steps, the fewest possible.
#'
#' *What is the fewest steps required to move starting from any square with
#' elevation `a` to the location that should get the best signal?*
#'
#' @param x some data
#' @return For Part One, `f12a(x)` returns .... For Part Two,
#'   `f12b(x)` returns ....
#' @export
#' @examples
#' f12a(example_data_12())
#' f12b()
f12a <- function(x) {
  rows <- strsplit(x, "")
  grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
  ngrid <- grid
  ngrid[which(grid == "S", arr.ind = TRUE)] <- "a"
  ngrid[which(grid == "E", arr.ind = TRUE)] <- "z"
  ngrid[] <- match(ngrid[], letters)
  mode(ngrid) <- "integer"
  startat <- which(t(grid) == "S")
  endat <- which(t(grid) == "E")
  min_path <- dijkstra(ngrid, endat, dir = -1)
  min_pathp[startat]
}

#' @rdname day12
#' @export
f12b <- function(x) {
  rows <- strsplit(x, "")
  grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
  ngrid <- grid
  ngrid[which(grid == "S", arr.ind = TRUE)] <- "a"
  ngrid[which(grid == "E", arr.ind = TRUE)] <- "z"
  ngrid[] <- match(ngrid[], letters)
  mode(ngrid) <- "integer"

  startat <- which(t(grid) == "a")
  endat <- which(t(grid) == "E")
  min_path <- dijkstra(ngrid, endat, dir = -1)
  min(min_path[startat])
}

get_pos <- function(grid, v) {
  i <- floor((v-1)/ncol(grid))+1
  j <- ((v-1) %% ncol(grid))+1
  return(c(i, j))
}

can_reach <- function(ngrid, v, dir = 1) {
  x <- get_pos(ngrid, v)
  i <- x[1]
  j <- x[2]
  # can only move 1 row away
  res <- abs(floor(0:(prod(dim(ngrid))-1) / ncol(ngrid)) + 1 - i) <= 1 &
    # can only move 1 col away
    abs((0:(prod(dim(ngrid))-1)%%ncol(ngrid)) + 1 - j) <= 1 &
    # can't move diagonally
    abs(floor(0:(prod(dim(ngrid))-1) / ncol(ngrid)) + 1 - i) + abs((0:(prod(dim(ngrid))-1)%%ncol(ngrid)) + 1 - j) == 1
    if (dir == 1) {
    # can only step up 1
      res <- res & c(t(ngrid - ngrid[i, j] <= 1))
    } else {
      res <- res & c(t(ngrid[i, j] - ngrid <= 1))
    }
  as.integer(res)
}

dijkstra <- function(grid, start, dir = -1){
  #' Implementation of dijkstra using on-demand query
  #' derived from https://www.algorithms-and-technologies.com/dijkstra/r
  #' This returns an array containing the length of the shortest path from the start node to each other node.
  #' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
  #' This has a runtime of O(|V|^2) (|V| = number of Nodes), for a faster implementation see @see ../fast/Dijkstra.java (using adjacency lists)
  #' @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
  #' @param start the node to start from.
  #' @param dir are we going up or down? passed to can_reach()
  #' @return an array containing the shortest distances from the given start node to each other node

  # This contains the distances from the start node to all other nodes
  distances = rep(Inf, prod(dim(grid)))
  paths = rep(list(), prod(dim(grid)))

  # This contains whether a node was already visited
  visited = rep(FALSE, prod(dim(grid)))

  # The distance from the start node to itself is of course 0
  distances[start] = 0
  paths[[start]] = start

  # While there are nodes left to visit...
  repeat{

    # ... find the node with the currently shortest distance from the start node...
    shortest_distance = Inf
    shortest_index = -1
    for(i in seq_along(distances)) {
      # ... by going through all nodes that haven't been visited yet
      if(distances[i] < shortest_distance && !visited[i]){
        shortest_distance = distances[i]
        shortest_index = i
      }
    }

    # cat("Visiting node ", shortest_index, " with current distance ", shortest_distance, "\n")

    if(shortest_index == -1){
      # There was no node not yet visited --> We are done
      return (list(distances, paths))
    }
    # ...then, for all neighboring nodes that haven't been visited yet....
    # for(i in seq_along(graph[shortest_index,])) {
    g <- can_reach(grid, shortest_index, dir = dir)
    for(i in seq_along(g)) {
      # ...if the path over this edge is shorter...
      # if(graph[shortest_index,i] != 0 && distances[i] > distances[shortest_index] + graph[shortest_index,i]){
      if(g[i] != 0 && distances[i] > distances[shortest_index] + g[i]){
        # ...Save this path as new shortest path.
        distances[i] = distances[shortest_index] + g[i]
        paths[[i]] <- c(paths[[shortest_index]], i)
        # cat("Updating distance of node ", i, " to ", distances[i], "\n")
      }
      # Lastly, note that we are finished with this node.
      visited[shortest_index] = TRUE
      # cat("Visited nodes: ", visited, "\n")
      # cat("Currently lowest distances: ", distances, "\n")
    }
  }
}

plot_path <- function(x, scale = 6) {
  library(ggplot2)
  library(gganimate)

  rows <- strsplit(x, "")
  grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
  ngrid <- grid
  ngrid[which(grid == "S", arr.ind = TRUE)] <- "a"
  ngrid[which(grid == "E", arr.ind = TRUE)] <- "z"
  ngrid[] <- match(ngrid[], letters)
  mode(ngrid) <- "integer"
  startat <- which(t(grid) == "S")
  endat <- which(t(grid) == "E")
  min_path <- dijkstra(ngrid, endat, dir = -1)

  moves <- min_path[[2]][[startat]]
  movespos <- as.data.frame(t(sapply(rev(moves), \(x) get_pos(grid, x))))
  movespos$id <- seq_len(nrow(movespos))
  gridpos <- expand.grid(V1 = 1:nrow(grid), V2 = 1:ncol(grid))
  gridpos$val <- grid[as.matrix(gridpos)]
  gridpos$num <- match(gridpos$val, letters)
  gridpos[gridpos$val == "S", "num"] <- 1
  gridpos[gridpos$val == "E", "num"] <- 26
  p <- ggplot(gridpos, aes(V1, V2)) +
    geom_tile(aes(fill = num)) +
    # geom_text(aes(label = val)) +
    geom_path(data = as.data.frame(movespos), aes(V1, V2), col = "red", size = 2, inherit.aes = FALSE) +
    coord_flip() +
    scale_x_reverse() +
    scale_fill_viridis_c() +
    theme_void() +
    guides(fill = "none") +
    # theme(aspect.ratio = 1) +
    transition_reveal(id)
  animate(p, end_pause = 12, height = nrow(grid)*scale, width = ncol(grid)*scale)
  anim_save(filename = "inst/vis-day12.gif")
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day12
#' @export
example_data_12 <- function(example = 1) {
  l <- list(
    a = c(
          "Sabqponm",
          "abcryxxl",
          "accszExk",
          "acctuvwj",
          "abdefghi"
    )
  )
  l[[example]]
}
