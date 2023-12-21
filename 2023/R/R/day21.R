#' Day 21: Step Counter
#'
#' [Step Counter](https://adventofcode.com/2023/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' You manage to catch the [airship](7) right as it\'s dropping someone
#' else off on their all-expenses-paid trip to Desert Island! It even
#' helpfully drops you off near the [gardener](5) and his massive farm.
#' 
#' \"You got the sand flowing again! Great work! Now we just need to wait
#' until we have enough sand to filter the water for Snow Island and we\'ll
#' have snow again in no time.\"
#' 
#' While you wait, one of the Elves that works with the gardener heard how
#' good you are at solving problems and would like your help. He needs to
#' get his
#' [steps](https://en.wikipedia.org/wiki/Pedometer){target="_blank"} in for
#' the day, and so he\'d like to know *which garden plots he can reach with
#' exactly his remaining `64` steps*.
#' 
#' He gives you an up-to-date map (your puzzle input) of his starting
#' position (`S`), garden plots (`.`), and rocks (`#`). For example:
#' 
#'     ...........
#'     .....###.#.
#'     .###.##..#.
#'     ..#.#...#..
#'     ....#.#....
#'     .##..S####.
#'     .##..#...#.
#'     .......##..
#'     .##.#.####.
#'     .##..##.##.
#'     ...........
#' 
#' The Elf starts at the starting position (`S`) which also counts as a
#' garden plot. Then, he can take one step north, south, east, or west, but
#' only onto tiles that are garden plots. This would allow him to reach any
#' of the tiles marked `O`:
#' 
#'     ...........
#'     .....###.#.
#'     .###.##..#.
#'     ..#.#...#..
#'     ....#O#....
#'     .##.OS####.
#'     .##..#...#.
#'     .......##..
#'     .##.#.####.
#'     .##..##.##.
#'     ...........
#' 
#' Then, he takes a second step. Since at this point he could be at
#' *either* tile marked `O`, his second step would allow him to reach any
#' garden plot that is one step north, south, east, or west of *any* tile
#' that he could have reached after the first step:
#' 
#'     ...........
#'     .....###.#.
#'     .###.##..#.
#'     ..#.#O..#..
#'     ....#.#....
#'     .##O.O####.
#'     .##.O#...#.
#'     .......##..
#'     .##.#.####.
#'     .##..##.##.
#'     ...........
#' 
#' After two steps, he could be at any of the tiles marked `O` above,
#' including the starting position (either by going north-then-south or by
#' going west-then-east).
#' 
#' A single third step leads to even more possibilities:
#' 
#'     ...........
#'     .....###.#.
#'     .###.##..#.
#'     ..#.#.O.#..
#'     ...O#O#....
#'     .##.OS####.
#'     .##O.#...#.
#'     ....O..##..
#'     .##.#.####.
#'     .##..##.##.
#'     ...........
#' 
#' He will continue like this until his steps for the day have been
#' exhausted. After a total of `6` steps, he could reach any of the garden
#' plots marked `O`:
#' 
#'     ...........
#'     .....###.#.
#'     .###.##.O#.
#'     .O#O#O.O#..
#'     O.O.#.#.O..
#'     .##O.O####.
#'     .##.O#O..#.
#'     .O.O.O.##..
#'     .##.#.####.
#'     .##O.##.##.
#'     ...........
#' 
#' In this example, if the Elf\'s goal was to get exactly `6` more steps
#' today, he could use them to reach any of *`16`* garden plots.
#' 
#' However, the Elf *actually needs to get `64` steps today*, and the map
#' he\'s handed you is much larger than the example map.
#' 
#' Starting from the garden plot marked `S` on your map, *how many garden
#' plots could the Elf reach in exactly `64` steps?*
#'
#' **Part Two**
#' 
#' The Elf seems confused by your answer until he realizes his mistake: he
#' was reading from a [list]{title="Next up: 729."} of his favorite numbers
#' that are both perfect squares and perfect cubes, not his step counter.
#' 
#' The *actual* number of steps he needs to get today is exactly
#' *`26501365`*.
#' 
#' He also points out that the garden plots and rocks are set up so that
#' the map *repeats infinitely* in every direction.
#' 
#' So, if you were to look one additional map-width or map-height out from
#' the edge of the example map above, you would find that it keeps
#' repeating:
#' 
#'     .................................
#'     .....###.#......###.#......###.#.
#'     .###.##..#..###.##..#..###.##..#.
#'     ..#.#...#....#.#...#....#.#...#..
#'     ....#.#........#.#........#.#....
#'     .##...####..##...####..##...####.
#'     .##..#...#..##..#...#..##..#...#.
#'     .......##.........##.........##..
#'     .##.#.####..##.#.####..##.#.####.
#'     .##..##.##..##..##.##..##..##.##.
#'     .................................
#'     .................................
#'     .....###.#......###.#......###.#.
#'     .###.##..#..###.##..#..###.##..#.
#'     ..#.#...#....#.#...#....#.#...#..
#'     ....#.#........#.#........#.#....
#'     .##...####..##..S####..##...####.
#'     .##..#...#..##..#...#..##..#...#.
#'     .......##.........##.........##..
#'     .##.#.####..##.#.####..##.#.####.
#'     .##..##.##..##..##.##..##..##.##.
#'     .................................
#'     .................................
#'     .....###.#......###.#......###.#.
#'     .###.##..#..###.##..#..###.##..#.
#'     ..#.#...#....#.#...#....#.#...#..
#'     ....#.#........#.#........#.#....
#'     .##...####..##...####..##...####.
#'     .##..#...#..##..#...#..##..#...#.
#'     .......##.........##.........##..
#'     .##.#.####..##.#.####..##.#.####.
#'     .##..##.##..##..##.##..##..##.##.
#'     .................................
#' 
#' This is just a tiny three-map-by-three-map slice of the
#' inexplicably-infinite farm layout; garden plots and rocks repeat as far
#' as you can see. The Elf still starts on the one middle tile marked `S`,
#' though - every other repeated `S` is replaced with a normal garden plot
#' (`.`).
#' 
#' Here are the number of reachable garden plots in this new infinite
#' version of the example map for different numbers of steps:
#' 
#' -   In exactly `6` steps, he can still reach *`16`* garden plots.
#' -   In exactly `10` steps, he can reach any of *`50`* garden plots.
#' -   In exactly `50` steps, he can reach *`1594`* garden plots.
#' -   In exactly `100` steps, he can reach *`6536`* garden plots.
#' -   In exactly `500` steps, he can reach *`167004`* garden plots.
#' -   In exactly `1000` steps, he can reach *`668697`* garden plots.
#' -   In exactly `5000` steps, he can reach *`16733044`* garden plots.
#' 
#' However, the step count the Elf needs is much larger! Starting from the
#' garden plot marked `S` on your infinite map, *how many garden plots
#' could the Elf reach in exactly `26501365` steps?*
#'
#' @param x some data
#' @return For Part One, `f21a(x)` returns .... For Part Two,
#'   `f21b(x)` returns ....
#' @export
#' @examples
#' f21a(example_data_21())
#' f21b()
f21a <- function(x) {
  grid <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  start <- which(grid == "S", arr.ind = TRUE)
  pos <- start[1] + start[2]*1i
  offsets <- c(0+1i,0-1i,1+0i,-1+0i)
  for (i in 1:64) {
    pos <- unique(rep(pos, rep(4, length(pos))) + rep(offsets, length(pos)))
    pos <- unique(pos[mapply(\(x, y) grid[x, y] != "#", Re(pos), Im(pos))])
  }
  length(pos)
}

# grid <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
# start <- which(grid == "S")
# min_path <- dijkstra(grid, start, adj = adj)
# y <- grid
# y[min_path[1][[1]] %in% seq(2, 64, by = 2)] <- "O"
# y[start] <- "O"
# sum(y == "O")

#' adj <- function(g, v) {
#'   x <- get_pos(g, v)
#'   i <- x[1]
#'   j <- x[2]
#'   
#'   # can only move no more than 1 tile
#'   res <- abs(floor(0:(prod(dim(g))-1) / nrow(g)) + 1 - i) <= 1 &
#'     abs((0:(prod(dim(g))-1)%%nrow(g)) + 1 - j) <= 1 &
#'     # can't move diagonally
#'     (abs(floor(0:(prod(dim(g))-1) / nrow(g)) + 1 - i) + abs((0:(prod(dim(g))-1)%%nrow(g)) + 1 - j)) == 1
#'   # can't sit still
#'   res[v] <- 0
#'   
#'   as.integer(res & g %in% c("S", "."))
#' }
#' 
#' # adj_2 <- function(g, v) {
#' #   x <- get_pos(g, v)
#' #   i <- x[1]
#' #   j <- x[2]
#' #   
#' #   # can only move no more than 1 tile
#' #   res <- abs(floor(0:(prod(dim(g))-1) / nrow(g)) + 1 - i) %% (ncol(g) - 1) <= 1 &
#' #     abs((0:(prod(dim(g))-1)%%nrow(g)) + 1 - j) %% (nrow(g) - 1) <= 1 & 
#' #     (abs(floor(0:(prod(dim(g))-1) / nrow(g)) + 1 - i) %% (ncol(g)) == 0 |
#' #     abs((0:(prod(dim(g))-1)%%nrow(g)) + 1 - j) %% (nrow(g)) == 0) 
#' #     # can't move diagonally
#' #     # ((abs(floor(0:(prod(dim(g))-1) / nrow(g)) + 1 - i) %% (ncol(g) - 1)) + (abs((0:(prod(dim(g))-1)%%nrow(g)) + 1 - j) %% (nrow(g) - 1))) == 1
#' #   # can't sit still
#' #   res[v] <- 0
#' #   
#' #   as.integer(res & g %in% c("S", "."))
#' # }
#' 
#' 
#' dijkstra <- function(grid, start, adj) {
#'   #' Implementation of dijkstra using on-demand query
#'   #' derived from https://www.algorithms-and-technologies.com/dijkstra/r
#'   #' This returns an array containing the length of the shortest path from the start node to each other node.
#'   #' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
#'   #' This has a runtime of O(|V|^2) (|V| = number of Nodes), for a faster implementation see @see ../fast/Dijkstra.java (using adjacency lists)
#'   #' @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
#'   #' @param start the node to start from.
#'   #' @param dir are we going up or down? passed to can_reach()
#'   #' @return an array containing the shortest distances from the given start node to each other node
#' 
#'   # This contains the distances from the start node to all other nodes
#'   distances = rep(Inf, prod(dim(grid)))
#'   paths = rep(list(), prod(dim(grid)))
#' 
#'   # This contains whether a node was already visited
#'   visited = rep(FALSE, prod(dim(grid)))
#' 
#'   # The distance from the start node to itself is of course 0
#'   distances[start] = 0
#'   paths[[start]] = start
#' 
#'   # While there are nodes left to visit...
#'   repeat{
#' 
#'     # ... find the node with the currently shortest distance from the start node...
#'     shortest_distance = Inf
#'     shortest_index = -1
#'     for(i in seq_along(distances)) {
#'       # ... by going through all nodes that haven't been visited yet
#'       if(distances[i] < shortest_distance && !visited[i]){
#'         shortest_distance = distances[i]
#'         shortest_index = i
#'       }
#'     }
#' 
#'     # cat("Visiting node ", shortest_index, " with current distance ", shortest_distance, "\n")
#' 
#'     if(shortest_index == -1){
#'       # There was no node not yet visited --> We are done
#'       return (list(distances, paths))
#'     }
#'     # ...then, for all neighboring nodes that haven't been visited yet....
#'     # for(i in seq_along(graph[shortest_index,])) {
#'     g <- adj(grid, shortest_index)
#'     for(i in seq_along(g)) {
#'       # ...if the path over this edge is shorter...
#'       if(g[i] != 0 && distances[i] >= distances[shortest_index] + g[i]){
#'         # ...Save this path as new shortest path.
#'         distances[i] = distances[shortest_index] + g[i]
#'         paths[[i]] <- c(paths[[shortest_index]], i)
#' 
#' 
#'         # cat("Updating distance of node ", i, " to ", distances[i], "\n")
#'       }
#'       # Lastly, note that we are finished with this node.
#'       visited[shortest_index] = TRUE
#'       # cat("Visited nodes: ", visited, "\n")
#'       # cat("Currently lowest distances: ", distances, "\n")
#'     }
#'   }
#' }
#' 
#' get_pos <- function(grid, v) {
#'   i <- floor((v-1)/nrow(grid))+1
#'   j <- ((v-1) %% nrow(grid))+1
#'   return(c(i, j))
#' }

#' @rdname day21
#' @export
f21b <- function(x) {
  grid <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  start <- which(grid == "S", arr.ind = TRUE)
  pos <- start[1] + start[2]*1i
  offsets <- c(0+1i,0-1i,1+0i,-1+0i)
  ntarget <- 26501365
  ni <- 3*nrow(grid) + ntarget %% nrow(grid) + 1
  plots <- integer(ni)
  for (i in 1:ni) {
    pos <- unique(rep(pos, rep(4, length(pos))) + rep(offsets, length(pos)))
    pos <- unique(pos[mapply(\(x, y) grid[x, y] != "#", 
                  (Re(pos)-1) %% nrow(grid) + 1,
                  (Im(pos)-1) %% ncol(grid) + 1)])
    plots[i] <- length(pos)
  }
  
  xi <- 0:3*nrow(grid) + ntarget %% nrow(grid)
  
  dat <- data.frame(x = xi, y = plots[xi])
  fit <- lm(y ~ 1 + x + I(x^2), data = dat)
  ans <- predict(fit, newdata = data.frame(x = ntarget))
  sprintf("%.15g", ans)
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

