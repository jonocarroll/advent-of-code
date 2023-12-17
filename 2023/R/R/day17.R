#' Day 17: Clumsy Crucible
#'
#' [Clumsy Crucible](https://adventofcode.com/2023/day/17)
#'
#' @name day17
#' @rdname day17
#' @details
#'
#' **Part One**
#'
#' The lava starts flowing rapidly once the Lava Production Facility is
#' operational. As you [leave]{title="see you soon?"}, the reindeer offers
#' you a parachute, allowing you to quickly reach Gear Island.
#' 
#' As you descend, your bird\'s-eye view of Gear Island reveals why you had
#' trouble finding anyone on your way up: half of Gear Island is empty, but
#' the half below you is a giant factory city!
#' 
#' You land near the gradually-filling pool of lava at the base of your new
#' *lavafall*. Lavaducts will eventually carry the lava throughout the
#' city, but to make use of it immediately, Elves are loading it into large
#' [crucibles](https://en.wikipedia.org/wiki/Crucible){target="_blank"} on
#' wheels.
#' 
#' The crucibles are top-heavy and pushed by hand. Unfortunately, the
#' crucibles become very difficult to steer at high speeds, and so it can
#' be hard to go in a straight line for very long.
#' 
#' To get Desert Island the machine parts it needs as soon as possible,
#' you\'ll need to find the best way to get the crucible *from the lava
#' pool to the machine parts factory*. To do this, you need to minimize
#' *heat loss* while choosing a route that doesn\'t require the crucible to
#' go in a *straight line* for too long.
#' 
#' Fortunately, the Elves here have a map (your puzzle input) that uses
#' traffic patterns, ambient temperature, and hundreds of other parameters
#' to calculate exactly how much heat loss can be expected for a crucible
#' entering any particular city block.
#' 
#' For example:
#' 
#'     2413432311323
#'     3215453535623
#'     3255245654254
#'     3446585845452
#'     4546657867536
#'     1438598798454
#'     4457876987766
#'     3637877979653
#'     4654967986887
#'     4564679986453
#'     1224686865563
#'     2546548887735
#'     4322674655533
#' 
#' Each city block is marked by a single digit that represents the *amount
#' of heat loss if the crucible enters that block*. The starting point, the
#' lava pool, is the top-left city block; the destination, the machine
#' parts factory, is the bottom-right city block. (Because you already
#' start in the top-left block, you don\'t incur that block\'s heat loss
#' unless you leave that block and then return to it.)
#' 
#' Because it is difficult to keep the top-heavy crucible going in a
#' straight line for very long, it can move *at most three blocks* in a
#' single direction before it must turn 90 degrees left or right. The
#' crucible also can\'t reverse direction; after entering each city block,
#' it may only turn left, continue straight, or turn right.
#' 
#' One way to *minimize heat loss* is this path:
#' 
#'     2>>34^>>>1323
#'     32v>>>35v5623
#'     32552456v>>54
#'     3446585845v52
#'     4546657867v>6
#'     14385987984v4
#'     44578769877v6
#'     36378779796v>
#'     465496798688v
#'     456467998645v
#'     12246868655<v
#'     25465488877v5
#'     43226746555v>
#' 
#' This path never moves more than three consecutive blocks in the same
#' direction and incurs a heat loss of only *`102`*.
#' 
#' Directing the crucible from the lava pool to the machine parts factory,
#' but not moving more than three consecutive blocks in the same direction,
#' *what is the least heat loss it can incur?*
#'
#' **Part Two**
#' 
#' The crucibles of lava simply aren\'t large enough to provide an adequate
#' supply of lava to the machine parts factory. Instead, the Elves are
#' going to upgrade to *ultra crucibles*.
#' 
#' Ultra crucibles are even more difficult to steer than normal crucibles.
#' Not only do they have trouble going in a straight line, but they also
#' have trouble turning!
#' 
#' Once an ultra crucible starts moving in a direction, it needs to move *a
#' minimum of four blocks* in that direction before it can turn (or even
#' before it can stop at the end). However, it will eventually start to get
#' wobbly: an ultra crucible can move a maximum of *ten consecutive blocks*
#' without turning.
#' 
#' In the above example, an ultra crucible could follow this path to
#' minimize heat loss:
#' 
#'     2>>>>>>>>1323
#'     32154535v5623
#'     32552456v4254
#'     34465858v5452
#'     45466578v>>>>
#'     143859879845v
#'     445787698776v
#'     363787797965v
#'     465496798688v
#'     456467998645v
#'     122468686556v
#'     254654888773v
#'     432267465553v
#' 
#' In the above example, an ultra crucible would incur the minimum possible
#' heat loss of *`94`*.
#' 
#' Here\'s another example:
#' 
#'     111111111111
#'     999999999991
#'     999999999991
#'     999999999991
#'     999999999991
#' 
#' Sadly, an ultra crucible would need to take an unfortunate path like
#' this one:
#' 
#'     1>>>>>>>1111
#'     9999999v9991
#'     9999999v9991
#'     9999999v9991
#'     9999999v>>>>
#' 
#' This route causes the ultra crucible to incur the minimum possible heat
#' loss of *`71`*.
#' 
#' Directing the *ultra crucible* from the lava pool to the machine parts
#' factory, *what is the least heat loss it can incur?*
#'
#' @param x some data
#' @return For Part One, `f17a(x)` returns .... For Part Two,
#'   `f17b(x)` returns ....
#' @export
#' @examples
#' f17a(example_data_17())
#' f17b()


## f17a <- function(x) {
##   rows <- strsplit(x, "")
##   grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
##   ngrid <- grid
##   mode(ngrid) <- "integer"
##   startat <- 1
##   endat <- length(grid)
##   ngrid[1] <- 0
##   
##   ans <- Inf
##   for (p in which(can_reach(grid, startat, FALSE) != 0)) {
##     min_path <- dijkstra(ngrid, p, dir = -1)
##     newval <- min_path[1][[1]][endat]
##     message("starting at ", p, " takes ", newval)
##     ans <- min(ans, newval)
##   }
##   ans
## }
## 
## 
## get_pos <- function(grid, v) {
##   i <- floor((v-1)/nrow(grid))+1
##   j <- ((v-1) %% nrow(grid))+1
##   return(c(i, j))
## }
## 
## can_reach <- function(ngrid, v, dir = 1, visited) {
##   x <- get_pos(ngrid, v)
##   i <- x[1]
##   j <- x[2]
## 
##   # can only move no more than 1 tile
##   res <- abs(floor(0:(prod(dim(ngrid))-1) / nrow(ngrid)) + 1 - i) <= 1 &
##     abs((0:(prod(dim(ngrid))-1)%%nrow(ngrid)) + 1 - j) <= 1 &
##     # can't move diagonally
##     (abs(floor(0:(prod(dim(ngrid))-1) / nrow(ngrid)) + 1 - i) + abs((0:(prod(dim(ngrid))-1)%%nrow(ngrid)) + 1 - j)) == 1
##   # can't sit still
##   res[v] <- 0
## 
##   as.integer(ifelse(res,ngrid,0))
## }
## 
## dijkstra <- function(grid, start, dir = -1){
##   #' Implementation of dijkstra using on-demand query
##   #' derived from https://www.algorithms-and-technologies.com/dijkstra/r
##   #' This returns an array containing the length of the shortest path from the start node to each other node.
##   #' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
##   #' This has a runtime of O(|V|^2) (|V| = number of Nodes), for a faster implementation see @see ../fast/Dijkstra.java (using adjacency lists)
##   #' @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
##   #' @param start the node to start from.
##   #' @param dir are we going up or down? passed to can_reach()
##   #' @return an array containing the shortest distances from the given start node to each other node
##   
##   # This contains the distances from the start node to all other nodes
##   distances = rep(Inf, prod(dim(grid)))
##   paths = rep(list(), prod(dim(grid)))
##   
##   # This contains whether a node was already visited
##   visited = rep(FALSE, prod(dim(grid)))
##   
##   # The distance from the start node to itself is of course 0
##   distances[start] = 0
##   paths[[start]] = start
##   
##   # While there are nodes left to visit...
##   repeat{
##     
##     # ... find the node with the currently shortest distance from the start node...
##     shortest_distance = Inf
##     shortest_index = -1
##     for(i in seq_along(distances)) {
##       # ... by going through all nodes that haven't been visited yet
##       if(distances[i] < shortest_distance && !visited[i]){
##         shortest_distance = distances[i]
##         shortest_index = i
##       }
##     }
##     
##     # cat("Visiting node ", shortest_index, " with current distance ", shortest_distance, "\n")
##     
##     if(shortest_index == -1){
##       # There was no node not yet visited --> We are done
##       return (list(distances, paths))
##     }
##     # ...then, for all neighboring nodes that haven't been visited yet....
##     # for(i in seq_along(graph[shortest_index,])) {
##     g <- can_reach(grid, shortest_index, dir = dir, visited = visited)
##     for(i in seq_along(g)) {
##       # ...if the path over this edge is shorter...
##       # if(graph[shortest_index,i] != 0 && distances[i] > distances[shortest_index] + graph[shortest_index,i]){
##         # if recently more than 3 same row or 3 same col, make Inf
##       rec <- tail(c(paths[[shortest_index]], i), 5)
##       pos <- sapply(rec, \(w) get_pos(grid, w))
##       # if (any(table(pos[1,]) == 5) | any(table(pos[2,]) == 5)) {
##       if (any(rle(rev(pos[1,]))$lengths > 4) | any(rle(rev(pos[2,]))$lengths > 4)) {
##         g[i] <- 0
##       }
##       if(g[i] != 0 && distances[i] >= distances[shortest_index] + g[i]){
##         # ...Save this path as new shortest path.
##         distances[i] = distances[shortest_index] + g[i]
##         paths[[i]] <- c(paths[[shortest_index]], i)
##         
##         
##         # cat("Updating distance of node ", i, " to ", distances[i], "\n")
##       }
##       # Lastly, note that we are finished with this node.
##       visited[shortest_index] = TRUE
##       # cat("Visited nodes: ", visited, "\n")
##       # cat("Currently lowest distances: ", distances, "\n")
##     }
##   }
## }


# y <- ngrid
# p <- min_path[2][[1]][[prod(dim(y))]]
# # p <- min_path[2][[1]][[1]]
# y[p] <- "#"
# y

## 
## #' @rdname day17
## #' @export
## f17b <- function(x) {
## 
## }


f17_helper <- function(x) {

}

#' @param x some data
#' @return For Part One, `f17a(x)` returns .... For Part Two,
#'   `f17b(x)` returns ....
#' @export
#' @examples
#' f17a(example_data_17())
#' f17b()
f17a <- f17b <- function(x, r) {
  rows <- strsplit(x, "")
  grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
  ngrid <- grid
  mode(ngrid) <- "integer"
  n <- nrow(ngrid)
  # could go horizontal or vertical, so track both
  h <- matrix(sum(ngrid), nrow = n, ncol = n)
  h[1] <- 0
  v <- h
  while (TRUE) {
    s <- sum(h) + sum(v)
    for (rc in 1:n) {
      for (ndir in r) {
        if (rc >= ndir + 1) {
          v[rc,] <- pmin(v[rc,], 
                        h[rc-ndir, ] + colSums(ngrid[(rc-ndir+1):rc, , drop=FALSE])
          )
        }
        if (rc + ndir <= n) {
          v[rc,] <- pmin(v[rc,], 
                        h[rc+ndir, ] + colSums(ngrid[rc:(rc+ndir-1), , drop=FALSE])
          )
        }
        if (rc >= ndir + 1) {
          h[, rc] <-  pmin(h[, rc], 
                          v[, rc-ndir] + rowSums(ngrid[, (rc-ndir+1):rc, drop=FALSE])
          )
        }
        if (rc + ndir <= n) {
          h[, rc] <- pmin(h[, rc], 
                         v[, rc+ndir] + rowSums(ngrid[, rc:(rc+ndir-1), drop=FALSE])
          )
        }
      }
    }
    if (s == sum(h) + sum(v)) break
  }
  min(h[length(h)], v[length(v)])
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day17
#' @export
example_data_17 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
