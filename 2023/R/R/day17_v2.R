f17a <- function(x) {
  rows <- strsplit(x, "")
  grid <- matrix(unlist(rows), ncol = nchar(x[1]), byrow = TRUE)
  ngrid <- grid
  mode(ngrid) <- "integer"
  startat <- 1
  endat <- length(grid)
  ngrid[1] <- 0
  
  adjacency_matrix <- can_reach(ngrid)
  graph <- graph_from_adjacency_matrix(adjacency_matrix, mode = "directed")
  
  ans <- Inf
  for (p in which(adjacency_matrix[startat, ] != 0)) {
    all_path <- dijkstra(graph, start = p, end = endat)
    # valid_path <- purrr::map_lgl(all_path[[1]], ~{
    #   pos <- sapply(.x, \(w) get_pos(ngrid, w))
    #   !(any(rle(rev(pos[1,]))$lengths > 4) | any(rle(rev(pos[2,]))$lengths > 4))
    # })
    # good <- all_path[[1]][valid_path]
    # best <- min(purrr::map_int(good, ~{sum(ngrid[.x])}))
    newval <- min_path[[1]][endat]
    # message("starting at ", p, " takes ", newval)
    ans <- min(ans, newval)
  }
  ans
}
  
#   ans <- Inf
#   for (p in which(adjacency_matrix[startat, ] != 0)) {
#     min_path <- dijkstra(graph, start = p)
#     newval <- min_path[[1]][endat]
#     message("starting at ", p, " takes ", newval)
#     ans <- min(ans, newval)
#   }
#   ans
# }



get_pos <- function(grid, v) {
  i <- floor((v-1)/nrow(grid))+1
  j <- ((v-1) %% nrow(grid))+1
  return(c(i, j))
}


#' dijkstra <- function(grid, start, dir = -1){
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
#'     g <- can_reach(grid)[shortest_index,]*ngrid
#'     for(i in seq_along(g)) {
#'       # ...if the path over this edge is shorter...
#'       # if(graph[shortest_index,i] != 0 && distances[i] > distances[shortest_index] + graph[shortest_index,i]){
#'       # if recently more than 3 same row or 3 same col, make Inf
#'       rec <- tail(c(paths[[shortest_index]], i), 5)
#'       pos <- sapply(rec, \(w) get_pos(grid, w))
#'       # if (any(table(pos[1,]) == 5) | any(table(pos[2,]) == 5)) {
#'       if (any(rle(rev(pos[1,]))$lengths > 4) | any(rle(rev(pos[2,]))$lengths > 4)) {
#'         g[i] <- 0
#'       }
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

dijkstra <- function(graph, start, end){
  # Implementation of dijkstra using igraph
  # This returns an array containing the length of the shortest path from the start node to each other node.
  # It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
  # This has a runtime of O(|V|^2) (|V| = number of Nodes), for a faster implementation see @see ../fast/Dijkstra.java (using adjacency lists)
  # @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
  # @param start the node to start from.
  # @return an array containing the shortest distances from the given start node to each other node
  
  # This contains the distances from the start node to all other nodes
  distances <- shortest_paths(graph, from = start, to = end, mode = "out")[[1]]
  return(list(distances, list()))
}


can_reach <- function(ngrid) {
  nrows <- nrow(ngrid)
  ncols <- ncol(ngrid)
  adjacency_matrix <- matrix(0, nrow = nrows * ncols, ncol = nrows * ncols)
  
  for (v in 1:(nrows * ncols)) {
    x <- get_pos(ngrid, v)
    i <- x[1]
    j <- x[2]
    
    for (w in 1:(nrows * ncols)) {
      y <- get_pos(ngrid, w)
      row_diff <- abs(y[1] - i)
      col_diff <- abs(y[2] - j)
      
      # Check if the move is within the allowed range
      if (row_diff <= 3 && col_diff <= 3 && row_diff + col_diff == 1) {
        # Check for consecutive moves in the same direction
        consecutive_moves_row <- sum(diff(c(i, y[1])) == 0)
        consecutive_moves_col <- sum(diff(c(j, y[2])) == 0)
        
        if (consecutive_moves_row <= 3 && consecutive_moves_col <= 3) {
          adjacency_matrix[v, w] <- 1
        }
      }
    }
  }
  
  return(adjacency_matrix)
}
