#' Day 16: Proboscidea Volcanium
#'
#' [Proboscidea Volcanium](https://adventofcode.com/2022/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' The sensors have led you to the origin of the distress signal: yet
#' another handheld device, just like the one the Elves gave you. However,
#' you don\'t see any Elves around; instead, the device is surrounded by
#' elephants! They must have gotten lost in these tunnels, and one of the
#' elephants apparently figured out how to turn on the distress signal.
#'
#' The ground rumbles again, much stronger this time. What kind of cave is
#' this, exactly? You scan the cave with your handheld device; it reports
#' mostly igneous rock, some ash, pockets of pressurized gas, magma\...
#' this isn\'t just a cave, it\'s a volcano!
#'
#' You need to get the elephants out of here, quickly. Your device
#' estimates that you have *30 minutes* before the volcano erupts, so you
#' don\'t have time to go back out the way you came in.
#'
#' You scan the cave for other options and discover a network of pipes and
#' pressure-release *valves*. You aren\'t sure how such a system got into a
#' volcano, but you don\'t have time to complain; your device produces a
#' report (your puzzle input) of each valve\'s *flow rate* if it were
#' opened (in pressure per minute) and the tunnels you could use to move
#' between the valves.
#'
#' There\'s even a valve in the room you and the elephants are currently
#' standing in labeled `AA`. You estimate it will take you one minute to
#' open a single valve and one minute to follow any tunnel from one valve
#' to another. What is the most pressure you could release?
#'
#' For example, suppose you had the following scan output:
#'
#'     Valve AA has flow rate=0; tunnels lead to valves DD, II, BB
#'     Valve BB has flow rate=13; tunnels lead to valves CC, AA
#'     Valve CC has flow rate=2; tunnels lead to valves DD, BB
#'     Valve DD has flow rate=20; tunnels lead to valves CC, AA, EE
#'     Valve EE has flow rate=3; tunnels lead to valves FF, DD
#'     Valve FF has flow rate=0; tunnels lead to valves EE, GG
#'     Valve GG has flow rate=0; tunnels lead to valves FF, HH
#'     Valve HH has flow rate=22; tunnel leads to valve GG
#'     Valve II has flow rate=0; tunnels lead to valves AA, JJ
#'     Valve JJ has flow rate=21; tunnel leads to valve II
#'
#' All of the valves begin *closed*. You start at valve `AA`, but it must
#' be damaged or
#' [jammed]{title="Wait, sir! The valve, sir! it appears to be... jammed!"}
#' or something: its flow rate is `0`, so there\'s no point in opening it.
#' However, you could spend one minute moving to valve `BB` and another
#' minute opening it; doing so would release pressure during the remaining
#' *28 minutes* at a flow rate of `13`, a total eventual pressure release
#' of `28 * 13 = `*`364`*. Then, you could spend your third minute moving
#' to valve `CC` and your fourth minute opening it, providing an additional
#' *26 minutes* of eventual pressure release at a flow rate of `2`, or
#' *`52`* total pressure released by valve `CC`.
#'
#' Making your way through the tunnels like this, you could probably open
#' many or all of the valves by the time 30 minutes have elapsed. However,
#' you need to release as much pressure as possible, so you\'ll need to be
#' methodical. Instead, consider this approach:
#'
#'     == Minute 1 ==
#'     No valves are open.
#'     You move to valve DD.
#'
#'     == Minute 2 ==
#'     No valves are open.
#'     You open valve DD.
#'
#'     == Minute 3 ==
#'     Valve DD is open, releasing 20 pressure.
#'     You move to valve CC.
#'
#'     == Minute 4 ==
#'     Valve DD is open, releasing 20 pressure.
#'     You move to valve BB.
#'
#'     == Minute 5 ==
#'     Valve DD is open, releasing 20 pressure.
#'     You open valve BB.
#'
#'     == Minute 6 ==
#'     Valves BB and DD are open, releasing 33 pressure.
#'     You move to valve AA.
#'
#'     == Minute 7 ==
#'     Valves BB and DD are open, releasing 33 pressure.
#'     You move to valve II.
#'
#'     == Minute 8 ==
#'     Valves BB and DD are open, releasing 33 pressure.
#'     You move to valve JJ.
#'
#'     == Minute 9 ==
#'     Valves BB and DD are open, releasing 33 pressure.
#'     You open valve JJ.
#'
#'     == Minute 10 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve II.
#'
#'     == Minute 11 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve AA.
#'
#'     == Minute 12 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve DD.
#'
#'     == Minute 13 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve EE.
#'
#'     == Minute 14 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve FF.
#'
#'     == Minute 15 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve GG.
#'
#'     == Minute 16 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You move to valve HH.
#'
#'     == Minute 17 ==
#'     Valves BB, DD, and JJ are open, releasing 54 pressure.
#'     You open valve HH.
#'
#'     == Minute 18 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You move to valve GG.
#'
#'     == Minute 19 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You move to valve FF.
#'
#'     == Minute 20 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You move to valve EE.
#'
#'     == Minute 21 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You open valve EE.
#'
#'     == Minute 22 ==
#'     Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
#'     You move to valve DD.
#'
#'     == Minute 23 ==
#'     Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
#'     You move to valve CC.
#'
#'     == Minute 24 ==
#'     Valves BB, DD, EE, HH, and JJ are open, releasing 79 pressure.
#'     You open valve CC.
#'
#'     == Minute 25 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     == Minute 26 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     == Minute 27 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     == Minute 28 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     == Minute 29 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     == Minute 30 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#' This approach lets you release the most pressure possible in 30 minutes
#' with this valve layout, *`1651`*.
#'
#' Work out the steps to release the most pressure in 30 minutes. *What is
#' the most pressure you can release?*
#'
#' **Part Two**
#'
#' You\'re worried that even with an optimal approach, the pressure
#' released won\'t be enough. What if you got one of the elephants to help
#' you?
#'
#' It would take you 4 minutes to teach an elephant how to open the right
#' valves in the right order, leaving you with only *26 minutes* to
#' actually execute your plan. Would having two of you working together be
#' better, even if it means having less time? (Assume that you teach the
#' elephant before opening any valves yourself, giving you both the same
#' full 26 minutes.)
#'
#' In the example above, you could teach the elephant to help you as
#' follows:
#'
#'     == Minute 1 ==
#'     No valves are open.
#'     You move to valve II.
#'     The elephant moves to valve DD.
#'
#'     == Minute 2 ==
#'     No valves are open.
#'     You move to valve JJ.
#'     The elephant opens valve DD.
#'
#'     == Minute 3 ==
#'     Valve DD is open, releasing 20 pressure.
#'     You open valve JJ.
#'     The elephant moves to valve EE.
#'
#'     == Minute 4 ==
#'     Valves DD and JJ are open, releasing 41 pressure.
#'     You move to valve II.
#'     The elephant moves to valve FF.
#'
#'     == Minute 5 ==
#'     Valves DD and JJ are open, releasing 41 pressure.
#'     You move to valve AA.
#'     The elephant moves to valve GG.
#'
#'     == Minute 6 ==
#'     Valves DD and JJ are open, releasing 41 pressure.
#'     You move to valve BB.
#'     The elephant moves to valve HH.
#'
#'     == Minute 7 ==
#'     Valves DD and JJ are open, releasing 41 pressure.
#'     You open valve BB.
#'     The elephant opens valve HH.
#'
#'     == Minute 8 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You move to valve CC.
#'     The elephant moves to valve GG.
#'
#'     == Minute 9 ==
#'     Valves BB, DD, HH, and JJ are open, releasing 76 pressure.
#'     You open valve CC.
#'     The elephant moves to valve FF.
#'
#'     == Minute 10 ==
#'     Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
#'     The elephant moves to valve EE.
#'
#'     == Minute 11 ==
#'     Valves BB, CC, DD, HH, and JJ are open, releasing 78 pressure.
#'     The elephant opens valve EE.
#'
#'     (At this point, all valves are open.)
#'
#'     == Minute 12 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     ...
#'
#'     == Minute 20 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#'     ...
#'
#'     == Minute 26 ==
#'     Valves BB, CC, DD, EE, HH, and JJ are open, releasing 81 pressure.
#'
#' With the elephant helping, after 26 minutes, the best you could do would
#' release a total of *`1707`* pressure.
#'
#' *With you and an elephant working together for 26 minutes, what is the
#' most pressure you could release?*
#'
#' @param x some data
#' @return For Part One, `f16a(x)` returns .... For Part Two,
#'   `f16b(x)` returns ....
#' @export
#' @examples
#' f16a(example_data_16())
#' f16b()
f16a <- function(x) {
  valves <- lapply(x, parseinput)
  valves <- setNames(valves, sapply(valves, "[[", "valve"))
  dists <- matrix(nrow = 0, ncol = length(valves))
  for (i in seq_along(valves)) {
    dists <- rbind(dists, matrix(dijkstra(valves, i)[[1]], nrow = 1))
  }
  colnames(dists) <- rownames(dists) <- names(valves)

  unname(calc_pressure("AA", valves, dists, 30))
}

#' @rdname day16
#' @export
f16b <- function(x) {
  valves <- lapply(x, parseinput)
  valves <- setNames(valves, sapply(valves, "[[", "valve"))
  dists <- matrix(nrow = 0, ncol = length(valves))
  for (i in seq_along(valves)) {
    dists <- rbind(dists, matrix(dijkstra(valves, i)[[1]], nrow = 1))
  }
  colnames(dists) <- rownames(dists) <- names(valves)

  paths_cache <<- vector(mode = "integer", length = 1e6) # assigned globally, as big as we want
  names_cache <<- vector(mode = "integer", length = 1e6) # assigned globally, as big as we want
  cachelen <<- 0

  ## Dr Strange style: look at all possible paths the elephant could take and
  ## for each of them, I run the optimal route with the remaining valves

  findpaths_ <- calc_pressure_all("AA", valves, dists, 26)
  message("futures calculated")
  futures <- setNames(paths_cache[1:cachelen], names_cache[1:cachelen])
  futures <- rev(sort(futures[futures > 800]))

  mypaths <- futures
  mypaths[] <- 0

  largest_seen <- 0

  statecache <- list()

  for (f in seq_along(futures)) {
    elephant <- futures[f]
    if (elephant < (largest_seen / 2)) break
    if (nchar(names(elephant)) == 0) next
    elephant_opens <- get_paths(elephant)
    valves2 <- valves
    for (v in seq_along(valves2)) {
      if (valves[[v]]$valve %in% elephant_opens) {
      valves2[[v]]$rate <- 0
      }
    }
    if (all(sapply(valves2, "[[", "rate") == 0)) next
    valvestate <- paste0(sapply(valves2, "[[", "rate"), collapse = "")
    if (valvestate %in% names(statecache)) {
      mypaths[f] <- statecache[[valvestate]]
      next
    }
    # can we beat this even if we open all the valves?
    if (elephant + 26*sum(sapply(valves2, "[[", "rate")) < largest_seen) next
    tmp <- calc_pressure("AA", valves2, dists, 26)
    mypaths[f] <- tmp
    names(mypaths)[f] <- names(tmp)
    statecache[[valvestate]] <- tmp
    largest_seen <- max(largest_seen, futures[f] + mypaths[f])
  }

  totalpressure <- futures + mypaths
  message("futures: ")
  print(futures[which.max(totalpressure)])
  message("mypaths:")
  print(mypaths[which.max(totalpressure)])
  message("Highest total pressure:")
  print(max(totalpressure))
  max(totalpressure)
}


adjacency <- function(valves, i) {
  nodes <- names(valves)
  adj <- sapply(valves, "[[", "tunnels")
  adj_nodes <- sapply(adj, \(y) match(y, nodes))
  m <- matrix(0, nrow = length(nodes), ncol = length(nodes))
  for (j in seq_along(nodes)) {
    m[matrix(c(rep(j, length(adj_nodes[[j]])), adj_nodes[[j]]), ncol = 2)] <- 1
  }
  m[i, ]
}

calc_pressure <- function(cave, valves, distances, time_left, path_to_here = "") {
  if (time_left <= 0) return(setNames(0, path_to_here))
  # only consider closed valves with rate > 0
  valves <- valves[sapply(valves, "[[", "state") == 0 & sapply(valves, "[[", "rate") > 0]
  if (length(valves) == 0) return(setNames(0, path_to_here))
  # for each potential valve, calculate total pressure after travelling to and opening
  found_pressures <- c()
  explored_paths <- c()
  for (i in names(valves)) {
    time_left_after_opening <- max(0, time_left - distances[cave, i] - 1)
    pressure_from_valve <- time_left_after_opening * valves[[i]]$rate
    next_steps <- calc_pressure(i, valves[names(valves) != i], distances, time_left_after_opening, paste0(path_to_here, i, collapse = ""))
    found_pressures <- c(found_pressures, pressure_from_valve + next_steps)
    explored_paths <- c(explored_paths, names(next_steps))
  }
  max_i <- which.max(found_pressures)
  max_p <- found_pressures[max_i]
  best_path <- explored_paths[max_i]
  return(setNames(max_p, best_path))
}

calc_pressure_all <- function(cave, valves, distances, time_left, path_to_here = "", pressure_to_here = 0) {
  if (time_left <= 0) {
    # paths_cache <<- c(paths_cache, setNames(pressure_to_here, path_to_here))
    return(setNames(pressure_to_here, path_to_here))
  }
  # don't continue if we've been here already
  # only consider closed valves with rate > 0
  valves <- valves[sapply(valves, "[[", "state") == 0 & sapply(valves, "[[", "rate") > 0]
  if (length(valves) == 0) {
    # paths_cache <<- c(paths_cache, setNames(pressure_to_here, path_to_here))
    return(setNames(pressure_to_here, path_to_here))
  }
  # for each potential valve, calculate total pressure after travelling to and opening
  found_pressures <- c()
  # explored_paths <- c()
  for (i in names(valves)) {
    path_would_be <- paste0(path_to_here, i, collapse = "")
    # full_cache <- get("paths_cache", envir = .GlobalEnv)
    if (path_would_be %in% names(paths_cache)) {
      message("skipping", path_would_be)
      next
    }
    # message("path_would_be = ")
    # print(path_would_be)
    time_left_after_opening <- max(0, time_left - distances[cave, i] - 1)
    pressure_from_valve <- time_left_after_opening * valves[[i]]$rate
    p <- setNames(pressure_to_here + pressure_from_valve, path_would_be)
    paths_cache[cachelen + 1:length(p)] <<- p
    names_cache[cachelen + 1:length(p)] <<- names(p)
    cachelen <<- cachelen + length(p)
    next_steps <- calc_pressure_all(i, valves[names(valves) != i], distances, time_left_after_opening, path_would_be, pressure_to_here + pressure_from_valve)
    found_pressures <- c(found_pressures, pressure_from_valve + next_steps)
    # explored_paths <- c(explored_paths, names(next_steps))
  }
  # max_i <- which.max(found_pressures)
  # max_p <- found_pressures[max_i]
  # best_path <- explored_paths[max_i]
  # return(setNames(max_p, best_path))
  # new_cache <- c(get("paths_cache", envir = .GlobalEnv), found_pressures)
  # assign("paths_cache", new_cache, envir = .GlobalEnv)
  # paths_cache <<- c(paths_cache, )
  # return(found_pressures)
  return(found_pressures)
}

# calc_pressure2 <- function(cave, valves, distances, time_left, path_to_here = cave) {
#   if (time_left <= 0) return(setNames(0, path_to_here))
#   # only consider closed valves with rate > 0
#   valves <- valves[sapply(valves, "[[", "state") == 0 & sapply(valves, "[[", "rate") > 0]
#   if (length(valves) == 0) return(setNames(0, path_to_here))
#   # for each potential valve, calculate total pressure after travelling to and opening
#   i_found_pressures <- c()
#   e_found_pressures <- c()
#   i_explored_paths <- c()
#   e_explored_paths <- c()
#   for (i in names(valves)) {
#     for (e in names(valves)) {
#       if (i == e) next;
#       i_time_left_after_opening <- max(0, time_left - distances[cave, i] - 1)
#       i_pressure_from_valve <- i_time_left_after_opening * valves[[i]]$rate
#       i_next_steps <- calc_pressure(i, valves[!names(valves) == i], distances, i_time_left_after_opening, paste0(path_to_here, i, collapse = ""))
#
#       e_time_left_after_opening <- max(0, time_left - distances[cave, e] - 1)
#       e_pressure_from_valve <- e_time_left_after_opening * valves[[e]]$rate
#       e_next_steps <- calc_pressure(e, valves[!names(valves) == e], distances, e_time_left_after_opening, paste0(path_to_here, e, collapse = ""))
#
#       # if (!anyDuplicated(c(get_paths(i_next_steps), get_paths(e_next_steps)))) {
#       # message("me : ", get_paths(i_next_steps))
#       # message("ele: ", get_paths(e_next_steps))
#       # foo <- readline()
#
#       if (length(intersect(get_paths(i_next_steps)[-1], get_paths(e_next_steps)[-1])) == 0) {
#         browser()
#         e_found_pressures <- c(e_found_pressures, e_pressure_from_valve + e_next_steps)
#         i_found_pressures <- c(i_found_pressures, i_pressure_from_valve + i_next_steps)
#
#         i_explored_paths <- c(i_explored_paths, names(i_next_steps))
#         e_explored_paths <- c(e_explored_paths, names(e_next_steps))
#       } else {
#         next
#       }
#
#     }
#   }
#
#   # max_i <- which.max(found_pressures)
#   # max_p <- found_pressures[max_i]
#   max_i <- which.max(i_found_pressures + e_found_pressures)
#   max_p <- i_found_pressures[max_i] + e_found_pressures[max_i]
#   message("ME : ", i_explored_paths[max_i])
#   message("ELE: ", e_explored_paths[max_i])
#   # best_path <- explored_paths[max_i]
#   # return(setNames(max_p, best_path))
#   return(max_p)
# #
# #
# #
# #   # i_best_path <- i_explored_paths[max_i]
# #   # e_best_path <- e_explored_paths[max_i]
# #   return(max_p)
# }

#
# #' adapted from https://favtutor.com/blogs/breadth-first-search-python
# #' re-adapted after day 18
# bfs <- function(node, x) {
#
#   # maxnodes <- length(x)
#   # bb <- apply(x, 2, range)
#   # bb[1, ] <- bb[1, ] - 2
#   # bb[2, ] <- bb[2, ] + 1
#
#   # visited = [] # List for visited nodes.
#   # queue = []     #Initialize a queue
#   visited <- c()
#   queue <- c()
#   # kv <- 0
#   # kq <- 0
#   #
#   # def bfs(visited, graph, node): #function for BFS
#   #   visited.append(node)
#   # queue.append(node)
#   # kv <- kv + 1
#   # visited[kv,] <- node
#   visited <- c(visited, node)
#   # kq <- kq + 1
#   # queue[kq, ] <- node
#   queue <- c(queue, node)
#
#   #
#   # while queue:          # Creating loop to visit each node
#   while (length(queue) > 0) {
#     #   m = queue.pop(0)
#     # print (m, end = " ")
#     # m <- queue[kq, ]
#     # kq <- kq - 1
#     m <- tail(queue, 1)
#     queue <- head(queue, -1)
#
#     # if (kv %% 10 == 0) message("visited ", kv, " points")
#
#     #
#     # for neighbour in graph[m]:
#     #   if neighbour not in visited:
#     #   visited.append(neighbour)
#     # queue.append(neighbour)
#     # nb <- neighbours(m, x)
#     nb <- x[[m]]$tunnels
#     if (nrow(nb) > 0) {
#       for (n in seq_len(nrow(nb))) {
#         # message("kv = ", kv)
#         # message("n = ", n)
#         # print(nb[n, ])
#         # if (!vecMatch(visited[1:kv, , drop = FALSE], nb[n, ])) {
#         if (!n %in% visited) {
#           # if (!(n %in% visited)) {
#           # kv <- kv + 1
#           # visited[kv, ] <- nb[n, ]
#           # kq <- kq + 1
#           visited <- c(visited, n)
#           # queue[kq, ] <- nb[n, ]
#           queue <- c(queue, n)
#         }
#       }
#     }
#   }
#   visited
# }
#
# vecMatch <- function(x, want) {
#   if (length(want) == 2) {
#     any(x[, 1] == want[1] & x[, 2] == want[2])
#   } else if (length(want) == 3) {
#     any(x[, 1] == want[1] & x[, 2] == want[2] & x[, 3] == want[3])
#   }
# }
#
get_paths <- function(x) {
  open_valves_str <- names(x)
  if (nchar(open_valves_str) == 2) return(open_valves_str)
  substring(open_valves_str, seq(1,nchar(open_valves_str)-1,2), seq(2,nchar(open_valves_str),2))
}

dijkstra <- function(grid, start){
  #' Implementation of dijkstra using on-demand query
  #' derived from https://www.algorithms-and-technologies.com/dijkstra/r
  #' This returns an array containing the length of the shortest path from the start node to each other node.
  #' It is only guaranteed to return correct results if there are no negative edges in the graph. Positive cycles are fine.
  #' This has a runtime of O(|V|^2) (|V| = number of Nodes), for a faster implementation see @see ../fast/Dijkstra.java (using adjacency lists)
  #' @param graph an adjacency-matrix-representation of the graph where (x,y) is the weight of the edge or 0 if there is no edge.
  #' @param start the node to start from.
  #' @return an array containing the shortest distances from the given start node to each other node

  # This contains the distances from the start node to all other nodes
  # distances = rep(Inf, prod(dim(grid)))
  distances <- rep(Inf, length(grid))
  # paths = rep(list(), prod(dim(grid)))
  paths <- rep(list(), length(grid))


  # This contains whether a node was already visited
  # visited = rep(FALSE, prod(dim(grid)))
  visited <- rep(FALSE, length(grid))

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
    g <- adjacency(grid, shortest_index)
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

# # a = list(value = 0, children = list(list(value = 1, children = list(list(value = 3), list(value = 4))), list(value = 2, children = list(list(value = 5), list(value = 6)))))
# dfs <- function(start, target) {
#   #' adapted from https://www.algorithms-and-technologies.com/dfs/r
#   #' Implementation of DFS (depth-first search) algorithm to find the shortest path from a start to a target node..
#   #' Given a start node, this returns the node in the tree below the start node with the target value (or null if it doesn't exist)
#   #' Runs in O(n), where n is the number of nodes in the tree, or O(b^d), where b is the branching factor and d is the depth.
#   #' @param start  the node to start the search from
#   #' @param target the value to search for
#   #' @return The node containing the target value or null if it doesn't exist.
#
#   cat("Visiting Node ", start$value, "\n")
#   if(start$value == target){
#     # We have found the goal node we we're searching for
#     print("Found the node we're looking for!")
#     return (start)
#   }
#
#   # Recurse with all children
#   for(i in seq_along(start$children)) {
#     result = dfs(start$children[[i]], target)
#     if (!is.null(result)) {
#       # We've found the goal node while going down that child
#       return (result)
#     }
#   }
#
#   # We've gone through all children and not found the goal node
#   cat("Went through all children of ", start$value, ", returning to it's parent.", "\n")
#   return (NULL)
# }

parseinput <- function(x) {
  valve <- sub("Valve ([A-Z]{2}).*", "\\1", x)
  rate <- as.integer(sub(".*rate=(.*);.*", "\\1", x))
  tunnels <- unname(unlist(read.csv(text = sub(".*to valve[s]? ", "", x), header = FALSE)))
  list(
    valve = valve,
    rate = rate,
    state = 0,
    tunnels = trimws(tunnels)
  )
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day16
#' @export
example_data_16 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example16.txt")
  )
  l[[example]]
}
