#' Day 18: Boiling Boulders
#'
#' [Boiling Boulders](https://adventofcode.com/2022/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' You and the elephants finally reach fresh air. You\'ve emerged near the
#' base of a large volcano that seems to be actively erupting! Fortunately,
#' the lava seems to be flowing away from you and toward the ocean.
#'
#' Bits of lava are still being ejected toward you, so you\'re sheltering
#' in the cavern exit a little longer. Outside the cave, you can see the
#' lava landing in a pond and hear it loudly hissing as it solidifies.
#'
#' Depending on the specific compounds in the lava and speed at which it
#' cools, it might be forming
#' [obsidian](https://en.wikipedia.org/wiki/Obsidian){target="_blank"}! The
#' cooling rate should be based on the surface area of the lava droplets,
#' so you take a quick scan of a droplet as it flies past you (your puzzle
#' input).
#'
#' Because of how quickly the lava is moving, the scan isn\'t very good;
#' its resolution is quite low and, as a result, it approximates the shape
#' of the lava droplet with *1x1x1
#' [cubes]{title="Unfortunately, you forgot your flint and steel in another dimension."}
#' on a 3D grid*, each given as its `x,y,z` position.
#'
#' To approximate the surface area, count the number of sides of each cube
#' that are not immediately connected to another cube. So, if your scan
#' were only two adjacent cubes like `1,1,1` and `2,1,1`, each cube would
#' have a single side covered and five sides exposed, a total surface area
#' of *`10`* sides.
#'
#' Here\'s a larger example:
#'
#'     2,2,2
#'     1,2,2
#'     3,2,2
#'     2,1,2
#'     2,3,2
#'     2,2,1
#'     2,2,3
#'     2,2,4
#'     2,2,6
#'     1,2,5
#'     3,2,5
#'     2,1,5
#'     2,3,5
#'
#' In the above example, after counting up all the sides that aren\'t
#' connected to another cube, the total surface area is *`64`*.
#'
#' *What is the surface area of your scanned lava droplet?*
#'
#' **Part Two**
#'
#' Something seems off about your calculation. The cooling rate depends on
#' exterior surface area, but your calculation also included the surface
#' area of air pockets trapped in the lava droplet.
#'
#' Instead, consider only cube sides that could be reached by the water and
#' steam as the lava droplet tumbles into the pond. The steam will expand
#' to reach as much as possible, completely displacing any air on the
#' outside of the lava droplet but never expanding diagonally.
#'
#' In the larger example above, exactly one cube of air is trapped within
#' the lava droplet (at `2,2,5`), so the exterior surface area of the lava
#' droplet is *`58`*.
#'
#' *What is the exterior surface area of your scanned lava droplet?*
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns .... For Part Two,
#'   `f18b(x)` returns ....
#' @export
#' @examples
#' f18a(example_data_18())
#' f18b()
f18a <- function(x) {
  exp <- 0
  for (i in seq_len(nrow(x))) {
    exp <- exp + exposed(i, x)
  }
  exp
}

touching <- function(cube, cubes) {
  thiscube <- cubes[cube, ]
  sides <- 0
  close_cubes <- cubes[abs(cubes[, 1] - cubes[cube, 1]) <= 1 |
                         abs(cubes[, 2] - cubes[cube, 2]) <= 1 |
                         abs(cubes[, 3] - cubes[cube, 3]) <= 1, ]
  if (nrow(close_cubes) == 0) return(0)
  for (r in seq_len(nrow(close_cubes))) {
    if (all(thiscube == close_cubes[r, ])) next
    for (d in 1:3) {
      connected <- abs(thiscube[d] - close_cubes[r, d]) == 1 &&
        all(thiscube[-d] == close_cubes[r, -d])
      if (connected) sides <- sides + 1
    }
  }
  sides
}

exposed <- function(cube, cubes) {
  6 - touching(cube, cubes)
}

outside_faces <- function(cube, cubes) {
  out <- 0
  for (d in 1:3) {
    other_dims <- setdiff(1:3, d)
    same_row <- cubes[-cube, ]
    # tmp <- same_row[, other_dims[1]]
    # if (length(tmp) == 0) next
    same_row <- same_row[same_row[, other_dims[1]] == cubes[cube, other_dims[1]], , drop = FALSE]
    if (nrow(same_row) != 0) {
      same_row <- same_row[same_row[, other_dims[2]] == cubes[cube, other_dims[2]], , drop = FALSE]
    }
    # tmp <- same_row[, other_dims[2]]
    # if (length(tmp) == 0) next
    if (nrow(same_row) == 0) {
      out <- out + 2
      next
    }
    extent <- range(same_row[, d])
    # if (cubes[cube, d] >= extent[2] || cubes[cube, d] <= extent[1] ) return(TRUE)
    # if (cubes[cube, d] %in% range(same_row[, d])) out <- out + 1
    if (cubes[cube, d] < extent[1]) out <- out + 1
    if (cubes[cube, d] > extent[2]) out <- out + 1
  }
  out
}

#' @rdname day18
#' @export
f18b <- function(x) {
  bb <- apply(x, 2, range)
  bb[1, ] <- bb[1, ] - 2
  bb[2, ] <- bb[2, ] + 1

  start <- bb[1, ]

  reachable <- bfs3d(start, x, neighbours, maxnodes = prod(apply(bb, 2, \(x) diff(x) + 1))) # warning: takes a long time
  allpts <- as.matrix(expand.grid(bb[1,1]:bb[2,1], bb[1,2]:bb[2,2], bb[1,3]:bb[2,3]))
  known <- rbind(x, reachable)
  # https://stackoverflow.com/a/44403312/4168169
  unreachable <- allpts[which(tail(!duplicated(rbind(known, allpts)), nrow(allpts))), ]
  # make these into lava
  x2 <- rbind(x, unreachable)

  exp <- 0
  for (i in seq_len(nrow(x2))) {
    exp <- exp + exposed(i, x2)
  }
  exp
}

#' adapted from https://favtutor.com/blogs/breadth-first-search-python
bfs3d <- function(node, x, neighbours, maxnodes) {

  bb <- apply(x, 2, range)
  bb[1, ] <- bb[1, ] - 2
  bb[2, ] <- bb[2, ] + 1

  # visited = [] # List for visited nodes.
  # queue = []     #Initialize a queue
  visited <- matrix(nrow = maxnodes, ncol = 3)
  queue <- matrix(nrow = maxnodes, ncol = 3)
  kv <- 0
  kq <- 0
  #
  # def bfs(visited, graph, node): #function for BFS
  #   visited.append(node)
  # queue.append(node)
  kv <- kv + 1
  visited[kv,] <- node
  kq <- kq + 1
  queue[kq, ] <- node

  #
  # while queue:          # Creating loop to visit each node
  while (kq > 0) {
    #   m = queue.pop(0)
    # print (m, end = " ")
    m <- queue[kq, ]
    kq <- kq - 1

    # if (kv %% 10 == 0) message("visited ", kv, " points")

    #
    # for neighbour in graph[m]:
    #   if neighbour not in visited:
    #   visited.append(neighbour)
    # queue.append(neighbour)
    nb <- neighbours(m, x, bb)
    if (nrow(nb) > 0) {
      for (n in seq_len(nrow(nb))) {
        # message("kv = ", kv)
        # message("n = ", n)
        # print(nb[n, ])
        if (!vecMatch(visited[1:kv, , drop = FALSE], nb[n, ])) {
          # if (!(n %in% visited)) {
          kv <- kv + 1
          visited[kv, ] <- nb[n, ]
          kq <- kq + 1
          queue[kq, ] <- nb[n, ]
        }
      }
    }
  }
  visited[1:kv, ]
}

vecMatch <- function(x, want) {
  any(x[, 1] == want[1] & x[, 2] == want[2] & x[, 3] == want[3])
  # https://stackoverflow.com/a/4294420/4168169
  # for (i in seq_len(nrow(x))) {
  #   if (isTRUE(all.equal(unname(c(x[i,])), want))) return(TRUE)
  # }
  # FALSE
  ##
  # out <- apply(x, 1, function(y) isTRUE(all.equal(unname(c(y)), want)))
  # any(out)
}

# neighbours <- function(x) {
#   switch(x,
#          '5' = c('3', '7'),
#          '3' = c('2', '4'),
#          '7' = c('8'),
#          '2' = c(),
#          '4' = c('8'),
#          '8' = c()
#   )
# }

neighbours <- function(pt, cubes, bb) {
  dirs <- c(-1, 1)
  n <- matrix(nrow = 0, ncol = 3)
  bmaps <- list(c(1, 0, 0), c(0, 1, 0), c(0, 0, 1))
  for (dim in 1:3) {
    for (dir in dirs) {
      newpt <- pt + dir*bmaps[[dim]]
      insidebb <- all(sapply(1:3, \(d) newpt[d] >= bb[1, d] && newpt[d] <= bb[2, d]))
      if (insidebb && !vecMatch(cubes, newpt)) {
        n <- rbind(n, newpt)
      }
    }
  }
  n
}

f18_helper <- function(x) {

}

f18vis <- function(x) {
  # # install.package('remotes')
  # remotes::install_github('coolbutuseless/isocubes')
  colnames(x) <- c("x", "y", "z")
  x <- as.data.frame(x)
  N <- max(apply(x, 2, range)[2,])
  fill <- rgb(red = 1 - x[,1] / N, x[,2] /N, 1 - x[,3]/N, maxColorValue = 1)
  cubes <- isocubes::isocubesGrob(x, fill, ysize = 1/35, xo = 0.5, yo = 0)
  png("inst/vis-day18.png")
  grid::grid.newpage()
  grid::grid.draw(cubes)
  dev.off()
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = as.matrix(read.csv("inst/example18.txt", header = FALSE))
  )
  l[[example]]
}
