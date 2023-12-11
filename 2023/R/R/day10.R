#' Day 10: Pipe Maze
#'
#' [Pipe Maze](https://adventofcode.com/2023/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' You use the hang glider to ride the hot air from Desert Island all the
#' way up to the floating metal island. This island is surprisingly cold
#' and there definitely aren\'t any thermals to glide on, so you leave your
#' hang glider behind.
#' 
#' You wander around for a while, but you don\'t find any people or
#' animals. However, you do occasionally find signposts labeled \"[Hot
#' Springs](https://en.wikipedia.org/wiki/Hot_spring){target="_blank"}\"
#' pointing in a seemingly consistent direction; maybe you can find someone
#' at the hot springs and ask them where the desert-machine parts are made.
#' 
#' The landscape here is alien; even the flowers and trees are made of
#' metal. As you stop to admire some metal grass, you notice something
#' metallic scurry away in your peripheral vision and jump into a big pipe!
#' It didn\'t look like any animal you\'ve ever seen; if you want a better
#' look, you\'ll need to get ahead of it.
#' 
#' Scanning the area, you discover that the entire field you\'re standing
#' on is [densely packed with
#' pipes]{title="Manufactured by Hamilton and Hilbert Pipe Company"}; it
#' was hard to tell at first because they\'re the same metallic silver
#' color as the \"ground\". You make a quick sketch of all of the surface
#' pipes you can see (your puzzle input).
#' 
#' The pipes are arranged in a two-dimensional grid of *tiles*:
#' 
#' -   `|` is a *vertical pipe* connecting north and south.
#' -   `-` is a *horizontal pipe* connecting east and west.
#' -   `L` is a *90-degree bend* connecting north and east.
#' -   `J` is a *90-degree bend* connecting north and west.
#' -   `7` is a *90-degree bend* connecting south and west.
#' -   `F` is a *90-degree bend* connecting south and east.
#' -   `.` is *ground*; there is no pipe in this tile.
#' -   `S` is the *starting position* of the animal; there is a pipe on
#'     this tile, but your sketch doesn\'t show what shape the pipe has.
#' 
#' Based on the acoustics of the animal\'s scurrying, you\'re confident the
#' pipe that contains the animal is *one large, continuous loop*.
#' 
#' For example, here is a square loop of pipe:
#' 
#'     .....
#'     .F-7.
#'     .|.|.
#'     .L-J.
#'     .....
#' 
#' If the animal had entered this loop in the northwest corner, the sketch
#' would instead look like this:
#' 
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#' 
#' In the above diagram, the `S` tile is still a 90-degree `F` bend: you
#' can tell because of how the adjacent pipes connect to it.
#' 
#' Unfortunately, there are also many pipes that *aren\'t connected to the
#' loop*! This sketch shows the same loop as above:
#' 
#'     -L|F7
#'     7S-7|
#'     L|7||
#'     -L-J|
#'     L|-JF
#' 
#' In the above diagram, you can still figure out which pipes form the main
#' loop: they\'re the ones connected to `S`, pipes those pipes connect to,
#' pipes *those* pipes connect to, and so on. Every pipe in the main loop
#' connects to its two neighbors (including `S`, which will have exactly
#' two pipes connecting to it, and which is assumed to connect back to
#' those two pipes).
#' 
#' Here is a sketch that contains a slightly more complex main loop:
#' 
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#' 
#' Here\'s the same example sketch with the extra, non-main-loop pipe tiles
#' also shown:
#' 
#'     7-F7-
#'     .FJ|7
#'     SJLL7
#'     |F--J
#'     LJ.LJ
#' 
#' If you want to *get out ahead of the animal*, you should find the tile
#' in the loop that is *farthest* from the starting position. Because the
#' animal is in the pipe, it doesn\'t make sense to measure this by direct
#' distance. Instead, you need to find the tile that would take the longest
#' number of steps *along the loop* to reach from the starting point -
#' regardless of which way around the loop the animal went.
#' 
#' In the first example with the square loop:
#' 
#'     .....
#'     .S-7.
#'     .|.|.
#'     .L-J.
#'     .....
#' 
#' You can count the distance each tile in the loop is from the starting
#' point like this:
#' 
#'     .....
#'     .012.
#'     .1.3.
#'     .234.
#'     .....
#' 
#' In this example, the farthest point from the start is *`4`* steps away.
#' 
#' Here\'s the more complex loop again:
#' 
#'     ..F7.
#'     .FJ|.
#'     SJ.L7
#'     |F--J
#'     LJ...
#' 
#' Here are the distances for each tile on that loop:
#' 
#'     ..45.
#'     .236.
#'     01.78
#'     14567
#'     23...
#' 
#' Find the single giant loop starting at `S`. *How many steps along the
#' loop does it take to get from the starting position to the point
#' farthest from the starting position?*
#'
#' **Part Two**
#' 
#' You quickly reach the farthest point of the loop, but the animal never
#' emerges. Maybe its nest is *within the area enclosed by the loop*?
#' 
#' To determine whether it\'s even worth taking the time to search for such
#' a nest, you should calculate how many tiles are contained within the
#' loop. For example:
#' 
#'     ...........
#'     .S-------7.
#'     .|F-----7|.
#'     .||.....||.
#'     .||.....||.
#'     .|L-7.F-J|.
#'     .|..|.|..|.
#'     .L--J.L--J.
#'     ...........
#' 
#' The above loop encloses merely *four tiles* - the two pairs of `.` in
#' the southwest and southeast (marked `I` below). The middle `.` tiles
#' (marked `O` below) are *not* in the loop. Here is the same loop again
#' with those regions marked:
#' 
#'     ...........
#'     .S-------7.
#'     .|F-----7|.
#'     .||OOOOO||.
#'     .||OOOOO||.
#'     .|L-7OF-J|.
#'     .|II|O|II|.
#'     .L--JOL--J.
#'     .....O.....
#' 
#' In fact, there doesn\'t even need to be a full tile path to the outside
#' for tiles to count as outside the loop - squeezing between pipes is also
#' allowed! Here, `I` is still within the loop and `O` is still outside the
#' loop:
#' 
#'     ..........
#'     .S------7.
#'     .|F----7|.
#'     .||OOOO||.
#'     .||OOOO||.
#'     .|L-7F-J|.
#'     .|II||II|.
#'     .L--JL--J.
#'     ..........
#' 
#' In both of the above examples, *`4`* tiles are enclosed by the loop.
#' 
#' Here\'s a larger example:
#' 
#'     .F----7F7F7F7F-7....
#'     .|F--7||||||||FJ....
#'     .||.FJ||||||||L7....
#'     FJL7L7LJLJ||LJ.L-7..
#'     L--J.L7...LJS7F-7L7.
#'     ....F-J..F7FJ|L7L7L7
#'     ....L7.F7||L7|.L7L7|
#'     .....|FJLJ|FJ|F7|.LJ
#'     ....FJL-7.||.||||...
#'     ....L---J.LJ.LJLJ...
#' 
#' The above sketch has many random bits of ground, some of which are in
#' the loop (`I`) and some of which are outside it (`O`):
#' 
#'     OF----7F7F7F7F-7OOOO
#'     O|F--7||||||||FJOOOO
#'     O||OFJ||||||||L7OOOO
#'     FJL7L7LJLJ||LJIL-7OO
#'     L--JOL7IIILJS7F-7L7O
#'     OOOOF-JIIF7FJ|L7L7L7
#'     OOOOL7IF7||L7|IL7L7|
#'     OOOOO|FJLJ|FJ|F7|OLJ
#'     OOOOFJL-7O||O||||OOO
#'     OOOOL---JOLJOLJLJOOO
#' 
#' In this larger example, *`8`* tiles are enclosed by the loop.
#' 
#' Any tile that isn\'t part of the main loop can count as being enclosed
#' by the loop. Here\'s another example with many bits of junk pipe lying
#' around that aren\'t connected to the main loop at all:
#' 
#'     FF7FSF7F7F7F7F7F---7
#'     L|LJ||||||||||||F--J
#'     FL-7LJLJ||||||LJL-77
#'     F--JF--7||LJLJ7F7FJ-
#'     L---JF-JLJ.||-FJLJJ7
#'     |F|F-JF---7F7-L7L|7|
#'     |FFJF7L7F-JF7|JL---7
#'     7-L-JL7||F7|L7F-7F7|
#'     L.L7LFJ|||||FJL7||LJ
#'     L7JLJL-JLJLJL--JLJ.L
#' 
#' Here are just the tiles that are *enclosed by the loop* marked with `I`:
#' 
#'     FF7FSF7F7F7F7F7F---7
#'     L|LJ||||||||||||F--J
#'     FL-7LJLJ||||||LJL-77
#'     F--JF--7||LJLJIF7FJ-
#'     L---JF-JLJIIIIFJLJJ7
#'     |F|F-JF---7IIIL7L|7|
#'     |FFJF7L7F-JF7IIL---7
#'     7-L-JL7||F7|L7F-7F7|
#'     L.L7LFJ|||||FJL7||LJ
#'     L7JLJL-JLJLJL--JLJ.L
#' 
#' In this last example, *`10`* tiles are enclosed by the loop.
#' 
#' Figure out whether you have time to search for the nest by calculating
#' the area within the loop. *How many tiles are enclosed by the loop?*
#'
#' @param x some data
#' @return For Part One, `f10a(x)` returns .... For Part Two,
#'   `f10b(x)` returns ....
#' @export
#' @examples
#' f10a(example_data_10())
#' f10b()
f10a <- function(x) {
  xx <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  s <- which(xx == "S", arr.ind = TRUE)
  visited <- list()
  dirs <- inputs(xx, s)
  for (di in seq_along(dirs)) {
    d <- dirs[[di]]
    j <- 0
    visited[[di]] <- cbind(as.data.frame(s), j = j)
    while (TRUE) {
      j <- j + 1
      visited[[di]] <- rbind(visited[[di]], c(d, j))
      nextdirs <- as.data.frame(do.call(rbind, exits(xx, d)))
      colnames(nextdirs) <- c("row", "col")
      nextdirs <- dplyr::setdiff(nextdirs, visited[[di]][, 1:2])
      d <- unlist(nextdirs)
      if (!length(d)) {
        visited[[di]] <- rbind(visited[[di]], c(s, j+1))
        break
      }
    }
  }
  
  # make_plot(xx, visited)
  # ggsave("inst/vis10.png", height = 4500, width = 4500, units = "px")
  
  res <- merge(visited[[1]], visited[[2]], by = c("row", "col")) |> 
    dplyr::filter(row != s[1,1] & col != s[1,2]) |>
    dplyr::filter(j.x == j.y)
  res$j.x
}


#' @rdname day10
#' @export
f10b <- function(x) {
  xx <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  
  # xx <- cbind(rep(".", nrow(xx)), xx)
  # xx <- cbind(xx, rep(".", nrow(xx)))
  # xx <- rbind(xx, rep(".", ncol(xx)))
  # xx <- rbind(rep(".", ncol(xx)), xx)
  
  s <- which(xx == "S", arr.ind = TRUE)
  visited <- list()
  dirs <- inputs(xx, s)
  for (di in 1) { # only loop one way
    d <- dirs[[di]]
    j <- 0
    visited[[di]] <- cbind(as.data.frame(s), j = j)
    while (TRUE) {
      j <- j + 1
      visited[[di]] <- rbind(visited[[di]], c(d, j))
      nextdirs <- as.data.frame(do.call(rbind, exits(xx, d)))
      colnames(nextdirs) <- c("row", "col")
      nextdirs <- dplyr::setdiff(nextdirs, visited[[di]][, 1:2])
      d <- unlist(nextdirs)
      if (!length(d)) {
        visited[[di]] <- rbind(visited[[di]], c(s, j+1))
        break
      }
    }
  }
  loop <- unique(visited[[1]][, 1:2]) 

  # pos <- c(1, 1)
  # flood <- data.frame(row = pos[1], col = pos[2])
  # filled <- unique(floodfill(xx, pos, loop, flood))

  yy <- xx
  nonloop <- dplyr::setdiff(as.data.frame(expand.grid(row = 1:nrow(xx), col = 1:ncol(xx))), loop)
  yy[as.matrix(nonloop)] <- "."
  z <- 0
  for (i in 1:nrow(yy)) {
    inside <- FALSE
    for (j in 1:ncol(yy)) {
      # if (yy[i, j] %in% c("|", "F", "J", "L", "7")) {
      if (yy[i, j] %in% c("|", "J", "L")) {
        inside <- !inside        
      } else if (yy[i, j] == "." && inside) {
        z <- z + 1
      }
    }
  }
  z  
  # z <- dplyr::setdiff(as.data.frame(expand.grid(row = 1:nrow(xx), col = 1:ncol(xx))), loop) |> 
  # # z <- dplyr::setdiff(z, loop) |> 
  #   dplyr::filter(row != 1, row != nrow(xx), col != 1, col != ncol(xx)) |> 
  #   dplyr::rowwise() |> 
  #   dplyr::mutate(inside = inside_loop(xx, c(row, col), loop)) |> 
  #   dplyr::filter(inside)
  # nrow(z)
}

# floodfill <- function(m, pos, loop, flood) {
#   res <- dplyr::setdiff(nbrs(m, pos[1], pos[2]), loop)
#   res <- dplyr::setdiff(res, flood)
#   flood <- unique(rbind(flood, res))
#   if (nrow(res) == 0) return(flood)
#   for (i in seq_len(nrow(res))) {
#     flood <- rbind(flood, floodfill(m, unlist(res[i, ]), loop, flood))
#   }
#   flood
# }

pt_in_loop <- function(r, c, loop) {
  nrow(dplyr::filter(loop, row == r & col == c)) > 0
}

inside_loop <- function(m, pos, loop) {
  vert_loop <- c("|", "F", "J", "L", "7")
  horiz_loop <- c("-", "F", "J", "L", "7")
  n <- e <- s <- w <- 0
  # for (i in (pos[1] - 1):1) {
  #   w <- w + ((m[i, pos[2]] %in% horiz_loop) && pt_in_loop(i, pos[2], loop))
  # }
  # if (w %% 2 == 0) return(FALSE)
  # for (i in (pos[1]+1):nrow(m)) {
  for (i in 1:nrow(m)) {
    e <- e + ((m[i, pos[2]] %in% horiz_loop) && pt_in_loop(i, pos[2], loop))
  }
  if (e %% 2 == 0) return(FALSE)
  # for (i in (pos[2]-1):1) {
  #   n <- n + ((m[pos[1], i] %in% vert_loop) && pt_in_loop(pos[1], i, loop))
  # }
  # if (n %% 2 == 0) return(FALSE)
  # for (i in (pos[2]+1):ncol(m)) {
  # for (i in 1:ncol(m)) {
  #   s <- s + ((m[pos[1], i] %in% vert_loop) && pt_in_loop(pos[1], i, loop))
  # }
  # if (s %% 2 == 0) return(FALSE)
  TRUE
}

nbrs <- function(m, r, c) {
  tibble::tribble(~row, ~col,
                  r - 1, c,
                  r + 1, c,
                  r, c - 1,
                  r, c + 1) |> 
    dplyr::filter(row > 0 & row <= nrow(m)) |> 
    dplyr::filter(col > 0 & col <= ncol(m)) 
}

inputs <- function(m, pos) {
  res <- list()
  
  if (pos[1] - 1 > 0) {
    if (m[pos[1] - 1, pos[2]] %in% c("|", "F", "7")) res <- c(res, list(c(pos[1] - 1, pos[2])))
  }
  if (pos[1] + 1 <= nrow(m)) {
    if (m[pos[1] + 1, pos[2]] %in% c("|", "J", "L")) res <- c(res, list(c(pos[1] + 1, pos[2])))
  }
  if (pos[2] - 1 > 0) {
    if (m[pos[1], pos[2] - 1] %in% c("-", "F", "L")) res <- c(res, list(c(pos[1], pos[2] - 1)))
  }
  if (pos[2] + 1 <= ncol(m)) {
    if (m[pos[1], pos[2] + 1] %in% c("-", "J", "7")) res <- c(res, list(c(pos[1], pos[2] + 1)))
  }
  
  res
}

exits <- function(m, pos) {
  switch(m[pos[1], pos[2]], 
         "F" = list(c(pos[1] + 1, pos[2]), c(pos[1], pos[2] + 1)),
         "-" = list(c(pos[1], pos[2] - 1), c(pos[1], pos[2] + 1)),
         "|" = list(c(pos[1] - 1, pos[2]), c(pos[1] + 1, pos[2])),
         "J" = list(c(pos[1], pos[2] - 1), c(pos[1] - 1, pos[2])),
         "L" = list(c(pos[1] - 1, pos[2]), c(pos[1], pos[2] + 1)),
         "7" = list(c(pos[1], pos[2] - 1), c(pos[1] + 1, pos[2])),
  )
}

make_plot <- function(m, loops) {
  library(ggplot2)
  
  message("aspect: ", ncol(m), " by ", nrow(m))
  
  mm <- data.frame(
    i = rev(c(t(row(m)))),
    j = c(t(col(m))),
    v = c(t(m))
  )
  s <- dplyr::filter(mm, v == "S")
  s$v <- sub("S", "🮽", s$v)
  
  mm$v <- gsub("F", "╔", mm$v)
  mm$v <- gsub("7", "╗", mm$v)
  mm$v <- gsub("J", "╝", mm$v)
  mm$v <- gsub("L", "╚", mm$v)
  mm$v <- gsub("\\|", "║", mm$v)
  mm$v <- gsub("\\-", "═", mm$v)
  mm$v <- gsub("\\.", " ", mm$v)
  mm$v <- gsub("S", " ", mm$v)

  yy <- m
  nonloop <- dplyr::setdiff(as.data.frame(expand.grid(row = 1:nrow(yy), col = 1:ncol(yy))), rbind(loops[[1]], loops[[2]])[, 1:2])
  yy[as.matrix(nonloop)] <- "."
  
  internals <- data.frame(row = numeric(), col = numeric())
  
  for (i in 1:nrow(yy)) {
    inside <- FALSE
    for (j in 1:ncol(yy)) 
      if (yy[i, j] %in% c("|", "J", "L")) {
        inside <- !inside        
      } else if (yy[i, j] == "." && inside) {
        internals <- rbind(internals, data.frame(row = i, col = j))
      }
  }
  
  loops[[1]]$row <- nrow(m) - loops[[1]]$row + 1
  loops[[2]]$row <- nrow(m) - loops[[2]]$row + 1
  internals$row <- nrow(m) - internals$row + 1
  
  furthest <- merge(loops[[1]], loops[[2]], by = c("row", "col")) |> 
    dplyr::filter(row != s[1,1] & col != s[1,2]) |>
    dplyr::filter(j.x == j.y)
  
  ll1 <- dplyr::left_join(loops[[1]], mm, by = c(row = "i", col = "j")) |> dplyr::filter(j < furthest$j.x)
  ll2 <- dplyr::left_join(loops[[2]], mm, by = c(row = "i", col = "j")) |> dplyr::filter(j < furthest$j.y)
  
  sizescale <- 1
  
  ggplot(mm) +
    geom_text(aes(factor(j), factor(i), label = v), col = "grey90", size = 4*sizescale) +
    geom_text(data = ll1, aes(factor(col), factor(row), label = v, col = j), size = 4*sizescale) +
    geom_text(data = ll2, aes(factor(col), factor(row), label = v, col = j), size = 4*sizescale) +
    scale_color_gradient2(low = "blue", mid = "orange", midpoint = furthest$j.x / 2, high = "red") +
    geom_text(data = internals, aes(factor(col), factor(row)), label = "█", col = "forestgreen", size = 2*sizescale) +
    geom_text(data = s, aes(factor(j), factor(i), label = v), size = 4*sizescale, col = "blue") +
    geom_text(data = as.data.frame(furthest), aes(factor(col), factor(row)), label = "🮽", size = 4*sizescale, col = "red") +
    theme_void() +
    guides(col = FALSE) +
    theme(aspect.ratio = 1, panel.background = element_rect(fill = "black"))
  
}

f10_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
