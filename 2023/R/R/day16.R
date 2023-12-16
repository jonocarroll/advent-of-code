#' Day 16: The Floor Will Be Lava
#'
#' [The Floor Will Be Lava](https://adventofcode.com/2023/day/16)
#'
#' @name day16
#' @rdname day16
#' @details
#'
#' **Part One**
#'
#' With the beam of light completely focused *somewhere*, the reindeer
#' leads you deeper still into the Lava Production Facility. At some point,
#' you realize that the steel facility walls have been replaced with cave,
#' and the doorways are just cave, and the floor is cave, and you\'re
#' pretty sure this is actually just a giant cave.
#' 
#' Finally, as you approach what must be the heart of the mountain, you see
#' a bright light in a cavern up ahead. There, you discover that the
#' [beam]{title="Not anymore, there's a blanket!"} of light you so
#' carefully focused is emerging from the cavern wall closest to the
#' facility and pouring all of its energy into a contraption on the
#' opposite side.
#' 
#' Upon closer inspection, the contraption appears to be a flat,
#' two-dimensional square grid containing *empty space* (`.`), *mirrors*
#' (`/` and `\`), and *splitters* (`|` and `-`).
#' 
#' The contraption is aligned so that most of the beam bounces around the
#' grid, but each tile on the grid converts some of the beam\'s light into
#' *heat* to melt the rock in the cavern.
#' 
#' You note the layout of the contraption (your puzzle input). For example:
#' 
#'     .|...\....
#'     |.-.\.....
#'     .....|-...
#'     ........|.
#'     ..........
#'     .........\
#'     ..../.\\..
#'     .-.-/..|..
#'     .|....-|.\
#'     ..//.|....
#' 
#' The beam enters in the top-left corner from the left and heading to the
#' *right*. Then, its behavior depends on what it encounters as it moves:
#' 
#' -   If the beam encounters *empty space* (`.`), it continues in the same
#'     direction.
#' -   If the beam encounters a *mirror* (`/` or `\`), the beam is
#'     *reflected* 90 degrees depending on the angle of the mirror. For
#'     instance, a rightward-moving beam that encounters a `/` mirror would
#'     continue *upward* in the mirror\'s column, while a rightward-moving
#'     beam that encounters a `\` mirror would continue *downward* from the
#'     mirror\'s column.
#' -   If the beam encounters the *pointy end of a splitter* (`|` or `-`),
#'     the beam passes through the splitter as if the splitter were *empty
#'     space*. For instance, a rightward-moving beam that encounters a `-`
#'     splitter would continue in the same direction.
#' -   If the beam encounters the *flat side of a splitter* (`|` or `-`),
#'     the beam is *split into two beams* going in each of the two
#'     directions the splitter\'s pointy ends are pointing. For instance, a
#'     rightward-moving beam that encounters a `|` splitter would split
#'     into two beams: one that continues *upward* from the splitter\'s
#'     column and one that continues *downward* from the splitter\'s
#'     column.
#' 
#' Beams do not interact with other beams; a tile can have many beams
#' passing through it at the same time. A tile is *energized* if that tile
#' has at least one beam pass through it, reflect in it, or split in it.
#' 
#' In the above example, here is how the beam of light bounces around the
#' contraption:
#' 
#'     >|<<<\....
#'     |v-.\^....
#'     .v...|->>>
#'     .v...v^.|.
#'     .v...v^...
#'     .v...v^..\
#'     .v../2\\..
#'     <->-/vv|..
#'     .|<<<2-|.\
#'     .v//.|.v..
#' 
#' Beams are only shown on empty tiles; arrows indicate the direction of
#' the beams. If a tile contains beams moving in multiple directions, the
#' number of distinct directions is shown instead. Here is the same diagram
#' but instead only showing whether a tile is *energized* (`#`) or not
#' (`.`):
#' 
#'     ######....
#'     .#...#....
#'     .#...#####
#'     .#...##...
#'     .#...##...
#'     .#...##...
#'     .#..####..
#'     ########..
#'     .#######..
#'     .#...#.#..
#' 
#' Ultimately, in this example, *`46`* tiles become *energized*.
#' 
#' The light isn\'t energizing enough tiles to produce lava; to debug the
#' contraption, you need to start by analyzing the current situation. With
#' the beam starting in the top-left heading right, *how many tiles end up
#' being energized?*
#'
#' **Part Two**
#' 
#' As you try to work out what might be wrong, the reindeer tugs on your
#' shirt and leads you to a nearby control panel. There, a collection of
#' buttons lets you align the contraption so that the beam enters from *any
#' edge tile* and heading away from that edge. (You can choose either of
#' two directions for the beam if it starts on a corner; for instance, if
#' the beam starts in the bottom-right corner, it can start heading either
#' left or upward.)
#' 
#' So, the beam could start on any tile in the top row (heading downward),
#' any tile in the bottom row (heading upward), any tile in the leftmost
#' column (heading right), or any tile in the rightmost column (heading
#' left). To produce lava, you need to find the configuration that
#' *energizes as many tiles as possible*.
#' 
#' In the above example, this can be achieved by starting the beam in the
#' fourth tile from the left in the top row:
#' 
#'     .|<2<\....
#'     |v-v\^....
#'     .v.v.|->>>
#'     .v.v.v^.|.
#'     .v.v.v^...
#'     .v.v.v^..\
#'     .v.v/2\\..
#'     <-2-/vv|..
#'     .|<<<2-|.\
#'     .v//.|.v..
#' 
#' Using this configuration, *`51`* tiles are energized:
#' 
#'     .#####....
#'     .#.#.#....
#'     .#.#.#####
#'     .#.#.##...
#'     .#.#.##...
#'     .#.#.##...
#'     .#.#####..
#'     ########..
#'     .#######..
#'     .#...#.#..
#' 
#' Find the initial beam configuration that energizes the largest number of
#' tiles; *how many tiles are energized in that configuration?*
#'
#' @param x some data
#' @return For Part One, `f16a(x)` returns .... For Part Two,
#'   `f16b(x)` returns ....
#' @export
#' @examples
#' f16a(example_data_16())
#' f16b()
f16a <- function(x) {
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  queue <<- visited <<- data.frame(row = integer(), col = integer(), dir = character())
  queue <<- rbind(queue, data.frame(row = 1, col = 1, dir = "r"))
  ans <- 0
  while (nrow(queue) > 0) {
    nextpos <- tail(queue, 1)
    queue <<- head(queue, -1)
    queue <<- dplyr::setdiff(queue, visited)
    ans <- ans + beam(x, c(nextpos$row, nextpos$col), nextpos$dir)
  }
  ans
}

beam <- function(m, pos, dir) {
  dirpos <- switch(dir, 
                "l" = c(0, -1),
                "r" = c(0, 1),
                "u" = c(-1, 0),
                "d" = c(1, 0))
  if (pos[1] < 1 | pos[1] > nrow(m) | 
      pos[2] < 1 | pos[2] > ncol(m)) {
    return(0)
  }
  if (nrow(visited[visited$row == pos[1] & visited$col == pos[2] & visited$dir == dir, ]) > 0) {
    return(0)
  }
  if (nrow(visited[visited$row == pos[1] & visited$col == pos[2], ]) == 0) {
    energized <- 1
  } else {
    energized <- 0
  }
  here <- m[pos[1], pos[2]]
  visited <<- rbind(visited, data.frame(row = pos[1], col = pos[2], dir = dir))
  if (here == ".") {
    queue <<- rbind(queue, data.frame(row = pos[1] + dirpos[1], 
                                      col = pos[2] + dirpos[2],
                                      dir = dir))
  } else if (here == "|" && dir %in% c("l", "r")) {
    for (n in c("u", "d")) {
      dirpos <- switch(n, 
                       "u" = c(-1, 0),
                       "d" = c(1, 0))
      queue <<- rbind(queue, data.frame(row = pos[1] + dirpos[1],
                                        col = pos[2] + dirpos[2], 
                                        dir = n))
    }
  } else if (here == "|" && dir %in% c("u", "d")) {
    queue <<- rbind(queue, data.frame(row = pos[1] + dirpos[1],
                                      col = pos[2] + dirpos[2], 
                                      dir = dir))
    
  } else if (here == "-" && dir %in% c("l", "r")) {
    queue <<- rbind(queue, data.frame(row = pos[1] + dirpos[1],
                                      col = pos[2] + dirpos[2], 
                                      dir = dir))
  } else if (here == "-" && dir %in% c("u", "d")) {
    for (n in c("l", "r")) {
      dirpos <- switch(n, 
                       "l" = c(0, -1),
                       "r" = c(0, 1))
      queue <<- rbind(queue, data.frame(row = pos[1] + dirpos[1],
                                        col = pos[2] + dirpos[2], 
                                        dir = n))
    }
  } else if (here == "\\") {
    newpos <- switch(dir, 
                     "l" = pos + c(-1, 0),
                     "r" = pos + c(1, 0),
                     "u" = pos + c(0, -1),
                     "d" = pos + c(0, 1))
    newdir <- switch(dir, 
                     "l" = "u",
                     "r" = "d",
                     "u" = "l", 
                     "d" = "r")
    queue <<- rbind(queue, data.frame(row = newpos[1],
                                      col = newpos[2], 
                                      dir = newdir))
    
  } else if (here == "/") {
    newpos <- switch(dir, 
                     "l" = pos + c(1, 0),
                     "r" = pos + c(-1, 0),
                     "u" = pos + c(0, 1),
                     "d" = pos + c(0, -1))
    newdir <- switch(dir, 
                     "l" = "d",
                     "r" = "u",
                     "u" = "r", 
                     "d" = "l")
    queue <<- rbind(queue, data.frame(row = newpos[1],
                                      col = newpos[2], 
                                      dir = newdir))
  }
  energized
}

  
#' @rdname day16
#' @export
f16b <- function(x) {
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  down <- expand.grid(row = 1, col = 1:ncol(x), dir = "d", stringsAsFactors = FALSE)
  up <- expand.grid(row = nrow(x), col = 1:ncol(x), dir = "u", stringsAsFactors = FALSE)
  right <- expand.grid(row = 1:nrow(x), col = 1, dir = "r", stringsAsFactors = FALSE)
  left <- expand.grid(row = 1:nrow(x), col = ncol(x), dir = "d", stringsAsFactors = FALSE)
  edges <- do.call(rbind, list(up, down, left, right))
  bestans <- 0
  library(future)
  plan(multisession)
  all_ans <- furrr::future_map_int(seq_len(nrow(edges)), \(z) f16_helper(x, edges, z))
  max(all_ans)
}


f16_helper <- function(x, edges, e) {
  queue <<- visited <<- data.frame(row = integer(), col = integer(), dir = character())
  queue <<- rbind(queue, data.frame(row = edges[e, 1], col = edges[e, 2], dir = edges[e, ]$dir))
  ans <- 0
  while (nrow(queue) > 0) {
    nextpos <- tail(queue, 1)
    queue <<- head(queue, -1)
    queue <<- dplyr::setdiff(queue, visited)
    ans <- ans + beam(x, c(nextpos$row, nextpos$col), nextpos$dir)
  }
 ans
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
