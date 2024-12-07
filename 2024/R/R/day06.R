#' Day 06: Guard Gallivant
#'
#' [Guard Gallivant](https://adventofcode.com/2024/day/6)
#'
#' @name day06
#' @rdname day06
#' @details
#'
#' **Part One**
#'
#' The Historians use their fancy [device](4) again, this time to whisk you
#' all away to the North Pole prototype suit manufacturing lab\... in the
#' year [1518](/2018/day/5)! It turns out that having direct access to
#' history is very convenient for a group of historians.
#' 
#' You still have to be careful of time paradoxes, and so it will be
#' important to avoid anyone from 1518 while The Historians search for the
#' Chief. Unfortunately, a single *guard* is patrolling this part of the
#' lab.
#' 
#' Maybe you can work out where the guard will go ahead of time so that The
#' Historians can search safely?
#' 
#' You start by making a map (your puzzle input) of the situation. For
#' example:
#' 
#'     ....#.....
#'     .........#
#'     ..........
#'     ..#.......
#'     .......#..
#'     ..........
#'     .#..^.....
#'     ........#.
#'     #.........
#'     ......#...
#' 
#' The map shows the current position of the guard with `^` (to indicate
#' the guard is currently facing *up* from the perspective of the map). Any
#' *obstructions* - crates, desks, alchemical reactors, etc. - are shown as
#' `#`.
#' 
#' Lab guards in 1518 follow a very strict patrol protocol which involves
#' repeatedly following these steps:
#' 
#' -   If there is something directly in front of you, turn right 90
#'     degrees.
#' -   Otherwise, take a step forward.
#' 
#' Following the above protocol, the guard moves up several times until she
#' reaches an obstacle (in this case, a pile of failed suit prototypes):
#' 
#'     ....#.....
#'     ....^....#
#'     ..........
#'     ..#.......
#'     .......#..
#'     ..........
#'     .#........
#'     ........#.
#'     #.........
#'     ......#...
#' 
#' Because there is now an obstacle in front of the guard, she turns right
#' before continuing straight in her new facing direction:
#' 
#'     ....#.....
#'     ........>#
#'     ..........
#'     ..#.......
#'     .......#..
#'     ..........
#'     .#........
#'     ........#.
#'     #.........
#'     ......#...
#' 
#' Reaching another obstacle (a spool of several *very* long polymers), she
#' turns right again and continues downward:
#' 
#'     ....#.....
#'     .........#
#'     ..........
#'     ..#.......
#'     .......#..
#'     ..........
#'     .#......v.
#'     ........#.
#'     #.........
#'     ......#...
#' 
#' This process continues for a while, but the guard eventually leaves the
#' mapped area (after walking past a tank of universal solvent):
#' 
#'     ....#.....
#'     .........#
#'     ..........
#'     ..#.......
#'     .......#..
#'     ..........
#'     .#........
#'     ........#.
#'     #.........
#'     ......#v..
#' 
#' By predicting the guard\'s route, you can determine which specific
#' positions in the lab will be in the patrol path. *Including the guard\'s
#' starting position*, the positions visited by the guard before leaving
#' the area are marked with an `X`:
#' 
#'     ....#.....
#'     ....XXXXX#
#'     ....X...X.
#'     ..#.X...X.
#'     ..XXXXX#X.
#'     ..X.X.X.X.
#'     .#XXXXXXX.
#'     .XXXXXXX#.
#'     #XXXXXXX..
#'     ......#X..
#' 
#' In this example, the guard will visit *`41`* distinct positions on your
#' map.
#' 
#' Predict the path of the guard. *How many distinct positions will the
#' guard visit before leaving the mapped area?*
#'
#' **Part Two**
#'
#' While The Historians begin working around the guard\'s patrol route, you
#' borrow their fancy device and step outside the lab. From the safety of a
#' supply closet, you time travel through the last few months and
#' [record](/2018/day/4) the nightly status of the lab\'s guard post on the
#' walls of the closet.
#' 
#' Returning after what seems like only a few seconds to The Historians,
#' they explain that the guard\'s patrol area is simply too large for them
#' to safely search the lab without getting caught.
#' 
#' Fortunately, they are *pretty sure* that adding a single new obstruction
#' *won\'t* cause a time paradox. They\'d like to place the new obstruction
#' in such a way that the guard will get [*stuck in a
#' loop*]{title="This vulnerability was later fixed by having the guard always turn left instead."},
#' making the rest of the lab safe to search.
#' 
#' To have the lowest chance of creating a time paradox, The Historians
#' would like to know *all* of the possible positions for such an
#' obstruction. The new obstruction can\'t be placed at the guard\'s
#' starting position - the guard is there right now and would notice.
#' 
#' In the above example, there are only *`6`* different positions where a
#' new obstruction would cause the guard to get stuck in a loop. The
#' diagrams of these six situations use `O` to mark the new obstruction,
#' `|` to show a position where the guard moves up/down, `-` to show a
#' position where the guard moves left/right, and `+` to show a position
#' where the guard moves both up/down and left/right.
#' 
#' Option one, put a printing press next to the guard\'s starting position:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ....|..#|.
#'     ....|...|.
#'     .#.O^---+.
#'     ........#.
#'     #.........
#'     ......#...
#' 
#' Option two, put a stack of failed suit prototypes in the bottom right
#' quadrant of the mapped area:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ..+-+-+#|.
#'     ..|.|.|.|.
#'     .#+-^-+-+.
#'     ......O.#.
#'     #.........
#'     ......#...
#' 
#' Option three, put a crate of chimney-squeeze prototype fabric next to
#' the standing desk in the bottom right quadrant:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ..+-+-+#|.
#'     ..|.|.|.|.
#'     .#+-^-+-+.
#'     .+----+O#.
#'     #+----+...
#'     ......#...
#' 
#' Option four, put an alchemical retroencabulator near the bottom left
#' corner:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ..+-+-+#|.
#'     ..|.|.|.|.
#'     .#+-^-+-+.
#'     ..|...|.#.
#'     #O+---+...
#'     ......#...
#' 
#' Option five, put the alchemical retroencabulator a bit to the right
#' instead:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ..+-+-+#|.
#'     ..|.|.|.|.
#'     .#+-^-+-+.
#'     ....|.|.#.
#'     #..O+-+...
#'     ......#...
#' 
#' Option six, put a tank of sovereign glue right next to the tank of
#' universal solvent:
#' 
#'     ....#.....
#'     ....+---+#
#'     ....|...|.
#'     ..#.|...|.
#'     ..+-+-+#|.
#'     ..|.|.|.|.
#'     .#+-^-+-+.
#'     .+----++#.
#'     #+----++..
#'     ......#O..
#' 
#' It doesn\'t really matter what you choose to use as an obstacle so long
#' as you and The Historians can put it into position without the guard
#' noticing. The important thing is having enough options that you can find
#' one that minimizes time paradoxes, and in this example, there are *`6`*
#' different positions you could choose.
#' 
#' You need to get the guard stuck in a loop by adding a single new
#' obstruction. *How many different positions could you choose for this
#' obstruction?*
#'
#' @param x some data
#' @return For Part One, `f06a(x)` returns .... For Part Two,
#'   `f06b(x)` returns ....
#' @export
#' @examples
#' f06a(example_data_06())
#' f06b()
f06a <- function() {
  x <- readLines("inst/input06.txt")
  # x <- readLines("../tmp.txt")
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  start <- which(x == "^", arr.ind = T)
  pos <- start
  blocks <- as.data.frame(which(x == "#", arr.ind = T))
  blocks$str <- apply(blocks, 1, \(x) paste0(x, collapse = "|"))
  dir <- c(-1, 0)
  allpos <- data.frame(r = integer(), c = integer())
  nr <- nrow(x)
  nc <- ncol(x)
  while (inbounds(nr, nc, pos)) {
    if (hit_block(blocks, pos + dir)) {
      dir <- turn_right(dir)
    } else {
      allpos <- rbind(allpos, pos)
      pos <- pos + dir
    }
  }
  nrow(unique(allpos))
}

turn_right <- function(d) {
  if (d[1] == 0 & d[2] == 1) return(c(1, 0))
  if (d[1] == 1 & d[2] == 0) return(c(0, -1))
  if (d[1] == 0 & d[2] == -1) return(c(-1, 0))
  if (d[1] == -1 & d[2] == 0) return(c(0, 1))
}

hit_block <- function(m, p) {
  paste0(p, collapse = "|") %in% m$str
  # nrow(dplyr::filter(m, .data$row == p[1] & .data$col == p[2])) > 0
}

inbounds <- function(nr, nc, x) {
  x[1] > 0 & x[2] > 0 & x[1] <= nr & x[2] <= nc
}

#' @rdname day06
#' @export
f06b <- function() {
  x <- readLines("inst/input06.txt")
  # x <- readLines("../tmp.txt")
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  start <- which(x == "^", arr.ind = T)
  pos <- start
  blocks <- as.data.frame(which(x == "#", arr.ind = T))
  blocks$str <- apply(blocks, 1, \(x) paste0(x, collapse = "|"))
  startdir <- c(-1, 0)
  dir <- startdir

  walkroute <- function(b, pos, dir) {
    b$seen <- 0
    nr <- nrow(x)
    nc <- ncol(x)
    while (inbounds(nr, nc, pos)) 
      if (hit_block(b, pos + dir)) {
        b[b$row == (pos+dir)[1] & b$col == (pos+dir)[2], "seen"] <- b[b$row == (pos+dir)[1] & b$col == (pos+dir)[2], "seen"] + 1
        if (any(b$seen > 4)) return(TRUE)
        dir <- turn_right(dir)
      } else {
        pos <- pos + dir
      }
    FALSE
  }
  
  seenpos <- data.frame(r = integer(1000), c = integer(1000))
  i <- 1
  while (inbounds(nrow(x), ncol(x), pos)) {
    seenpos[i, ] <- pos
    if (hit_block(blocks, pos + dir)) {
      dir <- turn_right(dir)
    } else {
      i <- i + 1
      pos <- pos + dir
    }
  }
  seenpos <- unique(seenpos[seenpos$r != 0, ])

  library(furrr)
  plan(multisession)
  ns <- furrr::future_map_lgl(seq_len(nrow(seenpos)), function(iz) {
  # ns <- purrr::map_lgl(seq_len(nrow(seenpos)), function(iz) {
    ir <- seenpos[iz, 1]
    ic <- seenpos[iz, 2]
    tmpblocks <- rbind(blocks, data.frame(row = ir, col = ic, str = paste0(c(ir, ic), collapse = "|")))
    return(walkroute(tmpblocks, start, startdir))
  })
  sum(ns)
}

# system.time(f06b())

f06_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day06
#' @export
example_data_06 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
