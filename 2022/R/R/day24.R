#' Day 24: Blizzard Basin
#'
#' [Blizzard Basin](https://adventofcode.com/2022/day/24)
#'
#' @name day24
#' @rdname day24
#' @details
#'
#' **Part One**
#'
#' With everything replanted for next year (and with elephants and monkeys
#' to tend the grove), you and the Elves leave for the extraction point.
#'
#' Partway up the mountain that shields the grove is a flat, open area that
#' serves as the extraction point. It\'s a bit of a climb, but nothing the
#' expedition can\'t handle.
#'
#' At least, that would normally be true; now that the mountain is covered
#' in snow, things have become more difficult than the Elves are used to.
#'
#' As the expedition reaches a valley that must be traversed to reach the
#' extraction site, you find that strong, turbulent winds are pushing small
#' *blizzards* of snow and sharp ice around the valley. It\'s a good thing
#' everyone packed warm clothes! To make it across safely, you\'ll need to
#' find a way to avoid them.
#'
#' Fortunately, it\'s easy to see all of this from the entrance to the
#' valley, so you make a map of the valley and the blizzards (your puzzle
#' input). For example:
#'
#'     #.#####
#'     #.....#
#'     #>....#
#'     #.....#
#'     #...v.#
#'     #.....#
#'     #####.#
#'
#' The walls of the valley are drawn as `#`; everything else is ground.
#' Clear ground - where there is currently no blizzard - is drawn as `.`.
#' Otherwise, blizzards are drawn with an arrow indicating their direction
#' of motion: up (`^`), down (`v`), left (`<`), or right (`>`).
#'
#' The above map includes two blizzards, one moving right (`>`) and one
#' moving down (`v`). In one minute, each blizzard moves one position in
#' the direction it is pointing:
#'
#'     #.#####
#'     #.....#
#'     #.>...#
#'     #.....#
#'     #.....#
#'     #...v.#
#'     #####.#
#'
#' Due to [conservation of blizzard
#' energy]{title="I think, anyway. Do I look like a theoretical blizzacist?"},
#' as a blizzard reaches the wall of the valley, a new blizzard forms on
#' the opposite side of the valley moving in the same direction. After
#' another minute, the bottom downward-moving blizzard has been replaced
#' with a new downward-moving blizzard at the top of the valley instead:
#'
#'     #.#####
#'     #...v.#
#'     #..>..#
#'     #.....#
#'     #.....#
#'     #.....#
#'     #####.#
#'
#' Because blizzards are made of tiny snowflakes, they pass right through
#' each other. After another minute, both blizzards temporarily occupy the
#' same position, marked `2`:
#'
#'     #.#####
#'     #.....#
#'     #...2.#
#'     #.....#
#'     #.....#
#'     #.....#
#'     #####.#
#'
#' After another minute, the situation resolves itself, giving each
#' blizzard back its personal space:
#'
#'     #.#####
#'     #.....#
#'     #....>#
#'     #...v.#
#'     #.....#
#'     #.....#
#'     #####.#
#'
#' Finally, after yet another minute, the rightward-facing blizzard on the
#' right is replaced with a new one on the left facing the same direction:
#'
#'     #.#####
#'     #.....#
#'     #>....#
#'     #.....#
#'     #...v.#
#'     #.....#
#'     #####.#
#'
#' This process repeats at least as long as you are observing it, but
#' probably forever.
#'
#' Here is a more complex example:
#'
#'     #.######
#'     #>>.<^<#
#'     #.<..<<#
#'     #>v.><>#
#'     #<^v^^>#
#'     ######.#
#'
#' Your expedition begins in the only non-wall position in the top row and
#' needs to reach the only non-wall position in the bottom row. On each
#' minute, you can *move* up, down, left, or right, or you can *wait* in
#' place. You and the blizzards act *simultaneously*, and you cannot share
#' a position with a blizzard.
#'
#' In the above example, the fastest way to reach your goal requires *`18`*
#' steps. Drawing the position of the expedition as `E`, one way to achieve
#' this is:
#'
#'     Initial state:
#'     #E######
#'     #>>.<^<#
#'     #.<..<<#
#'     #>v.><>#
#'     #<^v^^>#
#'     ######.#
#'
#'     Minute 1, move down:
#'     #.######
#'     #E>3.<.#
#'     #<..<<.#
#'     #>2.22.#
#'     #>v..^<#
#'     ######.#
#'
#'     Minute 2, move down:
#'     #.######
#'     #.2>2..#
#'     #E^22^<#
#'     #.>2.^>#
#'     #.>..<.#
#'     ######.#
#'
#'     Minute 3, wait:
#'     #.######
#'     #<^<22.#
#'     #E2<.2.#
#'     #><2>..#
#'     #..><..#
#'     ######.#
#'
#'     Minute 4, move up:
#'     #.######
#'     #E<..22#
#'     #<<.<..#
#'     #<2.>>.#
#'     #.^22^.#
#'     ######.#
#'
#'     Minute 5, move right:
#'     #.######
#'     #2Ev.<>#
#'     #<.<..<#
#'     #.^>^22#
#'     #.2..2.#
#'     ######.#
#'
#'     Minute 6, move right:
#'     #.######
#'     #>2E<.<#
#'     #.2v^2<#
#'     #>..>2>#
#'     #<....>#
#'     ######.#
#'
#'     Minute 7, move down:
#'     #.######
#'     #.22^2.#
#'     #<vE<2.#
#'     #>>v<>.#
#'     #>....<#
#'     ######.#
#'
#'     Minute 8, move left:
#'     #.######
#'     #.<>2^.#
#'     #.E<<.<#
#'     #.22..>#
#'     #.2v^2.#
#'     ######.#
#'
#'     Minute 9, move up:
#'     #.######
#'     #<E2>>.#
#'     #.<<.<.#
#'     #>2>2^.#
#'     #.v><^.#
#'     ######.#
#'
#'     Minute 10, move right:
#'     #.######
#'     #.2E.>2#
#'     #<2v2^.#
#'     #<>.>2.#
#'     #..<>..#
#'     ######.#
#'
#'     Minute 11, wait:
#'     #.######
#'     #2^E^2>#
#'     #<v<.^<#
#'     #..2.>2#
#'     #.<..>.#
#'     ######.#
#'
#'     Minute 12, move down:
#'     #.######
#'     #>>.<^<#
#'     #.<E.<<#
#'     #>v.><>#
#'     #<^v^^>#
#'     ######.#
#'
#'     Minute 13, move down:
#'     #.######
#'     #.>3.<.#
#'     #<..<<.#
#'     #>2E22.#
#'     #>v..^<#
#'     ######.#
#'
#'     Minute 14, move right:
#'     #.######
#'     #.2>2..#
#'     #.^22^<#
#'     #.>2E^>#
#'     #.>..<.#
#'     ######.#
#'
#'     Minute 15, move right:
#'     #.######
#'     #<^<22.#
#'     #.2<.2.#
#'     #><2>E.#
#'     #..><..#
#'     ######.#
#'
#'     Minute 16, move right:
#'     #.######
#'     #.<..22#
#'     #<<.<..#
#'     #<2.>>E#
#'     #.^22^.#
#'     ######.#
#'
#'     Minute 17, move down:
#'     #.######
#'     #2.v.<>#
#'     #<.<..<#
#'     #.^>^22#
#'     #.2..2E#
#'     ######.#
#'
#'     Minute 18, move down:
#'     #.######
#'     #>2.<.<#
#'     #.2v^2<#
#'     #>..>2>#
#'     #<....>#
#'     ######E#
#'
#' *What is the fewest number of minutes required to avoid the blizzards
#' and reach the goal?*
#'
#' **Part Two**
#'
#' As the expedition reaches the far side of the valley, one of the Elves
#' looks especially dismayed:
#'
#' He *forgot his snacks* at the entrance to the valley!
#'
#' Since you\'re so good at dodging blizzards, the Elves humbly request
#' that you go back for his snacks. From the same initial conditions, how
#' quickly can you make it from the start to the goal, then back to the
#' start, then back to the goal?
#'
#' In the above example, the first trip to the goal takes `18` minutes, the
#' trip back to the start takes `23` minutes, and the trip back to the goal
#' again takes `13` minutes, for a total time of *`54`* minutes.
#'
#' *What is the fewest number of minutes required to reach the goal, go
#' back to the start, then reach the goal again?*
#'
#' @param x some data
#' @return For Part One, `f24a(x)` returns .... For Part Two,
#'   `f24b(x)` returns ....
#' @export
#' @examples
#' f24a(example_data_24())
#' f24b()
f24a <- function(x, v = NULL, start = c(1, 2), end = c(nrow(valley), ncol(valley)-1)) {
  if (is.null(v)) {
    valley <- readValley(x)
  } else {
    valley <- v
  }

  nextmoves <- matrix(c(start), ncol = 2)
  goal <- end
  t <- 0

  can_exit <- FALSE
  while (!can_exit) {
    t <- t + 1
    cat("t = ", t, "\n")
    valley <- tickBlizzard(valley)
    # printValley(valley)
    nmlist <- list()
    for (i in seq_len(nrow(nextmoves))) {
      nmlist[[i]] <- validMoves(valley, nextmoves[i, ])
    }
    nextmoves <- unique(do.call(rbind, nmlist))
    # message(nrow(nextmoves), " states to check")
    can_exit <- any(nextmoves[, 1] == goal[1] & nextmoves[, 2] == goal[2])
  }

  list(valley = valley, time = t )
}


#' @rdname day24
#' @export
f24b <- function(x, start, end) {
  tmp <- f24a(x)
  t1 <- tmp$time
  print(t1)
  valley <- tmp$valley
  # start = c(1, 2), end = c(nrow(valley), ncol(valley)-1)
  tmp <- f24a(v = valley, start = c(nrow(valley), ncol(valley)-1), end = c(1, 2))
  t2 <- tmp$time
  print(t2)
  valley <- tmp$valley
  tmp <- f24a(v = valley)
  t3 <- tmp$time
  print(t3)
  t1 + t2 + t3
}

validMoves <- function(valley, pos) {
  # up, down, left, right, still
  dirs <- matrix(c(-1, 0, 1, 0, 0, -1, 0, 1, 0, 0), ncol = 2, byrow = TRUE)
  for (d in 1:2) {
    dirs[, d] <- pos[d] + dirs[, d]
  }
  dirs <- dirs[which(dirs[, 1] > 0 & dirs[, 1] <= nrow(valley) &
                 dirs[, 2] > 0 & dirs[, 2] <= ncol(valley)), ]
  dirs[valley[dirs] == ".", ]
}

tickBlizzard <- function(valley) {

  bn <- which(apply(valley, c(1,2), \(x) grepl("^", x, fixed = TRUE)), arr.ind = TRUE)
  be <- which(apply(valley, c(1,2), \(x) grepl(">", x, fixed = TRUE)), arr.ind = TRUE)
  bs <- which(apply(valley, c(1,2), \(x) grepl("v", x)), arr.ind = TRUE)
  bw <- which(apply(valley, c(1,2), \(x) grepl("<", x, fixed = TRUE)), arr.ind = TRUE)

  bn[,1] <- (((bn[,1] - 1) - 2) %% (nrow(valley)-2)) + 2
  be[,2] <- (((be[,2] + 1) - 2) %% (ncol(valley)-2)) + 2
  bs[,1] <- (((bs[,1] + 1) - 2) %% (nrow(valley)-2)) + 2
  bw[,2] <- (((bw[,2] - 1) - 2) %% (ncol(valley)-2)) + 2

  valley[] <- "."
  valley[c(1, nrow(valley)), ] <- "#"
  valley[, c(1, ncol(valley))] <- "#"
  valley[matrix(c(1,2,nrow(valley), ncol(valley)-1), ncol = 2, byrow = TRUE)] <- "."
  valley[bn] <- paste0(sub("\\.", "", valley[bn]), "^")
  valley[be] <- paste0(sub("\\.", "", valley[be]), ">")
  valley[bs] <- paste0(sub("\\.", "", valley[bs]), "v")
  valley[bw] <- paste0(sub("\\.", "", valley[bw]), "<")

  valley

}

readValley <- function(x) {
  x = strsplit(x, "")
  valley <- matrix(nrow = length(x), ncol = length(x[[1]]))
  # # surround with an extra wall
  # valley[c(1,length(x) + 2), ] <- "#"
  for (i in seq_along(x)) {
    for (j in seq_along(x[[i]])) {
      valley[i, j] <- x[[i]][j]
    }
  }
  # valley[2:(length(x) - 1), 2:(length(x[[1]]) - 1)]
  valley
}

printValley <- function(x, pos = NULL) {
  if (!is.null(pos)) {
    x[pos] <- "E"
  }
  x <- apply(x, c(1, 2), \(y) ifelse(nchar(y)>1,nchar(y),y))
  for (i in seq_len(nrow(x))) {
    cat(x[i, ], "\n", sep = "")
  }
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day24
#' @export
example_data_24 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example24.txt")
  )
  l[[example]]
}
