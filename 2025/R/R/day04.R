#' Day 04: Printing Department
#'
#' [Printing Department](https://adventofcode.com/2025/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' You ride the escalator down to the printing department. They\'re clearly
#' getting ready for Christmas; they have lots of large rolls of paper
#' everywhere, and there\'s even a massive printer in the corner (to handle
#' the really [big]{title="This joke is stupid and I love it."} print
#' jobs).
#'
#' Decorating here will be easy: they can make their own decorations. What
#' you really need is a way to get further into the North Pole base while
#' the elevators are offline.
#'
#' \"Actually, maybe we can help with that,\" one of the Elves replies when
#' you ask for help. \"We\'re pretty sure there\'s a cafeteria on the other
#' side of the back wall. If we could break through the wall, you\'d be
#' able to keep moving. It\'s too bad all of our forklifts are so busy
#' moving those big rolls of paper around.\"
#'
#' If you can optimize the work the forklifts are doing, maybe they would
#' have time to spare to break through the wall.
#'
#' The rolls of paper (`@`) are arranged on a large grid; the Elves even
#' have a helpful diagram (your puzzle input) indicating where everything
#' is located.
#'
#' For example:
#'
#'     ..@@.@@@@.
#'     @@@.@.@.@@
#'     @@@@@.@.@@
#'     @.@@@@..@.
#'     @@.@@@@.@@
#'     .@@@@@@@.@
#'     .@.@.@.@@@
#'     @.@@@.@@@@
#'     .@@@@@@@@.
#'     @.@.@@@.@.
#'
#' The forklifts can only access a roll of paper if there are *fewer than
#' four rolls of paper* in the eight adjacent positions. If you can figure
#' out which rolls of paper the forklifts can access, they\'ll spend less
#' time looking and more time breaking down the wall to the cafeteria.
#'
#' In this example, there are *`13`* rolls of paper that can be accessed by
#' a forklift (marked with `x`):
#'
#'     ..xx.xx@x.
#'     x@@.@.@.@@
#'     @@@@@.x.@@
#'     @.@@@@..@.
#'     x@.@@@@.@x
#'     .@@@@@@@.@
#'     .@.@.@.@@@
#'     x.@@@.@@@@
#'     .@@@@@@@@.
#'     x.x.@@@.x.
#'
#' Consider your complete diagram of the paper roll locations. *How many
#' rolls of paper can be accessed by a forklift?*
#'
#' **Part Two**

#' Now, the Elves just need help accessing as much of the paper as they
#' can.
#'
#' Once a roll of paper can be accessed by a forklift, it can be *removed*.
#' Once a roll of paper is removed, the forklifts might be able to access
#' *more* rolls of paper, which they might also be able to remove. How many
#' total rolls of paper could the Elves remove if they keep repeating this
#' process?
#'
#' Starting with the same example as above, here is one way you could
#' remove as many rolls of paper as possible, using highlighted *`@`* to
#' indicate that a roll of paper is about to be removed, and using `x` to
#' indicate that a roll of paper was just removed:
#'
#'     Initial state:
#'     ..@@.@@@@.
#'     @@@.@.@.@@
#'     @@@@@.@.@@
#'     @.@@@@..@.
#'     @@.@@@@.@@
#'     .@@@@@@@.@
#'     .@.@.@.@@@
#'     @.@@@.@@@@
#'     .@@@@@@@@.
#'     @.@.@@@.@.
#'
#'     Remove 13 rolls of paper:
#'     ..xx.xx@x.
#'     x@@.@.@.@@
#'     @@@@@.x.@@
#'     @.@@@@..@.
#'     x@.@@@@.@x
#'     .@@@@@@@.@
#'     .@.@.@.@@@
#'     x.@@@.@@@@
#'     .@@@@@@@@.
#'     x.x.@@@.x.
#'
#'     Remove 12 rolls of paper:
#'     .......x..
#'     .@@.x.x.@x
#'     x@@@@...@@
#'     x.@@@@..x.
#'     .@.@@@@.x.
#'     .x@@@@@@.x
#'     .x.@.@.@@@
#'     ..@@@.@@@@
#'     .x@@@@@@@.
#'     ....@@@...
#'
#'     Remove 7 rolls of paper:
#'     ..........
#'     .x@.....x.
#'     .@@@@...xx
#'     ..@@@@....
#'     .x.@@@@...
#'     ..@@@@@@..
#'     ...@.@.@@x
#'     ..@@@.@@@@
#'     ..x@@@@@@.
#'     ....@@@...
#'
#'     Remove 5 rolls of paper:
#'     ..........
#'     ..x.......
#'     .x@@@.....
#'     ..@@@@....
#'     ...@@@@...
#'     ..x@@@@@..
#'     ...@.@.@@.
#'     ..x@@.@@@x
#'     ...@@@@@@.
#'     ....@@@...
#'
#'     Remove 2 rolls of paper:
#'     ..........
#'     ..........
#'     ..x@@.....
#'     ..@@@@....
#'     ...@@@@...
#'     ...@@@@@..
#'     ...@.@.@@.
#'     ...@@.@@@.
#'     ...@@@@@x.
#'     ....@@@...
#'
#'     Remove 1 roll of paper:
#'     ..........
#'     ..........
#'     ...@@.....
#'     ..x@@@....
#'     ...@@@@...
#'     ...@@@@@..
#'     ...@.@.@@.
#'     ...@@.@@@.
#'     ...@@@@@..
#'     ....@@@...
#'
#'     Remove 1 roll of paper:
#'     ..........
#'     ..........
#'     ...x@.....
#'     ...@@@....
#'     ...@@@@...
#'     ...@@@@@..
#'     ...@.@.@@.
#'     ...@@.@@@.
#'     ...@@@@@..
#'     ....@@@...
#'
#'     Remove 1 roll of paper:
#'     ..........
#'     ..........
#'     ....x.....
#'     ...@@@....
#'     ...@@@@...
#'     ...@@@@@..
#'     ...@.@.@@.
#'     ...@@.@@@.
#'     ...@@@@@..
#'     ....@@@...
#'
#'     Remove 1 roll of paper:
#'     ..........
#'     ..........
#'     ..........
#'     ...x@@....
#'     ...@@@@...
#'     ...@@@@@..
#'     ...@.@.@@.
#'     ...@@.@@@.
#'     ...@@@@@..
#'     ....@@@...
#'
#' Stop once no more rolls of paper are accessible by a forklift. In this
#' example, a total of *`43`* rolls of paper can be removed.
#'
#' Start with your original diagram. *How many rolls of paper in total can
#' be removed by the Elves and their forklifts?*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b()
f04a <- function(x) {
  # d <- strsplit(readLines("input.txt"), "")
  # d <- strsplit(readLines("inst/input04.txt"), "")
  d <- strsplit(x, "")
  floor <- (matrix(unlist(d), ncol = length(d[[1]]), byrow = TRUE))
  paper <- which(floor == "@", arr.ind = TRUE)
  acc <- 0
  for (pi in seq_len(nrow(paper))) {
    acc <- acc + is_clear(floor, paper[pi, ])
  }
  acc
}

is_clear <- function(f, p) {
  dirs <- t(p + t(expand.grid(-1:1, -1:1)[-5, ]))
  dirs <- dirs[dirs[,1] >= 1 & dirs[,2] >= 1 & dirs[,1] <= nrow(f) & dirs[,2] <= ncol(f), ]
  res <- rep(FALSE, 8)
  res[1:nrow(dirs)] <- f[as.matrix(dirs)] == "@"
  sum(res) < 4
}

#' @rdname day04
#' @export
f04b <- function(x) {
  # d <- strsplit(readLines("input.txt"), "")
  # d <- strsplit(readLines("inst/input04.txt"), "")
  d <- strsplit(x, "")
  floor <- (matrix(unlist(d), ncol = length(d[[1]]), byrow = TRUE))
  accessible <- Inf
  nrolls <- 0
  while (accessible > 0) {
    accessible <- 0
    paper <- which(floor == "@", arr.ind = TRUE)
    remove <- paper[NULL, ]
    for (pi in seq_len(nrow(paper))) {
      if (is_clear(floor, paper[pi, ])) {
        remove <- rbind(remove, paper[pi, ])
        nrolls <- nrolls + 1
        accessible <- accessible + 1
      }
    }
    floor[as.matrix(remove)] <- "."
  }
  nrolls
}


f04_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day04
#' @export
example_data_04 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
