#' Day 05: Cafeteria
#'
#' [Cafeteria](https://adventofcode.com/2025/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' As the forklifts break through the wall, the Elves are delighted to
#' discover that there was a cafeteria on the other side after all.
#'
#' You can hear a commotion coming from the kitchen. \"At this rate, we
#' won\'t have any time left to put the wreaths up in the dining hall!\"
#' Resolute in your quest, you investigate.
#'
#' \"If only we hadn\'t switched to the new inventory management system
#' right before Christmas!\" another Elf exclaims. You ask what\'s going
#' on.
#'
#' The Elves in the kitchen explain the situation: because of their
#' complicated new inventory management system, they can\'t figure out
#' which of their ingredients are *fresh* and which are
#' [*spoiled*]{title="No, this puzzle does not take place on Gleba. Why do you ask?"}.
#' When you ask how it works, they give you a copy of their database (your
#' puzzle input).
#'
#' The database operates on *ingredient IDs*. It consists of a list of
#' *fresh ingredient ID ranges*, a blank line, and a list of *available
#' ingredient IDs*. For example:
#'
#'     3-5
#'     10-14
#'     16-20
#'     12-18
#'
#'     1
#'     5
#'     8
#'     11
#'     17
#'     32
#'
#' The fresh ID ranges are *inclusive*: the range `3-5` means that
#' ingredient IDs `3`, `4`, and `5` are all *fresh*. The ranges can also
#' *overlap*; an ingredient ID is fresh if it is in *any* range.
#'
#' The Elves are trying to determine which of the *available ingredient
#' IDs* are *fresh*. In this example, this is done as follows:
#'
#' - Ingredient ID `1` is spoiled because it does not fall into any range.
#' - Ingredient ID `5` is *fresh* because it falls into range `3-5`.
#' - Ingredient ID `8` is spoiled.
#' - Ingredient ID `11` is *fresh* because it falls into range `10-14`.
#' - Ingredient ID `17` is *fresh* because it falls into range `16-20` as
#'   well as range `12-18`.
#' - Ingredient ID `32` is spoiled.
#'
#' So, in this example, *`3`* of the available ingredient IDs are fresh.
#'
#' Process the database file from the new inventory management system. *How
#' many of the available ingredient IDs are fresh?*
#'
#' **Part Two**
#'
#' The Elves start bringing their spoiled inventory to the trash chute at
#' the back of the kitchen.
#'
#' So that they can stop bugging you when they get new inventory, the Elves
#' would like to know *all* of the IDs that the *fresh ingredient ID
#' ranges* consider to be *fresh*. An ingredient ID is still considered
#' fresh if it is in any range.
#'
#' Now, the second section of the database (the available ingredient IDs)
#' is irrelevant. Here are the fresh ingredient ID ranges from the above
#' example:
#'
#'     3-5
#'     10-14
#'     16-20
#'     12-18
#'
#' The ingredient IDs that these ranges consider to be fresh are `3`, `4`,
#' `5`, `10`, `11`, `12`, `13`, `14`, `15`, `16`, `17`, `18`, `19`, and
#' `20`. So, in this example, the fresh ingredient ID ranges consider a
#' total of *`14`* ingredient IDs to be fresh.
#'
#' Process the database file again. *How many ingredient IDs are considered
#' to be fresh according to the fresh ingredient ID ranges?*
#'
#' @param x some data
#' @return For Part One, `f05a(x)` returns .... For Part Two,
#'   `f05b(x)` returns ....
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b()
f05a <- function(x) {
  # d <- readLines("input.txt")
  # d <- readLines("inst/input05.txt")
  d <- x
  s <- which(d == "")
  fresh <- lapply(strsplit(d[1:(s-1)], "-"), as.numeric)
  avail <- as.numeric(d[(s+1):length(d)])
  acc <- 0
  for (i in seq_along(avail)) {
    for (j in seq_along(fresh)) {
      if (avail[i] >= fresh[[j]][1] & avail[i] <= fresh[[j]][2]) {
        acc <- acc + 1
        break
      }
    }
  }
  acc
}


#' @rdname day05
#' @export
f05b <- function(x) {
  # d <- readLines("input.txt")
  # d <- readLines("inst/input05.txt")
  d <- x
  s <- which(d == "")
  fresh <- lapply(d[1:(s-1)], \(x) as.numeric(strsplit(x, "-")[[1]]))
  fresh <- fresh[order(sapply(fresh, `[[`, 1))]
  top <- 0
  acc <- 0

  for (i in fresh) {
    if (i[2] <= top) next
    acc <- if (i[1] > top) {
      acc + i[2] - i[1] + 1
    } else {
      acc + i[2] - top
    }
    top <- i[2]
  }
  sprintf("%16.0f", acc)

}


f05_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
