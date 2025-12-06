#' Day 06: Trash Compactor
#'
#' [Trash Compactor](https://adventofcode.com/2025/day/6)
#'
#' @name day06
#' @rdname day06
#' @details
#'
#' **Part One**
#'
#' After helping the Elves in the kitchen, you were taking a break and
#' helping them re-enact a movie scene when you over-enthusiastically
#' jumped into the garbage chute!
#'
#' A brief fall later, you find yourself in a [garbage
#' smasher]{title="To your surprise, the smell isn't that bad."}.
#' Unfortunately, the door\'s been magnetically sealed.
#'
#' As you try to find a way out, you are approached by a family of
#' cephalopods! They\'re pretty sure they can get the door open, but it
#' will take some time. While you wait, they\'re curious if you can help
#' the youngest cephalopod with her [math homework](/2021/day/18).
#'
#' Cephalopod math doesn\'t look that different from normal math. The math
#' worksheet (your puzzle input) consists of a list of *problems*; each
#' problem has a group of numbers that need to either be either *added*
#' (`+`) or *multiplied* (`*`) together.
#'
#' However, the problems are arranged a little strangely; they seem to be
#' presented next to each other in a very long horizontal list. For
#' example:
#'
#'     123 328  51 64
#'      45 64  387 23
#'       6 98  215 314
#'     *   +   *   +
#'
#' Each problem\'s numbers are arranged vertically; at the bottom of the
#' problem is the symbol for the operation that needs to be performed.
#' Problems are separated by a full column of only spaces. The left/right
#' alignment of numbers within each problem can be ignored.
#'
#' So, this worksheet contains four problems:
#'
#' - `123` \* `45` \* `6` = *`33210`*
#' - `328` + `64` + `98` = *`490`*
#' - `51` \* `387` \* `215` = *`4243455`*
#' - `64` + `23` + `314` = *`401`*
#'
#' To check their work, cephalopod students are given the *grand total* of
#' adding together all of the answers to the individual problems. In this
#' worksheet, the grand total is `33210` + `490` + `4243455` + `401` =
#' *`4277556`*.
#'
#' Of course, the actual worksheet is *much* wider. You\'ll need to make
#' sure to unroll it completely so that you can read the problems clearly.
#'
#' Solve the problems on the math worksheet. *What is the grand total found
#' by adding together all of the answers to the individual problems?*
#'
#' **Part Two**
#'
#' The big cephalopods come back to check on how things are going. When
#' they see that your grand total doesn\'t match the one expected by the
#' worksheet, they realize they forgot to explain how to read cephalopod
#' math.
#'
#' Cephalopod math is written *right-to-left in columns*. Each number is
#' given in its own column, with the most significant digit at the top and
#' the least significant digit at the bottom. (Problems are still separated
#' with a column consisting only of spaces, and the symbol at the bottom of
#' the problem is still the operator to use.)
#'
#' Here\'s the example worksheet again:
#'
#'     123 328  51 64
#'      45 64  387 23
#'       6 98  215 314
#'     *   +   *   +
#'
#' Reading the problems right-to-left one column at a time, the problems
#' are now quite different:
#'
#' - The rightmost problem is `4` + `431` + `623` = *`1058`*
#' - The second problem from the right is `175` \* `581` \* `32` =
#'   *`3253600`*
#' - The third problem from the right is `8` + `248` + `369` = *`625`*
#' - Finally, the leftmost problem is `356` \* `24` \* `1` = *`8544`*
#'
#' Now, the grand total is `1058` + `3253600` + `625` + `8544` =
#' *`3263827`*.
#'
#' Solve the problems on the math worksheet again. *What is the grand total
#' found by adding together all of the answers to the individual problems?*
#'
#' @param x some data
#' @return For Part One, `f06a(x)` returns .... For Part Two,
#'   `f06b(x)` returns ....
#' @export
#' @examples
#' f06a(example_data_06())
#' f06b()
f06a <- function(x) {
  # d <- readLines("input.txt")
  # d <- readLines("inst/input06.txt")
  d <- x
  math <- read.csv(text = paste(gsub("[ ]+", ",", trimws(d)), collapse = "\n"), header = FALSE)
  ops <- math[nrow(math), ]
  values <- lapply(math[-nrow(math), ], as.numeric)
  sprintf("%16.0f", sum(mapply(Reduce, ops, values)))
}


#' @rdname day06
#' @export
f06b <- function(x) {
  # d <- readLines("input.txt")
  # d <- readLines("inst/input06.txt")
  d <- x
  rows <- length(d) - 1
  i <- nchar(d[length(d)])
  values <- c()
  acc <- 0
  while (i > 0) {
    z <- substring(d[length(d)], i, i)
    newval <- as.numeric(paste0(sapply(1:rows, \(x) substring(d[x], i, i)), collapse = ""))
    if (is.na(newval)) {
      i <- i - 1
      next
    }
    values <- c(values, newval)
    if (z %in% c("+", "*")) {
      acc <- acc + Reduce(z, values)
      values <- c()
    }
    i <- i - 1
  }
  sprintf("%16.0f", acc)
}


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
