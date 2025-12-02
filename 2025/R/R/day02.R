#' Day 02: Gift Shop
#'
#' [Gift Shop](https://adventofcode.com/2025/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' You get inside and take the elevator to its only other stop: the gift
#' shop. \"Thank you for visiting the North Pole!\" gleefully exclaims a
#' nearby sign. You aren\'t sure who is even allowed to visit the North
#' Pole, but you know you can access the lobby through here, and from there
#' you can access the rest of the North Pole base.
#'
#' As you make your way through the [surprisingly
#' extensive]{title="They even sell lunch boxes and blue tents!"}
#' selection, one of the clerks recognizes you and asks for your help.
#'
#' As it turns out, one of the younger Elves was playing on a gift shop
#' computer and managed to add a whole bunch of invalid product IDs to
#' their gift shop database! Surely, it would be no trouble for you to
#' identify the invalid product IDs for them, right?
#'
#' They\'ve even checked most of the product ID ranges already; they only
#' have a few product ID ranges (your puzzle input) that you\'ll need to
#' check. For example:
#'
#'     11-22,95-115,998-1012,1188511880-1188511890,222220-222224,
#'     1698522-1698528,446443-446449,38593856-38593862,565653-565659,
#'     824824821-824824827,2121212118-2121212124
#'
#' (The ID ranges are wrapped here for legibility; in your input, they
#' appear on a single long line.)
#'
#' The ranges are separated by commas (`,`); each range gives its *first
#' ID* and *last ID* separated by a dash (`-`).
#'
#' Since the young Elf was just doing silly patterns, you can find the
#' *invalid IDs* by looking for any ID which is made only of some sequence
#' of digits repeated twice. So, `55` (`5` twice), `6464` (`64` twice), and
#' `123123` (`123` twice) would all be invalid IDs.
#'
#' None of the numbers have leading zeroes; `0101` isn\'t an ID at all.
#' (`101` is a *valid* ID that you would ignore.)
#'
#' Your job is to find all of the invalid IDs that appear in the given
#' ranges. In the above example:
#'
#' - `11-22` has two invalid IDs, *`11`* and *`22`*.
#' - `95-115` has one invalid ID, *`99`*.
#' - `998-1012` has one invalid ID, *`1010`*.
#' - `1188511880-1188511890` has one invalid ID, *`1188511885`*.
#' - `222220-222224` has one invalid ID, *`222222`*.
#' - `1698522-1698528` contains no invalid IDs.
#' - `446443-446449` has one invalid ID, *`446446`*.
#' - `38593856-38593862` has one invalid ID, *`38593859`*.
#' - The rest of the ranges contain no invalid IDs.
#'
#' Adding up all the invalid IDs in this example produces *`1227775554`*.
#'
#' *What do you get if you add up all of the invalid IDs?*
#'
#' **Part Two**
#'
#' The clerk quickly discovers that there are still invalid IDs in the
#' ranges in your list. Maybe the young Elf was doing other silly patterns
#' as well?
#'
#' Now, an ID is invalid if it is made only of some sequence of digits
#' repeated *at least* twice. So, `12341234` (`1234` two times),
#' `123123123` (`123` three times), `1212121212` (`12` four times), and
#' `1111111` (`1` seven times) are all invalid IDs.
#'
#' From the same example as before:
#'
#' - `11-22` still has two invalid IDs, *`11`* and *`22`*.
#' - `95-115` now has two invalid IDs, *`99`* and *`111`*.
#' - `998-1012` now has two invalid IDs, *`999`* and *`1010`*.
#' - `1188511880-1188511890` still has one invalid ID, *`1188511885`*.
#' - `222220-222224` still has one invalid ID, *`222222`*.
#' - `1698522-1698528` still contains no invalid IDs.
#' - `446443-446449` still has one invalid ID, *`446446`*.
#' - `38593856-38593862` still has one invalid ID, *`38593859`*.
#' - `565653-565659` now has one invalid ID, *`565656`*.
#' - `824824821-824824827` now has one invalid ID, *`824824824`*.
#' - `2121212118-2121212124` now has one invalid ID, *`2121212121`*.
#'
#' Adding up all the invalid IDs in this example produces *`4174379265`*.
#'
#' *What do you get if you add up all of the invalid IDs using these new
#' rules?*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b()
f02a <- function(x) {
  # x <- unlist(read.csv("input.txt", header = FALSE))
  # x <- unlist(read.csv("inst/input02.txt", header = FALSE))
  v <- sapply(strsplit(x, "-"), as.numeric)
  sum(unlist(mapply(invalid, v[1,], v[2, ])  ))
}

invalid <- function(x, y) {
  z <- x:y
  z[sapply(as.character(z), check)]
}

check <- function(x) {
  identical(substring(x, 1, ceiling(nchar(x)/2)),
            substring(x, ceiling(nchar(x)/2)+1, nchar(x)))
}

invalid2 <- function(x, y) {
  z <- x:y
  z[sapply(as.character(z), check2)]
}

check2 <- function(x) {
  chars <- strsplit(x, "")[[1]]
  if (length(chars) == 1) return(FALSE)
  if (length(unique(chars)) == 1) return(TRUE)
  for (n in 2:length(chars)) {
    if (nchar(x) %% n != 0) next # doesn't cleanly divide
    u <- apply(matrix(chars, nrow = n, byrow = TRUE), 1, paste0, collapse = "")
    if (length(unique(u)) == 1) return(TRUE)
  }
  FALSE
}

#' @rdname day02
#' @export
f02b <- function(x) {
  # x <- unlist(read.csv("input.txt", header = FALSE))
  # x <- unlist(read.csv("inst/input02.txt", header = FALSE))
  v <- sapply(strsplit(x, "-"), as.numeric)
  res <- unlist(mapply(invalid2, v[1,], v[2, ]))
  sum(res)
}


f02_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
