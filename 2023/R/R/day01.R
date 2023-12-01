#' Day 01: Trebuchet?!
#'
#' [Trebuchet?!](https://adventofcode.com/2023/day/1)
#'
#' @name day01
#' @rdname day01
#' @details
#'
#' **Part One**
#'
#' Something is wrong with global snow production, and you\'ve been
#' selected to take a look. The Elves have even given you a map; on it,
#' they\'ve used stars to mark the top fifty locations that are likely to
#' be having problems.
#' 
#' You\'ve been doing this long enough to know that to restore snow
#' operations, you need to check all *fifty stars* by December 25th.
#' 
#' Collect stars by solving puzzles. Two puzzles will be made available on
#' each day in the Advent calendar; the second puzzle is unlocked when you
#' complete the first. Each puzzle grants *one star*. Good luck!
#' 
#' You try to ask why they can\'t just use a [weather machine](/2015/day/1)
#' (\"not powerful enough\") and where they\'re even sending you (\"the
#' sky\") and why your map looks mostly blank (\"you sure ask a lot of
#' questions\")
#' [and]{title="My hope is that this abomination of a run-on sentence somehow conveys the chaos of being hastily loaded into a trebuchet."}
#' hang on did you just say the sky (\"of course, where do you think snow
#' comes from\") when you realize that the Elves are already loading you
#' into a
#' [trebuchet](https://en.wikipedia.org/wiki/Trebuchet){target="_blank"}
#' (\"please hold still, we need to strap you in\").
#' 
#' As they\'re making the final adjustments, they discover that their
#' calibration document (your puzzle input) has been *amended* by a very
#' young Elf who was apparently just excited to show off her art skills.
#' Consequently, the Elves are having trouble reading the values on the
#' document.
#' 
#' The newly-improved calibration document consists of lines of text; each
#' line originally contained a specific *calibration value* that the Elves
#' now need to recover. On each line, the calibration value can be found by
#' combining the *first digit* and the *last digit* (in that order) to form
#' a single *two-digit number*.
#' 
#' For example:
#' 
#'     1abc2
#'     pqr3stu8vwx
#'     a1b2c3d4e5f
#'     treb7uchet
#' 
#' In this example, the calibration values of these four lines are `12`,
#' `38`, `15`, and `77`. Adding these together produces *`142`*.
#' 
#' Consider your entire calibration document. *What is the sum of all of
#' the calibration values?*
#'
#' **Part Two**
#'
#' Your calculation isn\'t quite right. It looks like some of the digits
#' are actually *spelled out with letters*: `one`, `two`, `three`, `four`,
#' `five`, `six`, `seven`, `eight`, and `nine` *also* count as valid
#' \"digits\".
#' 
#' Equipped with this new information, you now need to find the real first
#' and last digit on each line. For example:
#' 
#'     two1nine
#'     eightwothree
#'     abcone2threexyz
#'     xtwone3four
#'     4nineeightseven2
#'     zoneight234
#'     7pqrstsixteen
#' 
#' In this example, the calibration values are `29`, `83`, `13`, `24`,
#' `42`, `14`, and `76`. Adding these together produces *`281`*.
#' 
#' *What is the sum of all of the calibration values?*
#'
#' @param x some data
#' @return For Part One, `f01a(x)` returns .... For Part Two,
#'   `f01b(x)` returns ....
#' @export
#' @examples
#' f01a(example_data_01())
#' f01b()
f01a <- function(x) {
  x <- strsplit(gsub("[^0-9]", "", x), "")
  fandl <- sapply(x, \(y) paste0(y[1], y[length(y)]))
  sum(as.integer(fandl))
}


#' @rdname day01
#' @export
f01b <- function(x) {
  x <- gsub("(one)", "o1ne", x)
  x <- gsub("(two)", "t2wo", x)
  x <- gsub("(three)", "thr3ee", x)
  x <- gsub("(four)", "fo4ur", x)
  x <- gsub("(five)", "fi5ve", x)
  x <- gsub("(six)", "si6x", x)
  x <- gsub("(seven)", "sev7en", x)
  x <- gsub("(eight)", "eig8ht", x)
  x <- gsub("(nine)", "ni9ne", x)
  f01a(x)
}


f01_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day01
#' @export
example_data_01 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}

## part a works with gregexec, but partb is still off by 7
#' f01a <- function(x) {
#'   digits <- gregexec("([0-9])", x)
#'   fandl <- sapply(seq_along(digits), \(y) {
#'     d <- digits[[y]]
#'     paste0(substr(x[[y]], d[1,1], d[1,1]), substr(x[[y]], d[1,ncol(d)], d[1,ncol(d)]))
#'     })
#'   sum(as.integer(fandl))
#' }
#' 
#' 
#' f01b <- function(x) {
#'   digits <- gregexec("[0-9]|one|two|three|four|five|six|seven|eight|nine", x)
#'   mat <- sapply(regmatches(x, digits), \(y) y[1, c(1, ncol(y))])
#'   d <- c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine")
#'   for (p in seq_along(d)) {
#'     mat <- sub(d[p], p, mat)    
#'   }
#'   fandl <- apply(mat, 2, paste0, collapse = "")
#'   sum(as.integer(fandl))
#' }