#' Day 03: Gear Ratios
#'
#' [Gear Ratios](https://adventofcode.com/2023/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' You and the Elf eventually reach a [gondola
#' lift](https://en.wikipedia.org/wiki/Gondola_lift){target="_blank"}
#' station; he says the gondola lift will take you up to the *water
#' source*, but this is as far as he can bring you. You go inside.
#' 
#' It doesn\'t take long to find the gondolas, but there seems to be a
#' problem: they\'re not moving.
#' 
#' \"Aaah!\"
#' 
#' You turn around to see a slightly-greasy Elf with a wrench and a look of
#' surprise. \"Sorry, I wasn\'t expecting anyone! The gondola lift isn\'t
#' working right now; it\'ll still be a while before I can fix it.\" You
#' offer to help.
#' 
#' The engineer explains that an engine part seems to be missing from the
#' engine, but nobody can figure out which one. If you can *add up all the
#' part numbers* in the engine schematic, it should be easy to work out
#' which part is missing.
#' 
#' The engine schematic (your puzzle input) consists of a visual
#' representation of the engine. There are lots of numbers and symbols you
#' don\'t really understand, but apparently *any number adjacent to a
#' symbol*, even diagonally, is a \"part number\" and should be included in
#' your sum. (Periods (`.`) do not count as a symbol.)
#' 
#' Here is an example engine schematic:
#' 
#'     467..114..
#'     ...*......
#'     ..35..633.
#'     ......#...
#'     617*......
#'     .....+.58.
#'     ..592.....
#'     ......755.
#'     ...$.*....
#'     .664.598..
#' 
#' In this schematic, two numbers are *not* part numbers because they are
#' not adjacent to a symbol: `114` (top right) and `58` (middle right).
#' Every other number is adjacent to a symbol and so *is* a part number;
#' their sum is *`4361`*.
#' 
#' Of course, the actual engine schematic is much larger. *What is the sum
#' of all of the part numbers in the engine schematic?*
#'
#' **Part Two**
#'
#' *(Use have to manually add this yourself.)*
#'
#' *(Try using `convert_clipboard_html_to_roxygen_md()`)*
#'
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b()
f03a <- function(x) {
  sum(as.integer(f03_helper(x)$part))
}
#326410 too low
#536679

#' @rdname day03
#' @export
f03b <- function(x) {

}


f03_helper <- function(x) {
  mat <- matrix(c(unlist(strsplit(x, ""))), ncol = nchar(x[[1]]), byrow = TRUE)
  matTF <- matrix(!mat %in% c(".", 0:9), ncol = ncol(mat), byrow = FALSE)
  symbols <- which(matTF, arr.ind = TRUE)
  numsTF <- matrix(mat %in% as.character(0:9), ncol = ncol(mat), byrow = FALSE)
  nums <- which(numsTF, arr.ind = TRUE)
  parts <- data.frame(part = character(), near = logical())
  for (n in seq_len(nrow(nums))) {
    parts <- rbind(parts, check_num(mat, nums[n, ]))
  }
  
  dplyr::group_by(parts, part) |>
    # dplyr::summarise(not_missing = any(near_sym)) |>
    dplyr::filter(near_sym)
}
  
check_num <- function(m, pos) {
  near_sym <- search_nearby(m, pos)
  snum <- m[matrix(pos, nrow = 1)]
  if (pos[2] - 1 > 0 && m[matrix(c(pos[1], pos[2] - 1), nrow = 1)] %in% as.character(0:9)) {
    return(NULL)
  }
  i <- 1
  while(pos[2] - i > 0 && m[matrix(c(pos[1], pos[2] - i), nrow = 1)] %in% as.character(0:9)) {
    snum <- paste0(m[matrix(c(pos[1], pos[2] - i), nrow = 1)], snum)
    near_sym <- near_sym || search_nearby(m, c(pos[1], pos[2] - i))
    i <- i + 1
  }
  startpos <- pos[2] - i + 1
  i <- 1
  while(pos[2] + i <= ncol(m) && m[matrix(c(pos[1], pos[2] + i), nrow = 1)] %in% as.character(0:9)) {
    snum <- paste0(snum, m[matrix(c(pos[1], pos[2] + i), nrow = 1)])
     near_sym <- near_sym || search_nearby(m, c(pos[1], pos[2] + i))
    i <- i + 1
  }
  endpos <- pos[2] + i - 1
  data.frame(part = snum, near_sym = near_sym)
}

search_nearby <- function(m, pos) {
  (test_for_sym(m, pos, c(-1, -1)) ||
     test_for_sym(m, pos, c(-1, 0)) ||
     test_for_sym(m, pos, c(-1, 1)) ||
     test_for_sym(m, pos, c(0, -1)) ||
     test_for_sym(m, pos, c(0, 1)) ||
     test_for_sym(m, pos, c(1, -1)) ||
     test_for_sym(m, pos, c(1, 0)) ||
     test_for_sym(m, pos, c(1, 1)) )
}

test_for_sym <- function(m, pos, offset) {
  newpos <- pos + offset
  if (newpos[1] > nrow(m) || newpos[1] <= 0 || newpos[2] > ncol(m) || newpos[2] <= 0) {
    FALSE
  } else {
    ! (m[matrix(newpos, nrow = 1)] %in% c(".", 0:9))
  }
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
