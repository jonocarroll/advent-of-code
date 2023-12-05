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
#' The engineer finds the missing part and installs it in the engine! As
#' the engine springs to life, you jump in the closest gondola, finally
#' ready to ascend to the water source.
#' 
#' You don\'t seem to be going very fast, though. Maybe something is still
#' wrong? Fortunately, the gondola has a phone labeled \"help\", so you
#' pick it up and the engineer answers.
#' 
#' Before you can explain the situation, she suggests that you look out the
#' window. There stands the engineer, holding a phone in one hand and
#' waving with the other. You\'re going so slowly that you haven\'t even
#' left the station. You exit the gondola.
#' 
#' The missing part wasn\'t the only issue - one of the gears in the engine
#' is wrong. A *gear* is any `*` symbol that is adjacent to *exactly two
#' part numbers*. Its *gear ratio* is the result of
#' [multiplying]{title="They're magic gears."} those two numbers together.
#' 
#' This time, you need to find the gear ratio of every gear and add them
#' all up so that the engineer can figure out which gear needs to be
#' replaced.
#' 
#' Consider the same engine schematic again:
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
#' In this schematic, there are *two* gears. The first is in the top left;
#' it has part numbers `467` and `35`, so its gear ratio is `16345`. The
#' second gear is in the lower right; its gear ratio is `451490`. (The `*`
#' adjacent to `617` is *not* a gear because it is only adjacent to one
#' part number.) Adding up all of the gear ratios produces *`467835`*.
#' 
#' *What is the sum of all of the gear ratios in your engine schematic?*
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

#' @rdname day03
#' @export
f03b <- function(x) {
  sum(f03b_helper(x)$power)
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
    dplyr::filter(near_sym)
}

f03b_helper <- function(x) {
  mat <- matrix(c(unlist(strsplit(x, ""))), ncol = nchar(x[[1]]), byrow = TRUE)
  matTF <- mat == "*"
  asterisks <- which(matTF, arr.ind = TRUE)
  offsets <- expand.grid(-1:1, -1:1)
  potential_nums <- data.frame(asterisk = character(), part = character())
  for (g in seq_len(nrow(asterisks))) {
    for (o in seq_len(nrow(offsets))) {
      newpos <- c(unlist(asterisks[g, ] + offsets[o,]))
      if (mat[matrix(newpos, nrow = 1)] %in% as.character(0:9)) {
        num <- find_num(mat, newpos)
        potential_nums <- rbind(potential_nums, cbind(asterisk = paste(asterisks[g, 1], asterisks[g, 2], sep = "_"), num))
      }
    }
  }
  potential_nums |> 
    dplyr::distinct() |> 
    dplyr::group_by(asterisk) |> 
    dplyr::filter(dplyr::n() == 2) |> 
    dplyr::summarise(power = prod(as.integer(part)))
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

find_num <- function(m, pos) {
  snum <- m[matrix(pos, nrow = 1)]
  i <- 1
  while(pos[2] - i > 0 && m[matrix(c(pos[1], pos[2] - i), nrow = 1)] %in% as.character(0:9)) {
    snum <- paste0(m[matrix(c(pos[1], pos[2] - i), nrow = 1)], snum)
    i <- i + 1
  }
  i <- 1
  while(pos[2] + i <= ncol(m) && m[matrix(c(pos[1], pos[2] + i), nrow = 1)] %in% as.character(0:9)) {
    snum <- paste0(snum, m[matrix(c(pos[1], pos[2] + i), nrow = 1)])
    i <- i + 1
  }
  data.frame(part = snum)
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

# ## golfed
# x <- readLines("inst/example03.txt")
# x <- readLines("inst/input03.txt")
# m <- matrix(c(unlist(strsplit(x, ""))), ncol = nchar(x[[1]]), byrow = TRUE)
# nr <- nrow(m)
# nc <- ncol(m)
# d <- matrix(m %in% as.character(0:9), ncol = nc, byrow = FALSE)
# n <- as.integer(eval(str2lang(
#   paste0("c(", 
#          gsub("^,|,$", "", 
#               gsub(",+", "\\1,", 
#                    gsub("\\D", ",", 
#                         paste(x, collapse = ""))))
#          , ")")
# )))
# has_sym <- function(p1, p2) {
#   o <- expand.grid(-1:1, -1:1)
#   np <- o + rep(c(p1, p2), each = nrow(o))
#   np <- np[np[, 1] > 0 & np[, 1] <= nr, ]
#   np <- np[np[, 2] > 0 & np[, 2] <= nc, ]
#   np <- np[np[, 1] != 0 & np[, 2] != 0, ]
#   any(
#     !sapply(
#       seq_len(nrow(np)), 
#       \(y) m[matrix(unlist(np[y, ]), nrow = 1)]) %in% c(".", 0:9)
#   )
# }
# vhs <- Vectorize(has_sym)
# s <- outer(1:nr, 1:nc, "vhs")
# sum(as.integer(
#   n[grepl("2", 
#           strsplit(
#             gsub("^0+|0+$", "", 
#                  paste(c(t((s&d)+d)), collapse = ""))
#             , "0+")[[1]]
#   )]
# ))
# 
