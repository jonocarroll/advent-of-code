#' Day 04: Ceres Search
#'
#' [Ceres Search](https://adventofcode.com/2024/day/4)
#'
#' @name day04
#' @rdname day04
#' @details
#'
#' **Part One**
#'
#' \"Looks like the Chief\'s not here. Next!\" One of The Historians pulls
#' out a device and pushes the only button on it. After a brief flash, you
#' recognize the interior of the [Ceres monitoring station](/2019/day/10)!
#' 
#' As the search for the Chief continues, a small Elf who lives on the
#' station tugs on your shirt; she\'d like to know if you could help her
#' with her *word search* (your puzzle input). She only has to find one
#' word: `XMAS`.
#' 
#' This word search allows words to be horizontal, vertical, diagonal,
#' written backwards, or even overlapping other words. It\'s a little
#' unusual, though, as you don\'t merely need to find one instance of
#' `XMAS` - you need to find *all of them*. Here are a few ways `XMAS`
#' might appear, where irrelevant characters have been replaced with `.`:
#' 
#'     ..X...
#'     .SAMX.
#'     .A..A.
#'     XMAS.S
#'     .X....
#' 
#' The actual word search will be full of letters instead. For example:
#' 
#'     MMMSXXMASM
#'     MSAMXMSMSA
#'     AMXSXMAAMM
#'     MSAMASMSMX
#'     XMASAMXAMM
#'     XXAMMXXAMA
#'     SMSMSASXSS
#'     SAXAMASAAA
#'     MAMMMXMMMM
#'     MXMXAXMASX
#' 
#' In this word search, `XMAS` occurs a total of *`18`* times; here\'s the
#' same word search again, but where letters not involved in any `XMAS`
#' have been replaced with `.`:
#' 
#'     ....XXMAS.
#'     .SAMXMS...
#'     ...S..A...
#'     ..A.A.MS.X
#'     XMASAMX.MM
#'     X.....XA.A
#'     S.S.S.S.SS
#'     .A.A.A.A.A
#'     ..M.M.M.MM
#'     .X.X.XMASX
#' 
#' Take a look at the little Elf\'s word search. *How many times does
#' `XMAS` appear?*
#'
#' **Part Two**
#' 
#' The Elf looks quizzically at you. Did you misunderstand the assignment?
#' 
#' Looking for the instructions, you flip over the word search to find that
#' this isn\'t actually an *`XMAS`* puzzle; it\'s an
#' [*`X-MAS`*]{title="This part originally involved searching for something else, but this joke was too dumb to pass up."}
#' puzzle in which you\'re supposed to find two `MAS` in the shape of an
#' `X`. One way to achieve that is like this:
#' 
#'     M.S
#'     .A.
#'     M.S
#' 
#' Irrelevant characters have again been replaced with `.` in the above
#' diagram. Within the `X`, each `MAS` can be written forwards or
#' backwards.
#' 
#' Here\'s the same example from before, but this time all of the `X-MAS`es
#' have been kept instead:
#' 
#'     .M.S......
#'     ..A..MSMS.
#'     .M.S.MAA..
#'     ..A.ASMSM.
#'     .M.S.M....
#'     ..........
#'     S.S.S.S.S.
#'     .A.A.A.A..
#'     M.M.M.M.M.
#'     ..........
#' 
#' In this example, an `X-MAS` appears *`9`* times.
#' 
#' Flip the word search from the instructions back over to the word search
#' side and try again. *How many times does an `X-MAS` appear?*
#'
#' @param x some data
#' @return For Part One, `f04a(x)` returns .... For Part Two,
#'   `f04b(x)` returns ....
#' @export
#' @examples
#' f04a(example_data_04())
#' f04b()
f04a <- function(x) {
  d <- readLines("inst/input04.txt")
  m <- matrix(unlist(strsplit(d, "")), ncol = nchar(d[[1]]), byrow = TRUE)
  n <- dim(m)
  tot <- 0
  for (dir in c(-1, 1)) {
    for (ix in seq_len(n[1])) {
      for (jx in seq_len(n[2])) {
        tot <- tot + 
          find_word("h", m, ix, jx, dir, dir) + 
          find_word("v", m, ix, jx, dir, dir) + 
          find_word("d", m, ix, jx, dir, dir) +
          find_word("d", m, ix, jx, dir, -dir)
      }
    }
  }
  tot
}

inbounds <- function(m, i, j) {
  !(i > nrow(m) || j > ncol(m) || i < 1 || j < 1)
}

find_word <- function(v, m, i, j, del1, del2) {
  xmas <- c("X", "M", "A", "S")
  ii <- i
  jj <- j
  for (off in 0:3) {
    if (v == "h") {
      jj <- j + off*del1
    } else if (v == "v") {
      ii <- i + off*del1
    } else {
      ii <- i + off*del1
      jj <- j + off*del2
    }
    if (!inbounds(m, ii, jj)) return(FALSE)
    if (m[ii, jj] != xmas[off+1]) return(FALSE)
  }
  TRUE
}

find_mas <- function(m, i, j) {
  
  ii <- i
  jj <- j
  
  # are we at an A?
  if (!inbounds(m, ii, jj)) return(FALSE)
  if (m[ii, jj] != "A") return(FALSE)
  
  # we're at an A; are the others correct?
  
  i1 <- i - 1
  j1 <- j - 1
  if (!inbounds(m, i1, j1)) return(FALSE)
  
  i2 <- i - 1
  j2 <- j + 1
  if (!inbounds(m, i2, j2)) return(FALSE)
  
  i3 <- i + 1
  j3 <- j - 1
  if (!inbounds(m, i3, j3)) return(FALSE)
  
  i4 <- i + 1
  j4 <- j + 1
  if (!inbounds(m, i4, j4)) return(FALSE)
  
  opt1 <- (m[[i1, j1]] == "M" && m[i4, j4] == "S") || 
    (m[[i1, j1]] == "S" && m[[i4, j4]] == "M")
  
  opt2 <- (m[[i2, j2]] == "M" && m[i3, j3] == "S") || 
    (m[[i2, j2]] == "S" && m[[i3, j3]] == "M")
  
  opt1 && opt2
  
}

#' @rdname day04
#' @export
f04b <- function(x) {
  d <- readLines("inst/input04.txt")
  m <- matrix(unlist(strsplit(d, "")), ncol = nchar(d[[1]]), byrow = TRUE)
  n <- dim(m)
  tot <- 0
  for (ix in seq_len(n[1])) {
    for (jx in seq_len(n[2])) {
      tot <- tot + find_mas(m, ix, jx)
    }
  }
  tot
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


