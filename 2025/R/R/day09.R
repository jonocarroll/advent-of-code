#' Day 09: Movie Theater
#'
#' [Movie Theater](https://adventofcode.com/2025/day/9)
#'
#' @name day09
#' @rdname day09
#' @details
#'
#' **Part One**
#'
#' You [slide
#' down]{title="wheeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeeee"}
#' the [firepole](https://en.wikipedia.org/wiki/Fireman%27s_pole) in the
#' corner of the playground and land in the North Pole base movie theater!
#'
#' The movie theater has a big tile floor with an interesting pattern.
#' Elves here are redecorating the theater by switching out some of the
#' square tiles in the big grid they form. Some of the tiles are *red*; the
#' Elves would like to find the largest rectangle that uses red tiles for
#' two of its opposite corners. They even have a list of where the red
#' tiles are located in the grid (your puzzle input).
#'
#' For example:
#'
#'     7,1
#'     11,1
#'     11,7
#'     9,7
#'     9,5
#'     2,5
#'     2,3
#'     7,3
#'
#' Showing red tiles as `#` and other tiles as `.`, the above arrangement
#' of red tiles would look like this:
#'
#'     ..............
#'     .......#...#..
#'     ..............
#'     ..#....#......
#'     ..............
#'     ..#......#....
#'     ..............
#'     .........#.#..
#'     ..............
#'
#' You can choose any two red tiles as the opposite corners of your
#' rectangle; your goal is to find the largest rectangle possible.
#'
#' For example, you could make a rectangle (shown as `O`) with an area of
#' `24` between `2,5` and `9,7`:
#'
#'     ..............
#'     .......#...#..
#'     ..............
#'     ..#....#......
#'     ..............
#'     ..OOOOOOOO....
#'     ..OOOOOOOO....
#'     ..OOOOOOOO.#..
#'     ..............
#'
#' Or, you could make a rectangle with area `35` between `7,1` and `11,7`:
#'
#'     ..............
#'     .......OOOOO..
#'     .......OOOOO..
#'     ..#....OOOOO..
#'     .......OOOOO..
#'     ..#....OOOOO..
#'     .......OOOOO..
#'     .......OOOOO..
#'     ..............
#'
#' You could even make a thin rectangle with an area of only `6` between
#' `7,3` and `2,3`:
#'
#'     ..............
#'     .......#...#..
#'     ..............
#'     ..OOOOOO......
#'     ..............
#'     ..#......#....
#'     ..............
#'     .........#.#..
#'     ..............
#'
#' Ultimately, the largest rectangle you can make in this example has area
#' *`50`*. One way to do this is between `2,5` and `11,1`:
#'
#'     ..............
#'     ..OOOOOOOOOO..
#'     ..OOOOOOOOOO..
#'     ..OOOOOOOOOO..
#'     ..OOOOOOOOOO..
#'     ..OOOOOOOOOO..
#'     ..............
#'     .........#.#..
#'     ..............
#'
#' Using two red tiles as opposite corners, *what is the largest area of
#' any rectangle you can make?*
#'
#' **Part Two**
#'
#' The Elves just remembered: they can only switch out tiles that are *red*
#' or *green*. So, your rectangle can only include red or green tiles.
#'
#' In your list, every red tile is connected to the red tile before and
#' after it by a straight line of *green tiles*. The list wraps, so the
#' first red tile is also connected to the last red tile. Tiles that are
#' adjacent in your list will always be on either the same row or the same
#' column.
#'
#' Using the same example as before, the tiles marked `X` would be green:
#'
#'     ..............
#'     .......#XXX#..
#'     .......X...X..
#'     ..#XXXX#...X..
#'     ..X........X..
#'     ..#XXXXXX#.X..
#'     .........X.X..
#'     .........#X#..
#'     ..............
#'
#' In addition, all of the tiles *inside* this loop of red and green tiles
#' are *also* green. So, in this example, these are the green tiles:
#'
#'     ..............
#'     .......#XXX#..
#'     .......XXXXX..
#'     ..#XXXX#XXXX..
#'     ..XXXXXXXXXX..
#'     ..#XXXXXX#XX..
#'     .........XXX..
#'     .........#X#..
#'     ..............
#'
#' The remaining tiles are never red nor green.
#'
#' The rectangle you choose still must have red tiles in opposite corners,
#' but any other tiles it includes must now be red or green. This
#' significantly limits your options.
#'
#' For example, you could make a rectangle out of red and green tiles with
#' an area of `15` between `7,3` and `11,1`:
#'
#'     ..............
#'     .......OOOOO..
#'     .......OOOOO..
#'     ..#XXXXOOOOO..
#'     ..XXXXXXXXXX..
#'     ..#XXXXXX#XX..
#'     .........XXX..
#'     .........#X#..
#'     ..............
#'
#' Or, you could make a thin rectangle with an area of `3` between `9,7`
#' and `9,5`:
#'
#'     ..............
#'     .......#XXX#..
#'     .......XXXXX..
#'     ..#XXXX#XXXX..
#'     ..XXXXXXXXXX..
#'     ..#XXXXXXOXX..
#'     .........OXX..
#'     .........OX#..
#'     ..............
#'
#' The largest rectangle you can make in this example using only red and
#' green tiles has area *`24`*. One way to do this is between `9,5` and
#' `2,3`:
#'
#'     ..............
#'     .......#XXX#..
#'     .......XXXXX..
#'     ..OOOOOOOOXX..
#'     ..OOOOOOOOXX..
#'     ..OOOOOOOOXX..
#'     .........XXX..
#'     .........#X#..
#'     ..............
#'
#' Using two red tiles as opposite corners, *what is the largest area of
#' any rectangle you can make using only red and green tiles?*
#'
#' @param x some data
#' @return For Part One, `f09a(x)` returns .... For Part Two,
#'   `f09b(x)` returns ....
#' @export
#' @examples
#' f09a(example_data_09())
#' f09b()
f09a <- function(x) {
  # d <- read.csv("input.txt", header = FALSE)
  d <- read.csv("inst/input09.txt", header = FALSE)
  n <- seq_len(nrow(d))
  max(outer(n, n, size))
}

size <- function(i, j) {
  (abs(d[j, 1] - d[i, 1]) +  1) * (abs(d[j, 2] - d[i, 2]) + 1)
}

#' @rdname day09
#' @export
f09b <- function(x) {
  # d <- read.csv("input.txt", header = FALSE)
  d <- read.csv("inst/input09.txt", header = FALSE)
  n <- seq_len(nrow(d))

  path <- data.frame(x = integer(0), y = integer(0))
  for (i in 2:nrow(d)) {
    if (d[i-1,1] - d[i,1] == 0) {
      path <- rbind(path, data.frame(x=d[i, 1], y=d[i-1,2]:d[i,2]))
    } else {
      path <- rbind(path, data.frame(x=d[i-1,1]:d[i,1], y= d[i, 2]))
    }
  }
  if (d[i,1] - d[1,1] == 0) {
    path <- rbind(path, data.frame(x=d[i, 1], y=d[i,2]:d[1,2]))
  } else {
    path <- rbind(path, data.frame(x=d[i,1]:d[1,1], y= d[i, 2]))
  }

  n <- seq_len(nrow(d))
  sizes <- outer(n, n, size)
  biggest <- rev(sort(unique(c(sizes))))

  res <- 0
  for (i in seq_along(biggest)) {
    ind <- which(sizes == biggest[i], arr.ind = TRUE)[1, ]
    bi <- ind[1]
    bj <- ind[2]
    anyinbox <- any(in_box(c(d[bi, 1], d[bj, 1]), c(d[bi, 2], d[bj, 2]), path[,1], path[,2]))
    if (!anyinbox) {
      res <- size(bi, bj)
      break
    }
  }
  res

}

in_box <- function(bx, by, xs, ys) {
  (xs > min(bx[1], bx[2])) & (xs < max(bx[1], bx[2])) & (ys > min(by[1], by[2])) & (ys < max(by[1], by[2]))
}

f09_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day09
#' @export
example_data_09 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
