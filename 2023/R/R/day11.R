#' Day 11: Cosmic Expansion
#'
#' [Cosmic Expansion](https://adventofcode.com/2023/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' You continue following signs for \"Hot Springs\" and eventually come
#' across an
#' [observatory](https://en.wikipedia.org/wiki/Observatory){target="_blank"}.
#' The Elf within turns out to be a researcher studying cosmic expansion
#' using the giant telescope here.
#' 
#' He doesn\'t know anything about the missing machine parts; he\'s only
#' visiting for this research project. However, he confirms that the hot
#' springs are the next-closest area likely to have people; he\'ll even
#' take you straight there once he\'s done with today\'s observation
#' analysis.
#' 
#' Maybe you can help him with the analysis to speed things up?
#' 
#' The researcher has collected a bunch of data and compiled the data into
#' a single giant *image* (your puzzle input). The image includes *empty
#' space* (`.`) and *galaxies* (`#`). For example:
#' 
#'     ...#......
#'     .......#..
#'     #.........
#'     ..........
#'     ......#...
#'     .#........
#'     .........#
#'     ..........
#'     .......#..
#'     #...#.....
#' 
#' The researcher is trying to figure out the sum of the lengths of the
#' *shortest path between every pair of galaxies*. However, there\'s a
#' catch: the universe expanded in the time it took the light from those
#' galaxies to reach the observatory.
#' 
#' Due to something involving gravitational effects, *only some space
#' expands*. In fact, the result is that *any rows or columns that contain
#' no galaxies* should all actually be twice as big.
#' 
#' In the above example, three columns and two rows contain no galaxies:
#' 
#'        v  v  v
#'      ...#......
#'      .......#..
#'      #.........
#'     >..........<
#'      ......#...
#'      .#........
#'      .........#
#'     >..........<
#'      .......#..
#'      #...#.....
#'        ^  ^  ^
#' 
#' These rows and columns need to be *twice as big*; the result of cosmic
#' expansion therefore looks like this:
#' 
#'     ....#........
#'     .........#...
#'     #............
#'     .............
#'     .............
#'     ........#....
#'     .#...........
#'     ............#
#'     .............
#'     .............
#'     .........#...
#'     #....#.......
#' 
#' Equipped with this expanded universe, the shortest path between every
#' pair of galaxies can be found. It can help to assign every galaxy a
#' unique number:
#' 
#'     ....1........
#'     .........2...
#'     3............
#'     .............
#'     .............
#'     ........4....
#'     .5...........
#'     ............6
#'     .............
#'     .............
#'     .........7...
#'     8....9.......
#' 
#' In these 9 galaxies, there are *36 pairs*. Only count each pair once;
#' order within the pair doesn\'t matter. For each pair, find any shortest
#' path between the two galaxies using only steps that move up, down, left,
#' or right exactly one `.` or `#` at a time. (The shortest path between
#' two galaxies is allowed to pass through another galaxy.)
#' 
#' For example, here is one of the shortest paths between galaxies `5` and
#' `9`:
#' 
#'     ....1........
#'     .........2...
#'     3............
#'     .............
#'     .............
#'     ........4....
#'     .5...........
#'     .##.........6
#'     ..##.........
#'     ...##........
#'     ....##...7...
#'     8....9.......
#' 
#' This path has length *`9`* because it takes a minimum of *nine steps* to
#' get from galaxy `5` to galaxy `9` (the eight locations marked `#` plus
#' the step onto galaxy `9` itself). Here are some other example shortest
#' path lengths:
#' 
#' -   Between galaxy `1` and galaxy `7`: 15
#' -   Between galaxy `3` and galaxy `6`: 17
#' -   Between galaxy `8` and galaxy `9`: 5
#' 
#' In this example, after expanding the universe, the sum of the shortest
#' path between all 36 pairs of galaxies is *`374`*.
#' 
#' Expand the universe, then find the length of the shortest path between
#' every pair of galaxies. *What is the sum of these lengths?*
#'
#' **Part Two**
#' 
#' The galaxies are much *older* (and thus much *farther apart*) than the
#' researcher initially estimated.
#' 
#' Now, instead of the expansion you did before, make each empty row or
#' column *[one
#' million]{title="And you have to have your pinky near your mouth when you do it."}
#' times* larger. That is, each empty row should be replaced with `1000000`
#' empty rows, and each empty column should be replaced with `1000000`
#' empty columns.
#' 
#' (In the example above, if each empty row or column were merely `10`
#' times larger, the sum of the shortest paths between every pair of
#' galaxies would be *`1030`*. If each empty row or column were merely
#' `100` times larger, the sum of the shortest paths between every pair of
#' galaxies would be *`8410`*. However, your universe will need to expand
#' far beyond these values.)
#' 
#' Starting with the same initial image, expand the universe according to
#' these new rules, then find the length of the shortest path between every
#' pair of galaxies. *What is the sum of these lengths?*
#'
#' @param x some data
#' @return For Part One, `f11a(x)` returns .... For Part Two,
#'   `f11b(x)` returns ....
#' @export
#' @examples
#' f11a(example_data_11())
#' f11b()
f11a <- function(x) {
  f11_helper(x)
}


#' @rdname day11
#' @export
f11b <- function(x) {
  xx <- seq(5, 1000, by = 50)
  dat <- sapply(xx, \(z) f11_helper(x, expand = z))
  fit <- lm(dat ~ xx)
  coefficients(fit)[1] + (1000000-1)*coefficients(fit)[2]
}


f11_helper <- function(x, expand = 1) {
  m <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  erow <- 1+expand*apply(m, 1, \(y) !any(y == "#"))
  newrows <- rep(1:nrow(m), erow)  
  m <- m[newrows, ]
  ecol <- 1+expand*apply(m, 2, \(y) !any(y == "#"))
  newcols <- rep(1:ncol(m), ecol)  
  m <- m[, newcols]
  
  locs <- which(m == "#", arr.ind = TRUE)
  s <- 0
  for (l1 in seq_len(nrow(locs))) {
    for (l2 in seq_len(nrow(locs))) {
      s <- s + sum(abs(locs[l1, ] - locs[l2, ]))
    }
  }
  s/2
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export
example_data_11 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
