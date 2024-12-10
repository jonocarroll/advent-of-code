#' Day 08: Resonant Collinearity
#'
#' [Resonant Collinearity](https://adventofcode.com/2024/day/8)
#'
#' @name day08
#' @rdname day08
#' @details
#'
#' **Part One**
#'
#' You find yourselves on the [roof](/2016/day/25) of a top-secret Easter
#' Bunny installation.
#' 
#' While The Historians do their thing, you take a look at the familiar
#' *huge antenna*. Much to your surprise, it seems to have been
#' reconfigured to emit a signal that makes people 0.1% more likely to buy
#' Easter Bunny brand [Imitation
#' Mediocre]{title="They could have imitated delicious chocolate, but the mediocre chocolate is WAY easier to imitate."}
#' Chocolate as a Christmas gift! Unthinkable!
#' 
#' Scanning across the city, you find that there are actually many such
#' antennas. Each antenna is tuned to a specific *frequency* indicated by a
#' single lowercase letter, uppercase letter, or digit. You create a map
#' (your puzzle input) of these antennas. For example:
#' 
#'     ............
#'     ........0...
#'     .....0......
#'     .......0....
#'     ....0.......
#'     ......A.....
#'     ............
#'     ............
#'     ........A...
#'     .........A..
#'     ............
#'     ............
#' 
#' The signal only applies its nefarious effect at specific *antinodes*
#' based on the resonant frequencies of the antennas. In particular, an
#' antinode occurs at any point that is perfectly in line with two antennas
#' of the same frequency - but only when one of the antennas is twice as
#' far away as the other. This means that for any pair of antennas with the
#' same frequency, there are two antinodes, one on either side of them.
#' 
#' So, for these two antennas with frequency `a`, they create the two
#' antinodes marked with `#`:
#' 
#'     ..........
#'     ...#......
#'     ..........
#'     ....a.....
#'     ..........
#'     .....a....
#'     ..........
#'     ......#...
#'     ..........
#'     ..........
#' 
#' Adding a third antenna with the same frequency creates several more
#' antinodes. It would ideally add four antinodes, but two are off the
#' right side of the map, so instead it adds only two:
#' 
#'     ..........
#'     ...#......
#'     #.........
#'     ....a.....
#'     ........a.
#'     .....a....
#'     ..#.......
#'     ......#...
#'     ..........
#'     ..........
#' 
#' Antennas with different frequencies don\'t create antinodes; `A` and `a`
#' count as different frequencies. However, antinodes *can* occur at
#' locations that contain antennas. In this diagram, the lone antenna with
#' frequency capital `A` creates no antinodes but has a
#' lowercase-`a`-frequency antinode at its location:
#' 
#'     ..........
#'     ...#......
#'     #.........
#'     ....a.....
#'     ........a.
#'     .....a....
#'     ..#.......
#'     ......A...
#'     ..........
#'     ..........
#' 
#' The first example has antennas with two different frequencies, so the
#' antinodes they create look like this, plus an antinode overlapping the
#' topmost `A`-frequency antenna:
#' 
#'     ......#....#
#'     ...#....0...
#'     ....#0....#.
#'     ..#....0....
#'     ....0....#..
#'     .#....A.....
#'     ...#........
#'     #......#....
#'     ........A...
#'     .........A..
#'     ..........#.
#'     ..........#.
#' 
#' Because the topmost `A`-frequency antenna overlaps with a `0`-frequency
#' antinode, there are *`14`* total unique locations that contain an
#' antinode within the bounds of the map.
#' 
#' Calculate the impact of the signal. *How many unique locations within
#' the bounds of the map contain an antinode?*
#'
#' **Part Two**
#' 
#' Watching over your shoulder as you work, one of The Historians asks if
#' you took the effects of resonant harmonics into your calculations.
#' 
#' Whoops!
#' 
#' After updating your model, it turns out that an antinode occurs at *any
#' grid position* exactly in line with at least two antennas of the same
#' frequency, regardless of distance. This means that some of the new
#' antinodes will occur at the position of each antenna (unless that
#' antenna is the only one of its frequency).
#' 
#' So, these three `T`-frequency antennas now create many antinodes:
#' 
#'     T....#....
#'     ...T......
#'     .T....#...
#'     .........#
#'     ..#.......
#'     ..........
#'     ...#......
#'     ..........
#'     ....#.....
#'     ..........
#' 
#' In fact, the three `T`-frequency antennas are all exactly in line with
#' two antennas, so they are all also antinodes! This brings the total
#' number of antinodes in the above example to *`9`*.
#' 
#' The original example now has *`34`* antinodes, including the antinodes
#' that appear on every antenna:
#' 
#'     ##....#....#
#'     .#.#....0...
#'     ..#.#0....#.
#'     ..##...0....
#'     ....0....#..
#'     .#...#A....#
#'     ...#..#.....
#'     #....#.#....
#'     ..#.....A...
#'     ....#....A..
#'     .#........#.
#'     ...#......##
#' 
#' Calculate the impact of the signal using this updated model. *How many
#' unique locations within the bounds of the map contain an antinode?*
#'
#' @param x some data
#' @return For Part One, `f08a(x)` returns .... For Part Two,
#'   `f08b(x)` returns ....
#' @export
#' @examples
#' f08a(example_data_08())
#' f08b()
f08a <- function() {
  solve(c(1, -2))
}

solve <- function(r) {
  x <- readLines("inst/input08.txt")
  x <- readLines("../tmp.txt")
  x <- matrix(strsplit(paste(x, collapse = ""), "")[[1]], ncol = nchar(x[1]), byrow = TRUE)
  zz <- x
  zz[] <- "."
  
  for (sym in unique(setdiff(c(x), "."))) {
    z <- which(x == sym, arr.ind = T)
    if (nrow(z) == 1) next
    mx <- matrix(nrow = nrow(z), ncol = nrow(z))
    my <- matrix(nrow = nrow(z), ncol = nrow(z))
    for (i in seq_len(nrow(z))) {
      for (j in seq_len(nrow(z))) {
        if (i >= j) next
        mx[i, j] <- z[i, 1] - z[j, 1]
        my[i, j] <- z[i, 2] - z[j, 2]
      }
    }
    
    for (i in seq_len(nrow(z))) {
      for (j in seq_len(nrow(z))) {
        if (i >= j) next
        if (is.na(mx[i, j])) next
        if (is.na(my[i, j])) next
        for (k in r) {
          if (inbounds(zz, z[i, 1] + k*mx[i,j], z[i, 2] + k*my[i, j])) {
            zz[z[i, 1] + k*mx[i,j], z[i, 2] + k*my[i, j]] <- "#"
          }
        }
      }
    }
  }
  sum(zz == "#")
  
}

inbounds <- function(z, x, y) {
  x > 0 & y > 0 & x <= nrow(z) & y <= ncol(z)
}

#' @rdname day08
#' @export
f08b <- function() {
  solve(-60:60)
}


f08_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day08
#' @export
example_data_08 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
