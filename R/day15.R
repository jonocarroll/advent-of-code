#' Day 15: Beacon Exclusion Zone
#'
#' [Beacon Exclusion Zone](https://adventofcode.com/2022/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You feel the ground rumble again as the distress signal leads you to a
#' large network of subterranean tunnels. You don\'t have time to search
#' them all, but you don\'t need to: your pack contains a set of deployable
#' *sensors* that you imagine were originally built to locate lost Elves.
#'
#' The sensors aren\'t very powerful, but that\'s okay; your handheld
#' device indicates that you\'re close enough to the source of the distress
#' signal to use them. You pull the emergency sensor system out of your
#' pack, hit the big button on top, and the sensors zoom off down the
#' tunnels.
#'
#' Once a sensor finds a spot it thinks will give it a good reading, it
#' attaches itself to a hard surface and begins monitoring for the nearest
#' signal source *beacon*. Sensors and beacons always exist at integer
#' coordinates. Each sensor knows its own position and can *determine the
#' position of a beacon precisely*; however, sensors can only lock on to
#' the one beacon *closest to the sensor* as measured by the [Manhattan
#' distance](https://en.wikipedia.org/wiki/Taxicab_geometry){target="_blank"}.
#' (There is never a tie where two beacons are the same distance to a
#' sensor.)
#'
#' It doesn\'t take long for the sensors to report back their positions and
#' closest beacons (your puzzle input). For example:
#'
#'     Sensor at x=2, y=18: closest beacon is at x=-2, y=15
#'     Sensor at x=9, y=16: closest beacon is at x=10, y=16
#'     Sensor at x=13, y=2: closest beacon is at x=15, y=3
#'     Sensor at x=12, y=14: closest beacon is at x=10, y=16
#'     Sensor at x=10, y=20: closest beacon is at x=10, y=16
#'     Sensor at x=14, y=17: closest beacon is at x=10, y=16
#'     Sensor at x=8, y=7: closest beacon is at x=2, y=10
#'     Sensor at x=2, y=0: closest beacon is at x=2, y=10
#'     Sensor at x=0, y=11: closest beacon is at x=2, y=10
#'     Sensor at x=20, y=14: closest beacon is at x=25, y=17
#'     Sensor at x=17, y=20: closest beacon is at x=21, y=22
#'     Sensor at x=16, y=7: closest beacon is at x=15, y=3
#'     Sensor at x=14, y=3: closest beacon is at x=15, y=3
#'     Sensor at x=20, y=1: closest beacon is at x=15, y=3
#'
#' So, consider the sensor at `2,18`; the closest beacon to it is at
#' `-2,15`. For the sensor at `9,16`, the closest beacon to it is at
#' `10,16`.
#'
#' Drawing sensors as `S` and beacons as `B`, the above arrangement of
#' sensors and beacons looks like this:
#'
#'                    1    1    2    2
#'          0    5    0    5    0    5
#'      0 ....S.......................
#'      1 ......................S.....
#'      2 ...............S............
#'      3 ................SB..........
#'      4 ............................
#'      5 ............................
#'      6 ............................
#'      7 ..........S.......S.........
#'      8 ............................
#'      9 ............................
#'     10 ....B.......................
#'     11 ..S.........................
#'     12 ............................
#'     13 ............................
#'     14 ..............S.......S.....
#'     15 B...........................
#'     16 ...........SB...............
#'     17 ................S..........B
#'     18 ....S.......................
#'     19 ............................
#'     20 ............S......S........
#'     21 ............................
#'     22 .......................B....
#'
#' This isn\'t necessarily a comprehensive map of all beacons in the area,
#' though. Because each sensor only identifies its closest beacon, if a
#' sensor detects a beacon, you know there are no other beacons that close
#' or closer to that sensor. There could still be beacons that just happen
#' to not be the closest beacon to any sensor. Consider the sensor at
#' `8,7`:
#'
#'                    1    1    2    2
#'          0    5    0    5    0    5
#'     -2 ..........#.................
#'     -1 .........###................
#'      0 ....S...#####...............
#'      1 .......#######........S.....
#'      2 ......#########S............
#'      3 .....###########SB..........
#'      4 ....#############...........
#'      5 ...###############..........
#'      6 ..#################.........
#'      7 .#########S#######S#........
#'      8 ..#################.........
#'      9 ...###############..........
#'     10 ....B############...........
#'     11 ..S..###########............
#'     12 ......#########.............
#'     13 .......#######..............
#'     14 ........#####.S.......S.....
#'     15 B........###................
#'     16 ..........#SB...............
#'     17 ................S..........B
#'     18 ....S.......................
#'     19 ............................
#'     20 ............S......S........
#'     21 ............................
#'     22 .......................B....
#'
#' This sensor\'s closest beacon is at `2,10`, and so you know there are no
#' beacons that close or closer (in any positions marked `#`).
#'
#' None of the detected beacons seem to be producing the distress signal,
#' so you\'ll need to [work
#' out]{title="\"When you have eliminated all which is impossible, then whatever remains, however improbable, must be where the missing beacon is.\" - Sherlock Holmes"}
#' where the distress beacon is by working out where it *isn\'t*. For now,
#' keep things simple by counting the positions where a beacon cannot
#' possibly be along just a single row.
#'
#' So, suppose you have an arrangement of beacons and sensors like in the
#' example above and, just in the row where `y=10`, you\'d like to count
#' the number of positions a beacon cannot possibly exist. The coverage
#' from all sensors near that row looks like this:
#'
#'                      1    1    2    2
#'            0    5    0    5    0    5
#'      9 ...#########################...
#'     10 ..####B######################..
#'     11 .###S#############.###########.
#'
#' In this example, in the row where `y=10`, there are *`26`* positions
#' where a beacon cannot be present.
#'
#' Consult the report from the sensors you just deployed. *In the row where
#' `y=2000000`, how many positions cannot contain a beacon?*
#'
#' **Part Two**
#'
#' Your handheld device indicates that the distress signal is coming from a
#' beacon nearby. The distress beacon is not detected by any sensor, but
#' the distress beacon must have `x` and `y` coordinates each no lower than
#' `0` and no larger than `4000000`.
#'
#' To isolate the distress beacon\'s signal, you need to determine its
#' *tuning frequency*, which can be found by multiplying its `x` coordinate
#' by `4000000` and then adding its `y` coordinate.
#'
#' In the example above, the search space is smaller: instead, the `x` and
#' `y` coordinates can each be at most `20`. With this reduced search area,
#' there is only a single position that could have a beacon: `x=14, y=11`.
#' The tuning frequency for this distress beacon is *`56000011`*.
#'
#' Find the only possible position for the distress beacon. *What is its
#' tuning frequency?*
#'
#' @param x some data
#' @return For Part One, `f15a(x)` returns .... For Part Two,
#'   `f15b(x)` returns ....
#' @export
#' @examples
#' f15a(example_data_15())
#' f15b()
f15a <- function(x, at, maxsize = NULL) {
  allsignals <- lapply(x, signal)
  dists <- lapply(allsignals, manhattan)
  filled <- vonneumann(allsignals[[1]][[1]], dists[[1]], target = at)
  if (!is.null(filled)) {
    is_filled <- filled[1]:filled[2]
  } else {
    is_filled <- c()
  }
  for (i in 2:length(allsignals)) {
    filled <- vonneumann(allsignals[[i]][[1]], dists[[i]], target = at)
    if (!is.null(filled)) {
      is_filled <- c(is_filled, filled[1]:filled[2])
    }
  }

  beacons <- t(sapply(allsignals, "[[", 2))
  n_beacons_at = nrow(unique(beacons[beacons[, 2] == at, , drop = FALSE]))
  n_signals_at = length(unique(is_filled))
  return(n_signals_at - n_beacons_at)
}

#' @rdname day15
#' @export
f15b <- function(x, maxsize) {
  allsignals <- lapply(x, signal)
  dists <- lapply(allsignals, manhattan)

  printat <- if (maxsize < 50) {
    seq(1, maxsize)
  } else {
    seq(1e4, maxsize, 1e4)
  }
  for (i in rev(0:maxsize)) {
    if (i %in% printat) cat("\r", round(100 * i / maxsize, 2), "%")
    ret <- f15b_helper(x, i, maxsize = maxsize, allsignals, dists)
    if (length(ret)) {
      message("")
      message("ret = ", ret, "; i = ", i)
      return(format((4e6*ret) + i, scientific = FALSE))
    }
  }
  FALSE
}

f15b_helper <- function(x, at, maxsize, allsignals, dists) {
  filled <- vonneumann(allsignals[[1]][[1]], dists[[1]], target = at)
  if (!is.null(filled)) {
    has_signal <- list(filled)
  } else {
    has_signal <- list()
  }
  for (i in 2:length(allsignals)) {
    filled <- vonneumann(allsignals[[i]][[1]], dists[[i]], target = at)
    if (!is.null(filled)) {
      has_signal <- c(has_signal, list(filled))
    }
  }

  has_signal_mat <- t(sapply(has_signal, as.matrix))
  has_signal_mat <- has_signal_mat[order(has_signal_mat[, 1], has_signal_mat[, 2]), ]
  interval <- has_signal_mat[1, ]
  for (r in seq_len(nrow(has_signal_mat))) {
    # https://stackoverflow.com/questions/325933/determine-whether-two-date-ranges-overlap/325964#325964
    if (has_signal_mat[r, 1] <= (interval[2]+1) && has_signal_mat[r, 2] >= (interval[1]-1)) {
      interval <- c(min(interval[1], has_signal_mat[r, 1]), max(interval[2], has_signal_mat[r, 2]))
    } else {
      return(has_signal_mat[r, 1] - 1)

    }
  }

  NULL
}

manhattan <- function(plist) {
  p1 <- plist[[1]]
  p2 <- plist[[2]]
  abs(p1[1] - p2[1]) + abs(p1[2] - p2[2])
}

vonneumann <- function(p, d, target) {
  # if the signal can't reach the target row, it's useless
  if (abs(p[2] - target) > d) return(NULL)
  if (p[2] == target) {
    return(c(p[1]-d, p[1]+d))
  }
  # dist to target
  dist_to_target <- target - p[2]
  return(c(p[1] - d + abs(dist_to_target), p[1] + d - abs(dist_to_target)))
}

signal <- function(s) {
  x1 = as.integer(sub("Sensor at x=(-?[0-9]*),.*", "\\1", s))
  y1 = as.integer(sub(".*?y=(-?[0-9]*):.*", "\\1", s))
  x2 = as.integer(sub(".*?is at x=(-?[0-9]*),.*", "\\1", s))
  y2 = as.integer(sub(".*?is at x=-?[0-9]*, y=(-?[0-9]*)", "\\1", s))
  return(list(c(x1, y1), c(x2, y2)))
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example15.txt")
  )
  l[[example]]
}
