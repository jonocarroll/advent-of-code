#' Day 17: Pyroclastic Flow
#'
#' [Pyroclastic Flow](https://adventofcode.com/2022/day/17)
#'
#' @name day17
#' @rdname day17
#' @details
#'
#' **Part One**
#'
#' Your handheld device has located an alternative exit from the cave for
#' you and the elephants. The ground is rumbling almost continuously now,
#' but the strange valves bought you some time. It\'s definitely getting
#' warmer in here, though.
#'
#' The tunnels eventually open into a very tall, narrow chamber. Large,
#' oddly-shaped rocks are falling into the chamber from above, presumably
#' due to all the rumbling. If you can\'t work out where the rocks will
#' fall next, you might be
#' [crushed]{title="I am the man who arranges the blocks / that descend upon me from up above!"}!
#'
#' The five types of rocks have the following peculiar shapes, where `#` is
#' rock and `.` is empty space:
#'
#'     ####
#'
#'     .#.
#'     ###
#'     .#.
#'
#'     ..#
#'     ..#
#'     ###
#'
#'     #
#'     #
#'     #
#'     #
#'
#'     ##
#'     ##
#'
#' The rocks fall in the order shown above: first the `-` shape, then the
#' `+` shape, and so on. Once the end of the list is reached, the same
#' order repeats: the `-` shape falls first, sixth, 11th, 16th, etc.
#'
#' The rocks don\'t spin, but they do get pushed around by jets of hot gas
#' coming out of the walls themselves. A quick scan reveals the effect the
#' jets of hot gas will have on the rocks as they fall (your puzzle input).
#'
#' For example, suppose this was the jet pattern in your cave:
#'
#'     >>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>
#'
#' In jet patterns, `<` means a push to the left, while `>` means a push to
#' the right. The pattern above means that the jets will push a falling
#' rock right, then right, then right, then left, then left, then right,
#' and so on. If the end of the list is reached, it repeats.
#'
#' The tall, vertical chamber is exactly *seven units wide*. Each rock
#' appears so that its left edge is two units away from the left wall and
#' its bottom edge is three units above the highest rock in the room (or
#' the floor, if there isn\'t one).
#'
#' After a rock appears, it alternates between *being pushed by a jet of
#' hot gas* one unit (in the direction indicated by the next symbol in the
#' jet pattern) and then *falling one unit down*. If any movement would
#' cause any part of the rock to move into the walls, floor, or a stopped
#' rock, the movement instead does not occur. If a *downward* movement
#' would have caused a falling rock to move into the floor or an
#' already-fallen rock, the falling rock stops where it is (having landed
#' on something) and a new rock immediately begins falling.
#'
#' Drawing falling rocks with `@` and stopped rocks with `#`, the jet
#' pattern in the example above manifests as follows:
#'
#'     The first rock begins falling:
#'     |..@@@@.|
#'     |.......|
#'     |.......|
#'     |.......|
#'     +-------+
#'
#'     Jet of gas pushes rock right:
#'     |...@@@@|
#'     |.......|
#'     |.......|
#'     |.......|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |...@@@@|
#'     |.......|
#'     |.......|
#'     +-------+
#'
#'     Jet of gas pushes rock right, but nothing happens:
#'     |...@@@@|
#'     |.......|
#'     |.......|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |...@@@@|
#'     |.......|
#'     +-------+
#'
#'     Jet of gas pushes rock right, but nothing happens:
#'     |...@@@@|
#'     |.......|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |...@@@@|
#'     +-------+
#'
#'     Jet of gas pushes rock left:
#'     |..@@@@.|
#'     +-------+
#'
#'     Rock falls 1 unit, causing it to come to rest:
#'     |..####.|
#'     +-------+
#'
#'     A new rock begins falling:
#'     |...@...|
#'     |..@@@..|
#'     |...@...|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Jet of gas pushes rock left:
#'     |..@....|
#'     |.@@@...|
#'     |..@....|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |..@....|
#'     |.@@@...|
#'     |..@....|
#'     |.......|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Jet of gas pushes rock right:
#'     |...@...|
#'     |..@@@..|
#'     |...@...|
#'     |.......|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |...@...|
#'     |..@@@..|
#'     |...@...|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Jet of gas pushes rock left:
#'     |..@....|
#'     |.@@@...|
#'     |..@....|
#'     |.......|
#'     |..####.|
#'     +-------+
#'
#'     Rock falls 1 unit:
#'     |..@....|
#'     |.@@@...|
#'     |..@....|
#'     |..####.|
#'     +-------+
#'
#'     Jet of gas pushes rock right:
#'     |...@...|
#'     |..@@@..|
#'     |...@...|
#'     |..####.|
#'     +-------+
#'
#'     Rock falls 1 unit, causing it to come to rest:
#'     |...#...|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     A new rock begins falling:
#'     |....@..|
#'     |....@..|
#'     |..@@@..|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |...#...|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#' The moment each of the next few rocks begins falling, you would see
#' this:
#'
#'     |..@....|
#'     |..@....|
#'     |..@....|
#'     |..@....|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |..#....|
#'     |..#....|
#'     |####...|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |..@@...|
#'     |..@@...|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |..@@@@.|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |...@...|
#'     |..@@@..|
#'     |...@...|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |.####..|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |....@..|
#'     |....@..|
#'     |..@@@..|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |..#....|
#'     |.###...|
#'     |..#....|
#'     |.####..|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |..@....|
#'     |..@....|
#'     |..@....|
#'     |..@....|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |.....#.|
#'     |.....#.|
#'     |..####.|
#'     |.###...|
#'     |..#....|
#'     |.####..|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |..@@...|
#'     |..@@...|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |....#..|
#'     |....#..|
#'     |....##.|
#'     |....##.|
#'     |..####.|
#'     |.###...|
#'     |..#....|
#'     |.####..|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#'     |..@@@@.|
#'     |.......|
#'     |.......|
#'     |.......|
#'     |....#..|
#'     |....#..|
#'     |....##.|
#'     |##..##.|
#'     |######.|
#'     |.###...|
#'     |..#....|
#'     |.####..|
#'     |....##.|
#'     |....##.|
#'     |....#..|
#'     |..#.#..|
#'     |..#.#..|
#'     |#####..|
#'     |..###..|
#'     |...#...|
#'     |..####.|
#'     +-------+
#'
#' To prove to the elephants your simulation is accurate, they want to know
#' how tall the tower will get after 2022 rocks have stopped (but before
#' the 2023rd rock begins falling). In this example, the tower of rocks
#' will be *`3068`* units tall.
#'
#' *How many units tall will the tower of rocks be after 2022 rocks have
#' stopped falling?*
#'
#' **Part Two**
#'
#' The elephants are not impressed by your simulation. They demand to know
#' how tall the tower will be after *`1000000000000`* rocks have stopped!
#' Only then will they feel confident enough to proceed through the cave.
#'
#' In the example above, the tower would be *`1514285714288`* units tall!
#'
#' *How tall will the tower be after `1000000000000` rocks have stopped?*
#'
#' @param x some data
#' @return For Part One, `f17a(x)` returns .... For Part Two,
#'   `f17b(x)` returns ....
#' @export
#' @examples
#' f17a(example_data_17())
#' f17b()
f17a <- function(x, n, ret = c("floor", "heights", "chamber")) {
  ret <- match.arg(ret)
  push <- strsplit(x, "")[[1]]
  #' The tall, vertical chamber is exactly *seven units wide*. Each rock
  #' appears so that its left edge is two units away from the left wall and
  #' its bottom edge is three units above the highest rock in the room (or
  #' the floor, if there isn\'t one).
  chamber <- matrix(0, ncol = 9, nrow = 2e4)
  chamber[, 1] <- 1
  chamber[, 9] <- 1

  r <- 0
  j <- 0
  floor <- 1
  chamber[floor, ] <- 1
  lower_left <- c(floor + 3, 3) + 1
  maxheights <- c()

  while (r < n) {

    rocknum <- r %% 5 + 1
    nextrock <- rocks[[rocknum]]

    # push
    nextpush <- push[j %% length(push) + 1]
    dir <- ifelse(nextpush == ">", 1, -1)
    if (!crash(lower_left + c(0, dir), nextrock, chamber)) {
      # message("dir = ", nextpush)
      lower_left <- lower_left + c(0, dir)
    } else {
      # message("crash ", nextpush)
    }

    # fall
    if (!crash(lower_left + c(-1, 0), nextrock, chamber)) {
      # message("fall")
      lower_left <- lower_left + c(-1, 0)
    } else {
      # message("crash down")
      chamber <- rest(lower_left, nextrock, chamber)
      floor <- max(apply(chamber[, 2:8], 2, \(x) max(which(x != 0))))
      maxheights <- c(maxheights, floor)
      r <- r + 1
      lower_left <- c(floor + 3, 3) + 1
    }

    j <- j + 1

  }

  if (ret == "heights") {
    maxheights - 1
  } else if (ret == "floor") {
    floor - 1
  } else if (ret == "chamber") {
    chamber[1:floor, ]
  }

}


#' @rdname day17
#' @export
f17b <- function(x) {
  a <- f17a(x, 1e4, "heights")
  message("games complete")
  lags <- sapply(seq_along(a)[-1], \(y) a[y] - a[y-1])
  cycles <- detect_cycle(lags)
  if (!is.null(cycles)) message ("cycles found")
  ## assume first cycle is corrupted by the floor
  height_start <- a[cycles$start + cycles$len - 1]
  height_each_cycle <- a[cycles$start + 2*cycles$len - 1] - height_start
  z <- 1000000000000
  offset <- z - (cycles$start - 1)
  n_mod <- ((offset - 1) %% cycles$len) + 1
  n_cyc <- floor((offset) / cycles$len) - 2
  format(a[cycles$start + 2*cycles$len - 1 + n_mod] + n_cyc*height_each_cycle, scientific = FALSE)
}

detect_cycle <- function(x) {
  # if the data is cyclic, a matrix of the data will have the same value in every column
  for (len in 2:3000) {
    if ((4*len - 1) > length(x)) break
    for (start in 1:500) {
      if ((start + 4*len - 1) > length(x)) break
      m <- matrix(x[start:(start + 4*len - 1)], ncol = len, byrow = TRUE)
      u <- 0
      for (r in 1:len) {
        if (!length(unique(m[, r])) == 1) break
        u <- u + 1
      }
      # if (all(apply(, 2, \(y) length(unique(y))) == 1)) {
      if (u == len) return(list(start = start, len = len, seq = x[start:(start + len - 1)]))
    }
  }
}

#
# # based on https://en.wikipedia.org/wiki/Cycle_detection
# floyd <- function(x) {
#   # Main phase of algorithm: finding a repetition x_i = x_2i.
#   # The hare moves twice as quickly as the tortoise and
#   # the distance between them increases by 1 at each step.
#   # Eventually they will both be inside the cycle and then,
#   # at some point, the distance between them will be
#   # divisible by the period λ.
#   # tortoise = f(x0) # f(x0) is the element/node next to x0.
#   tortoise <- 2
#   # hare = f(f(x0))
#   hare <- 3
#   # while tortoise != hare:
#   #   tortoise = f(tortoise)
#   # hare = f(f(hare))
#   while (x[tortoise] != x[hare]) {
#     tortoise <- tortoise + 1
#     hare <- hare + 2
#   }
#
#   # At this point the tortoise position, ν, which is also equal
#   # to the distance between hare and tortoise, is divisible by
#   # the period λ. So hare moving in circle one step at a time,
#   # and tortoise (reset to x0) moving towards the circle, will
#   # intersect at the beginning of the circle. Because the
#   # distance between them is constant at 2ν, a multiple of λ,
#   # they will agree as soon as the tortoise reaches index μ.
#
#   # Find the position μ of first repetition.
#   mu <- 1
#   # tortoise = x0
#   tortoise <- 1
#
#   # while tortoise != hare:
#   #   tortoise = f(tortoise)
#   # hare = f(hare)   # Hare and tortoise move at same speed
#   # mu += 1
#   while (x[tortoise] != x[hare]) {
#     tortoise <- tortoise + 1
#     hare <- hare + 1
#     mu <- mu + 1
#   }
#
#   # Find the length of the shortest cycle starting from x_μ
#   # The hare moves one step at a time while tortoise is still.
#   # lam is incremented until λ is found.
#   lambda <- 1
#   # hare = f(tortoise)
#   hare <- tortoise + 1
#   # while tortoise != hare:
#   #   hare = f(hare)
#   # lam += 1
#   while (x[tortoise] != x[hare]) {
#     hare <- hare + 1
#     lambda <- lambda + 1
#   }
#
#   return(list(length = lambda, start = mu, seq = x[mu:(mu + lambda - 1)]))
# }
#
# # https://stackoverflow.com/a/16537008/4168169
# vecIn <- function(a,b){
#   which(
#     Reduce('+', lapply(seq_along(y <- lapply(b, '==', a)), function(x){
#       y[[x]][x:(length(a) - length(b) +x)]
#     }
#     )
#     ) == length(b)
#   )
# }

f17_helper <- function(x) {

}

#
# #Checks for the largest common prefix
# # https://www.javatpoint.com/program-to-find-longest-repeating-sequence-in-a-string
# lcp <- function(s, t) {
#   n = min(nchar(s), nchar(t))
#   for (i in 1:n) {
#     si <- substring(s, i, i)
#     ti <- substring(t, i, i)
#     if (si != ti) {
#       return(substring(s, 1, i-1))
#     }
#   }
#   return(substring(s, 1, n))
# }
#
# lcp_i <- function(s, t) {
#   n = min(length(s), length(t))
#   for (i in 1:n) {
#     if (s[i] != t[i]) {
#       return(s[1:(i-1)])
#     }
#   }
#   return(s[1:n])
# }
#
# # find the longest repeated sequence
# # https://www.javatpoint.com/program-to-find-longest-repeating-sequence-in-a-string
# lrs <- function(s) {
#   ret <- ""
#   n = nchar(s)
#   for (i in 1:n) {
#     for (j in (i+1):n) {
#       x = lcp(substring(s, i, n), substring(s, j, n))
#       if (nchar(x) >= nchar(ret)) {
#         ret <- x
#       }
#     }
#   }
#   ret
# }
#
# lrs_i <- function(s) {
#   ret <- c()
#   n = length(s)
#   for (i in 1:(n-1)) {
#     for (j in (i+1):(n-1)) {
#       x <- lcp_i(s[i:n], s[j:n])
#       if (length(x) >= length(ret)) {
#         ret <- x
#       }
#     }
#   }
#   ret
# }

crash <- function(ll, rock, chamber) {
  # chamber_pts <- as.matrix(expand.grid(ll[1] + 0:3, ll[2] + 0:3))
  # any(c(rock) & chamber[chamber_pts])
  rockidx <- which(rock != 0, arr.ind = TRUE)
  rockidx[, 1] <- rockidx[, 1] + ll[1] - 1
  rockidx[, 2] <- rockidx[, 2] + ll[2] - 1
  any(chamber[rockidx] != 0)
}

# need to deal with overwriting 0-
rest <- function(ll, rock, chamber) {
  rockno <- max(unique(c(rock)))
  rockidx <- which(rock != 0, arr.ind = TRUE)
  rockidx[, 1] <- rockidx[, 1] + ll[1] - 1
  rockidx[, 2] <- rockidx[, 2] + ll[2] - 1
  chamber[rockidx] <- rockno
  chamber
}

#'     ####
#'
#'     .#.
#'     ###
#'     .#.
#'
#'     ..#
#'     ..#
#'     ###
#'
#'     #
#'     #
#'     #
#'     #
#'
#'     ##
#'     ##
# inverted for matrix representation
rocks <- list(
  matrix(c(1, 1, 1, 1,
           0, 0, 0, 0,
           0, 0, 0, 0,
           0, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE),
  matrix(c(0, 2, 0, 0,
           2, 2, 2, 0,
           0, 2, 0, 0,
           0, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE),
  matrix(c(3, 3, 3, 0,
           0, 0, 3, 0,
           0, 0, 3, 0,
           0, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE),
  matrix(c(4, 0, 0, 0,
           4, 0, 0, 0,
           4, 0, 0, 0,
           4, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE),
  matrix(c(5, 5, 0, 0,
           5, 5, 0, 0,
           0, 0, 0, 0,
           0, 0, 0, 0), nrow = 4, ncol = 4, byrow = TRUE)
)

height <- function(r) {
  c(1, 3, 3, 4, 2)[r]
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day17
#' @export
example_data_17 <- function(example = 1) {
  l <- list(
    a = c(
      ">>><<><>><<<>><>>><<<>>><<<><<<>><>><<>>"
    )
  )
  l[[example]]
}
