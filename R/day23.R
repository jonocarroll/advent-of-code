#' Day 23: Unstable Diffusion
#'
#' [Unstable Diffusion](https://adventofcode.com/2022/day/23)
#'
#' @name day23
#' @rdname day23
#' @details
#'
#' **Part One**
#'
#' You enter a large crater of gray dirt where the grove is supposed to be.
#' All around you, plants you imagine were expected to be full of fruit are
#' instead withered and broken. A large group of Elves has formed in the
#' middle of the grove.
#'
#' \"\...but this volcano has been dormant for months. Without ash, the
#' fruit can\'t grow!\"
#'
#' You look up to see a massive, snow-capped mountain towering above you.
#'
#' \"It\'s not like there are other active volcanoes here; we\'ve looked
#' everywhere.\"
#'
#' \"But our scanners show active magma flows; clearly it\'s going
#' *somewhere*.\"
#'
#' They finally notice you at the edge of the grove, your pack almost
#' overflowing from the random *star* fruit you\'ve been collecting. Behind
#' you, elephants and monkeys explore the grove, looking concerned. Then,
#' the Elves recognize the ash cloud slowly spreading above your recent
#' detour.
#'
#' \"Why do you\--\" \"How is\--\" \"Did you just\--\"
#'
#' Before any of them can form a complete question, another Elf speaks up:
#' \"Okay, new plan. We have almost enough fruit already, and ash from the
#' plume should spread here eventually. If we quickly plant new seedlings
#' now, we can still make it to the extraction point. Spread out!\"
#'
#' The Elves each reach into their pack and pull out a tiny plant. The
#' plants rely on important nutrients from the ash, so they can\'t be
#' planted too close together.
#'
#' There isn\'t enough time to let the Elves figure out where to plant the
#' seedlings themselves; you quickly scan the grove (your puzzle input) and
#' note their positions.
#'
#' For example:
#'
#'     ....#..
#'     ..###.#
#'     #...#.#
#'     .#...##
#'     #.###..
#'     ##.#.##
#'     .#..#..
#'
#' The scan shows Elves `#` and empty ground `.`; outside your scan, more
#' empty ground extends a long way in every direction. The scan is oriented
#' so that *north is up*; orthogonal directions are written N (north), S
#' (south), W (west), and E (east), while diagonal directions are written
#' NE, NW, SE, SW.
#'
#' The Elves follow a time-consuming process to figure out where they
#' should each go; you can speed up this process considerably. The process
#' consists of some number of *rounds* during which Elves alternate between
#' considering where to move and actually moving.
#'
#' During the *first half* of each round, each Elf considers the eight
#' positions adjacent to themself. If no other Elves are in one of those
#' eight positions, the Elf *does not do anything* during this round.
#' Otherwise, the Elf looks in each of four directions in the following
#' order and *proposes* moving one step in the *first valid direction*:
#'
#' -   If there is no Elf in the N, NE, or NW adjacent positions, the Elf
#'     proposes moving *north* one step.
#' -   If there is no Elf in the S, SE, or SW adjacent positions, the Elf
#'     proposes moving *south* one step.
#' -   If there is no Elf in the W, NW, or SW adjacent positions, the Elf
#'     proposes moving *west* one step.
#' -   If there is no Elf in the E, NE, or SE adjacent positions, the Elf
#'     proposes moving *east* one step.
#'
#' After each Elf has had a chance to propose a move, the *second half* of
#' the round can begin. Simultaneously, each Elf moves to their proposed
#' destination tile if they were the *only* Elf to propose moving to that
#' position. If two or more Elves propose moving to the same position,
#' *none* of those Elves move.
#'
#' Finally, at the end of the round, the *first direction* the Elves
#' considered is moved to the end of the list of directions. For example,
#' during the second round, the Elves would try proposing a move to the
#' south first, then west, then east, then north. On the third round, the
#' Elves would first consider west, then east, then north, then south.
#'
#' As a smaller example, consider just these five Elves:
#'
#'     .....
#'     ..##.
#'     ..#..
#'     .....
#'     ..##.
#'     .....
#'
#' The northernmost two Elves and southernmost two Elves all propose moving
#' north, while the middle Elf cannot move north and proposes moving south.
#' The middle Elf proposes the same destination as the southwest Elf, so
#' neither of them move, but the other three do:
#'
#'     ..##.
#'     .....
#'     ..#..
#'     ...#.
#'     ..#..
#'     .....
#'
#' Next, the northernmost two Elves and the southernmost Elf all propose
#' moving south. Of the remaining middle two Elves, the west one cannot
#' move south and proposes moving west, while the east one cannot move
#' south *or* west and proposes moving east. All five Elves succeed in
#' moving to their proposed positions:
#'
#'     .....
#'     ..##.
#'     .#...
#'     ....#
#'     .....
#'     ..#..
#'
#' Finally, the southernmost two Elves choose not to move at all. Of the
#' remaining three Elves, the west one proposes moving west, the east one
#' proposes moving east, and the middle one proposes moving north; all
#' three succeed in moving:
#'
#'     ..#..
#'     ....#
#'     #....
#'     ....#
#'     .....
#'     ..#..
#'
#' At this point, no Elves need to move, and so the process ends.
#'
#' The larger example above proceeds as follows:
#'
#'     == Initial State ==
#'     ..............
#'     ..............
#'     .......#......
#'     .....###.#....
#'     ...#...#.#....
#'     ....#...##....
#'     ...#.###......
#'     ...##.#.##....
#'     ....#..#......
#'     ..............
#'     ..............
#'     ..............
#'
#'     == End of Round 1 ==
#'     ..............
#'     .......#......
#'     .....#...#....
#'     ...#..#.#.....
#'     .......#..#...
#'     ....#.#.##....
#'     ..#..#.#......
#'     ..#.#.#.##....
#'     ..............
#'     ....#..#......
#'     ..............
#'     ..............
#'
#'     == End of Round 2 ==
#'     ..............
#'     .......#......
#'     ....#.....#...
#'     ...#..#.#.....
#'     .......#...#..
#'     ...#..#.#.....
#'     .#...#.#.#....
#'     ..............
#'     ..#.#.#.##....
#'     ....#..#......
#'     ..............
#'     ..............
#'
#'     == End of Round 3 ==
#'     ..............
#'     .......#......
#'     .....#....#...
#'     ..#..#...#....
#'     .......#...#..
#'     ...#..#.#.....
#'     .#..#.....#...
#'     .......##.....
#'     ..##.#....#...
#'     ...#..........
#'     .......#......
#'     ..............
#'
#'     == End of Round 4 ==
#'     ..............
#'     .......#......
#'     ......#....#..
#'     ..#...##......
#'     ...#.....#.#..
#'     .........#....
#'     .#...###..#...
#'     ..#......#....
#'     ....##....#...
#'     ....#.........
#'     .......#......
#'     ..............
#'
#'     == End of Round 5 ==
#'     .......#......
#'     ..............
#'     ..#..#.....#..
#'     .........#....
#'     ......##...#..
#'     .#.#.####.....
#'     ...........#..
#'     ....##..#.....
#'     ..#...........
#'     ..........#...
#'     ....#..#......
#'     ..............
#'
#' After a few more rounds\...
#'
#'     == End of Round 10 ==
#'     .......#......
#'     ...........#..
#'     ..#.#..#......
#'     ......#.......
#'     ...#.....#..#.
#'     .#......##....
#'     .....##.......
#'     ..#........#..
#'     ....#.#..#....
#'     ..............
#'     ....#..#..#...
#'     ..............
#'
#' To make sure they\'re on the right track, the Elves like to check after
#' round 10 that they\'re making good progress toward covering enough
#' ground. To do this, count the number of empty ground tiles contained by
#' the smallest rectangle that contains every Elf. (The edges of the
#' rectangle should be aligned to the N/S/E/W directions; the Elves do not
#' have the patience to calculate [arbitrary
#' rectangles]{title="Arbitrary Rectangles is my Piet Mondrian cover band."}.)
#' In the above example, that rectangle is:
#'
#'     ......#.....
#'     ..........#.
#'     .#.#..#.....
#'     .....#......
#'     ..#.....#..#
#'     #......##...
#'     ....##......
#'     .#........#.
#'     ...#.#..#...
#'     ............
#'     ...#..#..#..
#'
#' In this region, the number of empty ground tiles is *`110`*.
#'
#' Simulate the Elves\' process and find the smallest rectangle that
#' contains the Elves after 10 rounds. *How many empty ground tiles does
#' that rectangle contain?*
#'
#' **Part Two**
#'
#' It seems you\'re on the right track. Finish simulating the process and
#' figure out where the Elves need to go. How many rounds did you save
#' them?
#'
#' In the example above, the *first round where no Elf moved* was round
#' *`20`*:
#'
#'     .......#......
#'     ....#......#..
#'     ..#.....#.....
#'     ......#.......
#'     ...#....#.#..#
#'     #.............
#'     ....#.....#...
#'     ..#.....#.....
#'     ....#.#....#..
#'     .........#....
#'     ....#......#..
#'     .......#......
#'
#' Figure out where the Elves need to go. *What is the number of the first
#' round where no Elf moves?*
#'
#' @param x some data
#' @return For Part One, `f23a(x)` returns .... For Part Two,
#'   `f23b(x)` returns ....
#' @export
#' @examples
#' f23a(example_data_23())
#' f23b()
f23a <- function(x) {
  elves <- x
  excess <- 50
  grid <- matrix(".", nrow = length(x) + 2*excess, ncol = length(x) + 2*excess)
  for (r in seq_along(elves)) {
    elfpos <- strsplit(elves[r], "")[[1]]
    for (i in seq_along(elfpos)) {
      grid[r + excess - 1, excess + i] <- elfpos[i]
    }
  }

  dirs <- list("N" = c(-1, 0), "S" = c(1, 0), "W" = c(0, -1), "E" = c(0, 1))

  for (i in 1:10) {
    tmp <- iterate(grid, dirs)
    grid <- tmp$grid
    dirs <- tmp$moveorder
  }

  rowrange <- suppressWarnings(apply(apply(grid, 2, \(y) range(grep("#", y))), 1, \(z) c(min(z), max(z))))
  rowrange <- unlist(rowrange)
  rowrange <- rowrange[is.finite(rowrange)]
  colrange <- suppressWarnings(apply(apply(grid, 1, \(y) range(grep("#", y))), 1, \(z) c(min(z), max(z))))
  colrange <- unlist(colrange)
  colrange <- colrange[is.finite(colrange)]
  rectgrid <- grid[rowrange[1]:rowrange[2], colrange[1]:colrange[2]]
  sum(rectgrid == ".")

}

crater <- function(len = 15, excess = 50) {
  grid[1:len + excess, 1:len +excess]
}

iterate <- function(grid, moveorder) {

  elves <- which(grid == "#", arr.ind = TRUE)
  propmat <- matrix(nrow = nrow(elves), ncol = 2)
  # elf <- 1
  elfdidntmove <- 0
  dirs <- as.matrix(expand.grid(-1:1, -1:1))
  dirs <- dirs[rowSums(abs(dirs)) != 0, ]
  for (elf in seq_len(nrow(elves))) {
    elfpos <- elves[elf, , drop = FALSE]
    otherelves <- t(apply(dirs, 1, \(y) elfpos + y))
    if (!any(grid[otherelves] == "#")) {
      propmat[elf, ] <- elfpos
      elfdidntmove <- elfdidntmove + 1
      next
    }

    propose <- if (testdir(grid, elfpos, names(moveorder)[1])) {
      elfpos + moveorder[[1]]
    } else if (testdir(grid, elfpos, names(moveorder)[2])) {
      elfpos + moveorder[[2]]
    } else if (testdir(grid, elfpos, names(moveorder)[3])) {
      elfpos + moveorder[[3]]
    } else if (testdir(grid, elfpos, names(moveorder)[4])) {
      elfpos + moveorder[[4]]
    } else {
      elfdidntmove <- elfdidntmove + 1
      elfpos
    }
    propmat[elf, ] <- propose
  }
  dupes <- propmat[duplicated(propmat), , drop = FALSE]
  dup_elves <- c()
  for (r in seq_len(nrow(dupes))) {
    dup_elves <- c(dup_elves, which(propmat[, 1] == dupes[r, 1] & propmat[, 2] == dupes[r, 2]))
  }
  if (length(unique(dup_elves))) {
    # message("weren't alone: ")
    # print(elves[dup_elves, ])
    propmat[dup_elves, ] <- elves[dup_elves, ]
  }
  elfdidntmove <- elfdidntmove - length(unique(dup_elves))

  # for (i in seq_len(nrow(elves))) {
  #   message("(", elves[i,1], ",", elves[i,2], ") -> (", propmat[i,1], ",", propmat[i,2], ")")
  # }
  grid[elves] <- "."
  grid[propmat] <- "#"
  elfmoved <- nrow(elves) - elfdidntmove
  moveorder <- moveorder[c(2:4, 1)]
  list(grid = grid, moveorder = moveorder, nmoved = elfmoved)

}

testdir <- function(grid, pos, dir) {
  offsetmat <- if (dir == "N") {
    matrix(c(rep(-1, 3), -1:1), ncol = 2, byrow = FALSE)
  }  else if (dir == "S") {
    matrix(c(rep(1, 3), -1:1), ncol = 2, byrow = FALSE)
  } else if (dir == "E") {
    matrix(c(-1:1, rep(1, 3)), ncol = 2, byrow = FALSE)
  } else if (dir == "W") {
    matrix(c(-1:1, rep(-1, 3)), ncol = 2, byrow = FALSE)
  }
    for (i in 1:2) {
      offsetmat[, i] <- offsetmat[, i] + pos[i]
    }
  !any(grid[offsetmat] == "#")
}

#' @rdname day23
#' @export
f23b <- function(x) {
  elves <- x
  excess <- 1000
  grid <- matrix(".", nrow = length(x) + 2*excess, ncol = length(x) + 2*excess)
  for (r in seq_along(elves)) {
    elfpos <- strsplit(elves[r], "")[[1]]
    for (i in seq_along(elfpos)) {
      grid[r + excess - 1, excess + i] <- elfpos[i]
    }
  }

  dirs <- list("N" = c(-1, 0), "S" = c(1, 0), "W" = c(0, -1), "E" = c(0, 1))

  for (i in 1:1000) {
    tmp <- iterate(grid, dirs)
    if (tmp$nmoved == 0) break
    grid <- tmp$grid
    dirs <- tmp$moveorder
  }
  i
}


f23_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day23
#' @export
example_data_23 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example23.txt")
  )
  l[[example]]
}
