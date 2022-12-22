#' Day 22: Monkey Map
#'
#' [Monkey Map](https://adventofcode.com/2022/day/22)
#'
#' @name day22
#' @rdname day22
#' @details
#'
#' **Part One**
#'
#' The monkeys take you on a surprisingly easy trail through the jungle.
#' They\'re even going in roughly the right direction according to your
#' handheld device\'s Grove Positioning System.
#'
#' As you walk, the monkeys explain that the grove is protected by a *force
#' field*. To pass through the force field, you have to enter a password;
#' doing so involves tracing a specific *path* on a strangely-shaped board.
#'
#' At least, you\'re pretty sure that\'s what you have to do; the elephants
#' aren\'t exactly fluent in monkey.
#'
#' The monkeys give you notes that they took when they last saw the
#' password entered (your puzzle input).
#'
#' For example:
#'
#'             ...#
#'             .#..
#'             #...
#'             ....
#'     ...#.......#
#'     ........#...
#'     ..#....#....
#'     ..........#.
#'             ...#....
#'             .....#..
#'             .#......
#'             ......#.
#'
#'     10R5L5R10L4R5L5
#'
#' The first half of the monkeys\' notes is a *map of the board*. It is
#' comprised of a set of *open tiles* (on which you can move, drawn `.`)
#' and *solid walls* (tiles which you cannot enter, drawn `#`).
#'
#' The second half is a description of *the path you must follow*. It
#' consists of alternating numbers and letters:
#'
#' -   A *number* indicates the *number of tiles to move* in the direction
#'     you are facing. If you run into a wall, you stop moving forward and
#'     continue with the next instruction.
#' -   A *letter* indicates whether to turn 90 degrees *clockwise* (`R`) or
#'     *[counterclockwise]{title="Or \"anticlockwise\", if you're anti-counterclockwise."}*
#'     (`L`). Turning happens in-place; it does not change your current
#'     tile.
#'
#' So, a path like `10R5` means \"go forward 10 tiles, then turn clockwise
#' 90 degrees, then go forward 5 tiles\".
#'
#' You begin the path in the leftmost open tile of the top row of tiles.
#' Initially, you are facing *to the right* (from the perspective of how
#' the map is drawn).
#'
#' If a movement instruction would take you off of the map, you *wrap
#' around* to the other side of the board. In other words, if your next
#' tile is off of the board, you should instead look in the direction
#' opposite of your current facing as far as you can until you find the
#' opposite edge of the board, then reappear there.
#'
#' For example, if you are at `A` and facing to the right, the tile in
#' front of you is marked `B`; if you are at `C` and facing down, the tile
#' in front of you is marked `D`:
#'
#'             ...#
#'             .#..
#'             #...
#'             ....
#'     ...#.D.....#
#'     ........#...
#'     B.#....#...A
#'     .....C....#.
#'             ...#....
#'             .....#..
#'             .#......
#'             ......#.
#'
#' It is possible for the next tile (after wrapping around) to be a *wall*;
#' this still counts as there being a wall in front of you, and so movement
#' stops before you actually wrap to the other side of the board.
#'
#' By drawing the *last facing you had* with an arrow on each tile you
#' visit, the full path taken by the above example looks like this:
#'
#'             >>v#
#'             .#v.
#'             #.v.
#'             ..v.
#'     ...#...v..v#
#'     >>>v...>#.>>
#'     ..#v...#....
#'     ...>>>>v..#.
#'             ...#....
#'             .....#..
#'             .#......
#'             ......#.
#'
#' To finish providing the password to this strange input device, you need
#' to determine numbers for your final *row*, *column*, and *facing* as
#' your final position appears from the perspective of the original map.
#' Rows start from `1` at the top and count downward; columns start from
#' `1` at the left and count rightward. (In the above example, row 1,
#' column 1 refers to the empty space with no tile on it in the top-left
#' corner.) Facing is `0` for right (`>`), `1` for down (`v`), `2` for left
#' (`<`), and `3` for up (`^`). The *final password* is the sum of 1000
#' times the row, 4 times the column, and the facing.
#'
#' In the above example, the final row is `6`, the final column is `8`, and
#' the final facing is `0`. So, the final password is 1000 \* 6 + 4 \* 8 +
#' 0: *`6032`*.
#'
#' Follow the path given in the monkeys\' notes. *What is the final
#' password?*
#'
#' **Part Two**
#'
#' As you reach the force field, you think you hear some Elves in the
#' distance. Perhaps they\'ve already arrived?
#'
#' You approach the strange *input device*, but it isn\'t quite what the
#' monkeys drew in their notes. Instead, you are met with a large *cube*;
#' each of its six faces is a square of 50x50 tiles.
#'
#' To be fair, the monkeys\' map *does* have six 50x50 regions on it. If
#' you were to *carefully fold the map*, you should be able to shape it
#' into a cube!
#'
#' In the example above, the six (smaller, 4x4) faces of the cube are:
#'
#'             1111
#'             1111
#'             1111
#'             1111
#'     222233334444
#'     222233334444
#'     222233334444
#'     222233334444
#'             55556666
#'             55556666
#'             55556666
#'             55556666
#'
#' You still start in the same position and with the same facing as before,
#' but the *wrapping* rules are different. Now, if you would walk off the
#' board, you instead *proceed around the cube*. From the perspective of
#' the map, this can look a little strange. In the above example, if you
#' are at A and move to the right, you would arrive at B facing down; if
#' you are at C and move down, you would arrive at D facing up:
#'
#'             ...#
#'             .#..
#'             #...
#'             ....
#'     ...#.......#
#'     ........#..A
#'     ..#....#....
#'     .D........#.
#'             ...#..B.
#'             .....#..
#'             .#......
#'             ..C...#.
#'
#' Walls still block your path, even if they are on a different face of the
#' cube. If you are at E facing up, your movement is blocked by the wall
#' marked by the arrow:
#'
#'             ...#
#'             .#..
#'          -->#...
#'             ....
#'     ...#..E....#
#'     ........#...
#'     ..#....#....
#'     ..........#.
#'             ...#....
#'             .....#..
#'             .#......
#'             ......#.
#'
#' Using the same method of drawing the *last facing you had* with an arrow
#' on each tile you visit, the full path taken by the above example now
#' looks like this:
#'
#'             >>v#
#'             .#v.
#'             #.v.
#'             ..v.
#'     ...#..^...v#
#'     .>>>>>^.#.>>
#'     .^#....#....
#'     .^........#.
#'             ...#..v.
#'             .....#v.
#'             .#v<<<<.
#'             ..v...#.
#'
#' The final password is still calculated from your final position and
#' facing from the perspective of the map. In this example, the final row
#' is `5`, the final column is `7`, and the final facing is `3`, so the
#' final password is 1000 \* 5 + 4 \* 7 + 3 = *`5031`*.
#'
#' Fold the map into a cube, *then* follow the path given in the monkeys\'
#' notes. *What is the final password?*
#'
#' @param x some data
#' @return For Part One, `f22a(x)` returns .... For Part Two,
#'   `f22b(x)` returns ....
#' @export
#' @examples
#' f22a(example_data_22())
#' f22b()
f22a <- function(x) {
  dat <- readmap(x)
  map <- dat$map
  instr <- dat$inst

  rowextent <<- t(apply(map, 1, \(y) range(grep("[.#]", y))))
  colextent <<- t(apply(map, 2, \(y) range(grep("[.#]", y))))

  pos <- c(1, rowextent[1, 1]) # finally count downward!
  facing <- "E"
  for (i in seq_along(instr)) {
  posdata <- move(map, instr[i], pos, facing)
  pos <- posdata[[1]]
  facing <- posdata[[2]]
}

  facingval <- switch(facing,
                      E = 0,
                      S = 1,
                      W = 2,
                      N = 3)
  message("final position: ")
  print(pos)
  message("final facing value: ", facingval)
  1e3*pos[1] + 4*pos[2] + facingval

}


#' @rdname day22
#' @export
f22b <- function(x) {

}


move <- function(map, instr, pos, facing) {

  testfacing <- switch(facing,
                   N = c(-1, 0),
                   E = c(0, 1),
                   S = c(1, 0),
                   W = c(0, -1))

  if (!is.na(suppressWarnings(num <- as.integer(instr)))) {
    # message("attempting to move ", num, " steps ", facing)
    newfacing <- facing
    for (i in 1:num) {
      testpos <- pos + testfacing

      if (facing %in% c("E", "W")) {
        if (testpos[2] < rowextent[testpos[1], 1]) {
          # message("wrap l")
          testpos[2] <- rowextent[testpos[1], 2]
        }
        if (testpos[2] > rowextent[testpos[1], 2]) {
          # message("wrap r")
          testpos[2] <- rowextent[testpos[1], 1]
        }
      } else if (facing %in% c("N", "S")) {
        if (testpos[1] < colextent[testpos[2], 1]) {
          # message("wrap up")
          testpos[1] <- colextent[testpos[2], 2]
        }
        if (testpos[1] > colextent[testpos[2], 2]) {
          # message("wrap down")
          testpos[1] <- colextent[testpos[2], 1]
        }
      }
      # print(testpos)
      # message("map here: ", map[matrix(testpos, ncol = 2)])
      if (any(grepl("[ #]", map[matrix(testpos, ncol = 2)]))) {
        break
      }

      pos <- testpos

    }
    newpos <- pos
  } else {
    if (instr == "L") {
      newfacing <- switch(facing,
                          N = "W",
                          E = "N",
                          S = "E",
                          W = "S")
    } else if (instr == "R") {
      newfacing <- switch(facing,
                          N = "E",
                          E = "S",
                          S = "W",
                          W = "N")
    }
    newpos <- pos
  }
  list(newpos, newfacing)
}

readmap <- function(x) {
  blank <- grep("^$", x)
  maplines <- strsplit(x[1:(blank-1)], "")
  maxlen <- max(sapply(maplines, length))
  maplines <- lapply(maplines, \(y) c(y, rep(" ", maxlen)))
  maplines <- lapply(maplines, \(y) y[1:maxlen])
  map <- do.call(rbind, maplines)
  instr <- strsplit(trimws(x[(blank+1):length(x)]), "")[[1]]
  instrvec <- vector(mode = "character", length = length(instr))
  instrvec[1] <- instr[1]
  i <- 1
  for (j in 2:length(instr)) {
    last_is_num <- instr[j-1] %in% as.character(0:9)
    this_is_num <- instr[j] %in% as.character(0:9)
    if (last_is_num && this_is_num) {
      instrvec[i] <- paste0(instr[j-1], instr[j])
    } else {
      i <- i + 1
      instrvec[i] <- instr[j]
    }
  }
  instrvec <- instrvec[1:i]
  list(map = map, instr = instrvec)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day22
#' @export
example_data_22 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example22.txt")
  )
  l[[example]]
}
