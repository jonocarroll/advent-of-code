#' Day 15: Warehouse Woes
#'
#' [Warehouse Woes](https://adventofcode.com/2024/day/15)
#'
#' @name day15
#' @rdname day15
#' @details
#'
#' **Part One**
#'
#' You appear back inside your own mini submarine! Each Historian drives
#' their mini submarine in a different direction; maybe the Chief has his
#' own submarine down here somewhere as well?
#' 
#' You look up to see a vast school of [lanternfish](/2021/day/6) swimming
#' past you. On closer inspection, they seem quite anxious, so you drive
#' your mini submarine over to see if you can help.
#' 
#' Because lanternfish populations grow rapidly, they need a lot of food,
#' and that food needs to be stored somewhere. That\'s why these
#' lanternfish have built elaborate warehouse complexes operated by robots!
#' 
#' These lanternfish seem so anxious because they have lost control of the
#' robot that operates one of their most important warehouses! It is
#' currently running
#' [amok]{title="Wesnoth players might solve their Warehouse Woes with a Warehouse Wose!"},
#' pushing around boxes in the warehouse with no regard for lanternfish
#' logistics *or* lanternfish inventory management strategies.
#' 
#' Right now, none of the lanternfish are brave enough to swim up to an
#' unpredictable robot so they could shut it off. However, if you could
#' anticipate the robot\'s movements, maybe they could find a safe option.
#' 
#' The lanternfish already have a map of the warehouse and a list of
#' movements the robot will *attempt* to make (your puzzle input). The
#' problem is that the movements will sometimes fail as boxes are shifted
#' around, making the actual movements of the robot difficult to predict.
#' 
#' For example:
#' 
#'     ##########
#'     #..O..O.O#
#'     #......O.#
#'     #.OO..O.O#
#'     #..O@..O.#
#'     #O#..O...#
#'     #O..O..O.#
#'     #.OO.O.OO#
#'     #....O...#
#'     ##########
#' 
#'     <vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
#'     vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
#'     ><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
#'     <<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
#'     ^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
#'     ^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
#'     >^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
#'     <><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
#'     ^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
#'     v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^
#' 
#' As the robot (`@`) attempts to move, if there are any boxes (`O`) in the
#' way, the robot will also attempt to push those boxes. However, if this
#' action would cause the robot or a box to move into a wall (`#`), nothing
#' moves instead, including the robot. The initial positions of these are
#' shown on the map at the top of the document the lanternfish gave you.
#' 
#' The rest of the document describes the *moves* (`^` for up, `v` for
#' down, `<` for left, `>` for right) that the robot will attempt to make,
#' in order. (The moves form a single giant sequence; they are broken into
#' multiple lines just to make copy-pasting easier. Newlines within the
#' move sequence should be ignored.)
#' 
#' Here is a smaller example to get started:
#' 
#'     ########
#'     #..O.O.#
#'     ##@.O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     <^^>>>vv<v>>v<<
#' 
#' Were the robot to attempt the given sequence of moves, it would push
#' around the boxes as follows:
#' 
#'     Initial state:
#'     ########
#'     #..O.O.#
#'     ##@.O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move <:
#'     ########
#'     #..O.O.#
#'     ##@.O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move ^:
#'     ########
#'     #.@O.O.#
#'     ##..O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move ^:
#'     ########
#'     #.@O.O.#
#'     ##..O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move >:
#'     ########
#'     #..@OO.#
#'     ##..O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move >:
#'     ########
#'     #...@OO#
#'     ##..O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move >:
#'     ########
#'     #...@OO#
#'     ##..O..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #......#
#'     ########
#' 
#'     Move v:
#'     ########
#'     #....OO#
#'     ##..@..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move v:
#'     ########
#'     #....OO#
#'     ##..@..#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move <:
#'     ########
#'     #....OO#
#'     ##.@...#
#'     #...O..#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move v:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #..@O..#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move >:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #...@O.#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move >:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #....@O#
#'     #.#.O..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move v:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #.....O#
#'     #.#.O@.#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move <:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #.....O#
#'     #.#O@..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#'     Move <:
#'     ########
#'     #....OO#
#'     ##.....#
#'     #.....O#
#'     #.#O@..#
#'     #...O..#
#'     #...O..#
#'     ########
#' 
#' The larger example has many more moves; after the robot has finished
#' those moves, the warehouse would look like this:
#' 
#'     ##########
#'     #.O.O.OOO#
#'     #........#
#'     #OO......#
#'     #OO@.....#
#'     #O#.....O#
#'     #O.....OO#
#'     #O.....OO#
#'     #OO....OO#
#'     ##########
#' 
#' The lanternfish use their own custom Goods Positioning System (GPS for
#' short) to track the locations of the boxes. The *GPS coordinate* of a
#' box is equal to 100 times its distance from the top edge of the map plus
#' its distance from the left edge of the map. (This process does not stop
#' at wall tiles; measure all the way to the edges of the map.)
#' 
#' So, the box shown below has a distance of `1` from the top edge of the
#' map and `4` from the left edge of the map, resulting in a GPS coordinate
#' of `100 * 1 + 4 = 104`.
#' 
#'     #######
#'     #...O..
#'     #......
#' 
#' The lanternfish would like to know the *sum of all boxes\' GPS
#' coordinates* after the robot finishes moving. In the larger example, the
#' sum of all boxes\' GPS coordinates is *`10092`*. In the smaller example,
#' the sum is *`2028`*.
#' 
#' Predict the motion of the robot and boxes in the warehouse. After the
#' robot is finished moving, *what is the sum of all boxes\' GPS
#' coordinates?*
#'
#' **Part Two**
#' 
#' The lanternfish use your information to find a safe moment to swim in
#' and turn off the malfunctioning robot! Just as they start preparing a
#' festival in your honor, reports start coming in that a *second*
#' warehouse\'s robot is *also* malfunctioning.
#' 
#' This warehouse\'s layout is surprisingly similar to the one you just
#' helped. There is one key difference: everything except the robot is
#' *twice as wide*! The robot\'s list of movements doesn\'t change.
#' 
#' To get the wider warehouse\'s map, start with your original map and, for
#' each tile, make the following changes:
#' 
#' -   If the tile is `#`, the new map contains `##` instead.
#' -   If the tile is `O`, the new map contains `[]` instead.
#' -   If the tile is `.`, the new map contains `..` instead.
#' -   If the tile is `@`, the new map contains `@.` instead.
#' 
#' This will produce a new warehouse map which is twice as wide and with
#' wide boxes that are represented by `[]`. (The robot does not change
#' size.)
#' 
#' The larger example from before would now look like this:
#' 
#'     ####################
#'     ##....[]....[]..[]##
#'     ##............[]..##
#'     ##..[][]....[]..[]##
#'     ##....[]@.....[]..##
#'     ##[]##....[]......##
#'     ##[]....[]....[]..##
#'     ##..[][]..[]..[][]##
#'     ##........[]......##
#'     ####################
#' 
#' Because boxes are now twice as wide but the robot is still the same size
#' and speed, boxes can be aligned such that they directly push two other
#' boxes at once. For example, consider this situation:
#' 
#'     #######
#'     #...#.#
#'     #.....#
#'     #..OO@#
#'     #..O..#
#'     #.....#
#'     #######
#' 
#'     <vv<<^^<<^^
#' 
#' After appropriately resizing this map, the robot would push around these
#' boxes as follows:
#' 
#'     Initial state:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##....[][]@.##
#'     ##....[]....##
#'     ##..........##
#'     ##############
#' 
#'     Move <:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##...[][]@..##
#'     ##....[]....##
#'     ##..........##
#'     ##############
#' 
#'     Move v:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##...[][]...##
#'     ##....[].@..##
#'     ##..........##
#'     ##############
#' 
#'     Move v:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##.......@..##
#'     ##############
#' 
#'     Move <:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##......@...##
#'     ##############
#' 
#'     Move <:
#'     ##############
#'     ##......##..##
#'     ##..........##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##.....@....##
#'     ##############
#' 
#'     Move ^:
#'     ##############
#'     ##......##..##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##.....@....##
#'     ##..........##
#'     ##############
#' 
#'     Move ^:
#'     ##############
#'     ##......##..##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##.....@....##
#'     ##..........##
#'     ##############
#' 
#'     Move <:
#'     ##############
#'     ##......##..##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##....@.....##
#'     ##..........##
#'     ##############
#' 
#'     Move <:
#'     ##############
#'     ##......##..##
#'     ##...[][]...##
#'     ##....[]....##
#'     ##...@......##
#'     ##..........##
#'     ##############
#' 
#'     Move ^:
#'     ##############
#'     ##......##..##
#'     ##...[][]...##
#'     ##...@[]....##
#'     ##..........##
#'     ##..........##
#'     ##############
#' 
#'     Move ^:
#'     ##############
#'     ##...[].##..##
#'     ##...@.[]...##
#'     ##....[]....##
#'     ##..........##
#'     ##..........##
#'     ##############
#' 
#' This warehouse also uses GPS to locate the boxes. For these larger
#' boxes, distances are measured from the edge of the map to the closest
#' edge of the box in question. So, the box shown below has a distance of
#' `1` from the top edge of the map and `5` from the left edge of the map,
#' resulting in a GPS coordinate of `100 * 1 + 5 = 105`.
#' 
#'     ##########
#'     ##...[]...
#'     ##........
#' 
#' In the scaled-up version of the larger example from above, after the
#' robot has finished all of its moves, the warehouse would look like this:
#' 
#'     ####################
#'     ##[].......[].[][]##
#'     ##[]...........[].##
#'     ##[]........[][][]##
#'     ##[]......[]....[]##
#'     ##..##......[]....##
#'     ##..[]............##
#'     ##..@......[].[][]##
#'     ##......[][]..[]..##
#'     ####################
#' 
#' The sum of these boxes\' GPS coordinates is *`9021`*.
#' 
#' Predict the motion of the robot and boxes in this new, scaled-up
#' warehouse. *What is the sum of all boxes\' final GPS coordinates?*
#'
#' @param x some data
#' @return For Part One, `f15a(x)` returns .... For Part Two,
#'   `f15b(x)` returns ....
#' @export
#' @examples
#' f15a(example_data_15())
#' f15b()
f15a <- function(x) {
  d <- readLines("inst/input15.txt")
  dsep <- which(d == "")
  m <- d[1:(dsep-1)]
  ins <- strsplit(paste(d[(dsep+1):length(d)], collapse = ""), "")[[1]]
  m <- matrix(strsplit(paste(m, collapse = ""), "")[[1]], nrow = length(m), byrow = TRUE)
  
  walls <- which(m == "#", arr.ind = TRUE)
  walls <- complex(real = walls[, 1], imaginary = walls[, 2])
  boxes <- which(m == "O", arr.ind = TRUE)
  boxes <- complex(real = boxes[, 1], imaginary = boxes[, 2])
  robot <- which(m == "@", arr.ind = TRUE)[1, ]
  robot <- complex(real = robot[1], imaginary = robot[2])

  b <- boxes
  r <- robot
  for (i in seq_along(ins)) {
    x <- move(b, r, ins[i])  
    b <- x[[1]]
    r <- x[[2]]
    # plotroom(b, r, ins[i])  
  }
  
  b <- setdiff(b, r)
  sum(sapply(b, gps))
}

gps <- function(x) {
  offset <- -1-1i
  100*Re(x+offset) + Im(x+offset)
}


plotroom <- function(b, r, d) {
  room <- m
  room[which(room == "O")] <- "."
  room[which(room == "@")] <- "."
  boxes <- as.matrix(data.frame(row = Re(b), col = Im(b)))
  room[boxes] <- "O"
  room[Re(r), Im(r)] <- "@"
  cat(d, "\n")
  cat(paste(apply(room, 1, paste, collapse = ""), collapse = "\n"))
  cat("\n")
}

plotroom2 <- function(b, r, d) {
  room <- m
  room[which(room == "[")] <- "."
  room[which(room == "]")] <- "."
  room[which(room == "@")] <- "."
  lboxes <- as.matrix(data.frame(row = Re(b[, 1]), col = Im(b[, 1])))
  rboxes <- as.matrix(data.frame(row = Re(b[, 2]), col = Im(b[, 2])))
  room[lboxes] <- "["
  room[rboxes] <- "]"
  room[Re(r), Im(r)] <- "@"
  cat(d, "\n")
  cat(paste(apply(room, 1, paste, collapse = ""), collapse = "\n"))
  cat("\n")
}

move <- function(b, p, d) {
  nextd <- switch(d, 
                  ">" = 0 + 1i,
                  "^" = -1 + 0i,
                  "<" = 0 - 1i,
                  "v" = 1 + 0i
  )
  if ((p + nextd) %in% walls) { 
    return(list(b, p, FALSE)) 
  }
  if ((p + nextd) %in% b) {
    tryb <- move(c(setdiff(b, p), p + nextd), p + nextd, d)
    if (!tryb[[3]]) {
      return(list(b, p, FALSE))
    } else {
      return(list(c(setdiff(tryb[[1]], p), p + nextd), p + nextd, TRUE))
    }
  }
  list(c(setdiff(b, p), p + nextd), p + nextd, TRUE)
}

move2 <- function(b, p, d) {
  nextd <- switch(d, 
                  ">" = 0 + 1i,
                  "^" = -1 + 0i,
                  "<" = 0 - 1i,
                  "v" = 1 + 0i
  )
  if ((p + nextd) %in% walls) { 
    return(list(b, p, FALSE)) 
  }
  if (((p + nextd) %in% b[, 1]) || ((p + nextd) %in% b[, 2])) {
    boxpos <- unlist(b[b[, 1] == (p + nextd) | b[, 2] == (p + nextd), ])
    if (d %in% c("^", "v")) {
      tryb1 <- move2(b, boxpos[1], d)
      tryb2 <- move2(tryb1[[1]], boxpos[2], d)
      if (!(tryb1[[3]] & tryb2[[3]])) {
        return(list(b, p, FALSE))
      } else {
        b <- move_box(tryb2[[1]], boxpos, nextd)
        return(list(b, p + nextd, TRUE))
      }
    } else {
      tryb <- move2(b, p + nextd, d)
      if (!tryb[[3]]) {
        return(list(b, p, FALSE))
      } else {
        b <- move_box(tryb[[1]], p + nextd, nextd)
        return(list(b, p + nextd, TRUE))
      } 
    }
  }
  
  list(b, p + nextd, TRUE)
}

move_box <- function(b, posv, d) {
  v1 <- posv[1]
  if (v1 %in% b[, 1]) {
    b[b[, 1] == v1, 1] <- v1 + d
    b[b[, 1] == v1, 2] <- v1 + d + (0+1i)
  }
  if (v1 %in% b[, 2]) {
    b[b[, 2] == v1, 2] <- v1 + d
    b[b[, 1] == v1, 1] <- v1 + d + (0-1i)
  }
  if (length(posv) > 1) {
    v2 <- posv[2]
    if (v2 %in% b[, 1]) {
      b[b[, 1] == v2, 1] <- v2 + d
      b[b[, 1] == v2, 2] <- v1 + d + (0+1i)
    }
    if (v2 %in% b[, 2]) {
      b[b[, 2] == v2, 2] <- v2 + d
      b[b[, 1] == v2, 1] <- v2 + d + (0-1i)
    }
  }
  b
}


#' @rdname day15
#' @export
f15b <- function(x) {
  d <- readLines("inst/input15.txt")
  dsep <- which(d == "")
  m <- d[1:(dsep-1)]
  ins <- strsplit(paste(d[(dsep+1):length(d)], collapse = ""), "")[[1]]

  m2 <- paste(m, collapse = "")
  m2 <- gsub("#", "##", m2)
  m2 <- gsub("O", "[]", m2)
  m2 <- gsub(".", "..", m2, fixed = TRUE)
  m2 <- gsub("@", "@.", m2)
  
  m <- matrix(strsplit(m2, "")[[1]], nrow = length(m), byrow = TRUE)
  
  walls <- which(m == "#", arr.ind = TRUE)
  walls <- complex(real = walls[, 1], imaginary = walls[, 2])
  lboxes <- which(m == "[", arr.ind = TRUE)
  lboxes <- complex(real = lboxes[, 1], imaginary = lboxes[, 2])
  rboxes <- which(m == "]", arr.ind = TRUE)
  rboxes <- complex(real = rboxes[, 1], imaginary = rboxes[, 2])
  robot <- which(m == "@", arr.ind = TRUE)[1, ]
  robot <- complex(real = robot[1], imaginary = robot[2])
  
  boxes <- data.frame(lboxes, rboxes)
  b <- boxes
  r <- robot
  plotroom2(b, r, ins[i]) 
  for (i in seq_along(ins)) {
    x <- move2(b, r, ins[i])  
    b <- x[[1]]
    r <- x[[2]]
    # plotroom2(b, r, ins[i])  
  }
  
  b <- setdiff(b, r)
  sum(sapply(b$lboxes, gps))
}


f15_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day15
#' @export
example_data_15 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
