#' Day 18: Lavaduct Lagoon
#'
#' [Lavaduct Lagoon](https://adventofcode.com/2023/day/18)
#'
#' @name day18
#' @rdname day18
#' @details
#'
#' **Part One**
#'
#' Thanks to your efforts, the machine parts factory is one of the first
#' factories up and running since the lavafall came back. However, to catch
#' up with the large backlog of parts requests, the factory will also need
#' a *large supply of lava* for a while; the Elves have already started
#' creating a large lagoon nearby for this purpose.
#' 
#' However, they aren\'t sure the lagoon will be big enough; they\'ve asked
#' you to take a look at the *dig plan* (your puzzle input). For example:
#' 
#'     R 6 (#70c710)
#'     D 5 (#0dc571)
#'     L 2 (#5713f0)
#'     D 2 (#d2c081)
#'     R 2 (#59c680)
#'     D 2 (#411b91)
#'     L 5 (#8ceee2)
#'     U 2 (#caa173)
#'     L 1 (#1b58a2)
#'     U 2 (#caa171)
#'     R 2 (#7807d2)
#'     U 3 (#a77fa3)
#'     L 2 (#015232)
#'     U 2 (#7a21e3)
#' 
#' The digger starts in a 1 meter cube hole in the ground. They then dig
#' the specified number of meters *up* (`U`), *down* (`D`), *left* (`L`),
#' or *right* (`R`), clearing full 1 meter cubes as they go. The directions
#' are given as seen from above, so if \"up\" were north, then \"right\"
#' would be east, and so on. Each trench is also listed with *the color
#' that the edge of the trench should be painted* as an [RGB hexadecimal
#' color
#' code](https://en.wikipedia.org/wiki/RGB_color_model#Numeric_representations){target="_blank"}.
#' 
#' When viewed from above, the above example dig plan would result in the
#' following loop of *trench* (`#`) having been dug out from otherwise
#' *ground-level terrain* (`.`):
#' 
#'     #######
#'     #.....#
#'     ###...#
#'     ..#...#
#'     ..#...#
#'     ###.###
#'     #...#..
#'     ##..###
#'     .#....#
#'     .######
#' 
#' At this point, the trench could contain 38 cubic meters of lava.
#' However, this is just the edge of the lagoon; the next step is to *dig
#' out the interior* so that it is one meter deep as well:
#' 
#'     #######
#'     #######
#'     #######
#'     ..#####
#'     ..#####
#'     #######
#'     #####..
#'     #######
#'     .######
#'     .######
#' 
#' Now, the lagoon can contain a much more respectable *`62`* cubic meters
#' of lava. While the interior is dug out, the edges are also painted
#' according to the color codes in the dig plan.
#' 
#' The Elves are concerned the lagoon won\'t be large enough; if they
#' follow their dig plan, *how many cubic meters of lava could it hold?*
#'
#' **Part Two**
#' 
#' The Elves were right to be concerned; the planned lagoon would be *much
#' too small*.
#' 
#' After a few minutes, someone realizes what happened; someone
#' *[swapped]{title="Futuristic sprintf()?"} the color and instruction
#' parameters* when producing the dig plan. They don\'t have time to fix
#' the bug; one of them asks if you can *extract the correct instructions*
#' from the hexadecimal codes.
#' 
#' Each hexadecimal code is *six hexadecimal digits* long. The first five
#' hexadecimal digits encode the *distance in meters* as a five-digit
#' hexadecimal number. The last hexadecimal digit encodes the *direction to
#' dig*: `0` means `R`, `1` means `D`, `2` means `L`, and `3` means `U`.
#' 
#' So, in the above example, the hexadecimal codes can be converted into
#' the true instructions:
#' 
#' -   `#70c710` = `R 461937`
#' -   `#0dc571` = `D 56407`
#' -   `#5713f0` = `R 356671`
#' -   `#d2c081` = `D 863240`
#' -   `#59c680` = `R 367720`
#' -   `#411b91` = `D 266681`
#' -   `#8ceee2` = `L 577262`
#' -   `#caa173` = `U 829975`
#' -   `#1b58a2` = `L 112010`
#' -   `#caa171` = `D 829975`
#' -   `#7807d2` = `L 491645`
#' -   `#a77fa3` = `U 686074`
#' -   `#015232` = `L 5411`
#' -   `#7a21e3` = `U 500254`
#' 
#' Digging out this loop and its interior produces a lagoon that can hold
#' an impressive *`952408144115`* cubic meters of lava.
#' 
#' Convert the hexadecimal color codes into the correct instructions; if
#' the Elves follow this new dig plan, *how many cubic meters of lava could
#' the lagoon hold?*
#'
#' @param x some data
#' @return For Part One, `f18a(x)` returns .... For Part Two,
#'   `f18b(x)` returns ....
#' @export
#' @examples
#' f18a(example_data_18())
#' f18b()
f18a <- function(x, size = 1000) {
  x <- strsplit(x, " ")
  pos <- c(0, 0)
  xs <- ys <- vector(mode = "integer", length = length(x) + 2)
  i <- 1
  xs[i] <- pos[1]
  ys[i] <- pos[2]
  perim <- 0
  for (xx in x) {
    i <- i + 1
    dir <- switch(xx[1],
                  "R" = c(0, 1),
                  "L" = c(0, -1),
                  "U" = c(-1, 0),
                  "D" = c(1, 0))
    dist <- as.integer(xx[2])
    pos <- pos + dist*dir
    perim <- perim + dist
    xs[i] <- pos[1]
    ys[i] <- pos[2]
  }
  a <- 0
  for (j in 1:(length(xs)-1)) {
    a <- a + det(matrix(c(xs[j], xs[j+1], ys[j], ys[j+1]), nrow = 2, byrow = TRUE))
  }
  sprintf("%.15g", abs(a/2) + perim/2 + 1)
  # x <- strsplit(x, " ")
  # cols <- m <- matrix(".", nrow = size, ncol = size)
  # pos <- c(floor(size/2) + 1, floor(size/2) + 1)
  # firstdir <- prevdir <- x[[1]][1]
  # for (xx in x) {
  #   dir <- switch(xx[1], 
  #                  "R" = c(0, 1),
  #                  "L" = c(0, -1),
  #                  "U" = c(-1, 0),
  #                  "D" = c(1, 0))
  #   dist <- as.integer(xx[2])
  #   col <- gsub("[)(]", "", xx[3])
  #   m[matrix(pos, ncol = 2)] <- adjust(xx, prevdir)
  #   for (j in 1:dist) {
  #     pos <- pos + dir
  #     if (pos[1] > nrow(m) | pos[1] < 1 | pos[2] > ncol(m) | pos[2] < 1) stop("make bigger!")
  #     m[matrix(pos, ncol = 2)] <- ifelse(xx[1] %in% c("U", "D"), xx[1], "*")
  #     cols[matrix(pos, ncol = 2)] <- col
  #   }
  #   prevdir <- xx[1]
  # }
  # m[matrix(pos, ncol = 2)] <- adjust(firstdir[1], xx[1])
  # nedge <- sum(m != ".")
  # m2 <- m
  # m2 <- m2[apply(m2, 1, \(w) any(grepl("[LRUD*]", w))), ]
  # m2 <- m2[, apply(m2, 2, \(w) any(grepl("[LRUD*]", w)))]
  # 
  # z <- 0
  # for (i in 1:nrow(m2)) {
  #   inside <- FALSE
  #   for (j in 1:ncol(m2)) {
  #     if (m2[i, j] %in% c("U", "D", "J", "L")) {
  #       inside <- !inside 
  #     } else if (m2[i, j] == "." && inside) {
  #       z <- z + 1
  #       m2[i, j] <- "#"
  #     }
  #   }
  # }
  # m2
  # z  
  # ninternal <- z  
  # message("ninternal: ", z)
  # message("sum: ", sum(m2 != "."))
  # nedge + ninternal
  
  
}

adjust <- function(xx, prevdir) {
  if (prevdir == "U" && xx[1] == "L") {
      "7"
    } else if (prevdir == "U" && xx[1] == "R") {
      "F"
    } else if (prevdir == "D" && xx[1] == "L") {
      "J"
    } else if (prevdir == "D" && xx[1] == "R") {
      "L"
    } else if (prevdir == "L" && xx[1] == "U") {
      "L"
    } else if (prevdir == "R" && xx[1] == "U") {
      "L"
    } else if (prevdir == "L" && xx[1] == "D") {
      "7"
    } else if (prevdir == "R" && xx[1] == "D") {
      "F"
    } else {
      xx[1]
    }
}

#' @rdname day18
#' @export
f18b <- function(x) {
  x <- strsplit(x, " ")
  pos <- c(0, 0)
  xs <- ys <- vector(mode = "integer", length = length(x) + 2)
  i <- 1
  xs[i] <- pos[1]
  ys[i] <- pos[2]
  perim <- 0
  for (xx in x) {
    i <- i + 1
    col <- gsub("[)(#]", "", xx[3])
    dist <- as.integer(as.hexmode(substr(col, 1, 5)))
    perim <- perim + dist
    dir <- substr(col, 6, 6)
    # `0` means `R`, `1` means `D`, `2` means `L`, and `3` means `U`.
    dir <- switch(dir,
                  "0" = "R",
                  "1" = "D",
                  "2" = "L",
                  "3" = "U")
    dir <- switch(dir,
                  "R" = c(0, 1),
                  "L" = c(0, -1),
                  "U" = c(-1, 0),
                  "D" = c(1, 0))
    pos <- pos + dist*dir
    xs[i] <- pos[1]
    ys[i] <- pos[2]
  }
  a <- 0
  for (j in 1:(length(xs)-1)) {
    a <- a + det(matrix(c(xs[j], xs[j+1], ys[j], ys[j+1]), nrow = 2, byrow = TRUE))
  }
  sprintf("%.15g", abs(a/2) + perim/2 + 1)
}

f18_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day18
#' @export
example_data_18 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
