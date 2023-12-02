#' Day 02: Cube Conundrum
#'
#' [Cube Conundrum](https://adventofcode.com/2023/day/2)
#'
#' @name day02
#' @rdname day02
#' @details
#'
#' **Part One**
#'
#' You\'re launched high into the atmosphere! The apex of your trajectory
#' just barely reaches the surface of a large island floating in the sky.
#' You gently land in a fluffy pile of leaves. It\'s quite cold, but you
#' don\'t see much snow. An Elf runs over to greet you.
#' 
#' The Elf explains that you\'ve arrived at *Snow Island* and apologizes
#' for the lack of snow. He\'ll be happy to explain the situation, but
#' it\'s a bit of a walk, so you have some time. They don\'t get many
#' visitors up here; [would you like to play a
#' game]{title="No, the Elf's name is not 'WOPR'. It's Joshua."} in the
#' meantime?
#' 
#' As you walk, the Elf shows you a small bag and some cubes which are
#' either red, green, or blue. Each time you play this game, he will hide a
#' secret number of cubes of each color in the bag, and your goal is to
#' figure out information about the number of cubes.
#' 
#' To get information, once a bag has been loaded with cubes, the Elf will
#' reach into the bag, grab a handful of random cubes, show them to you,
#' and then put them back in the bag. He\'ll do this a few times per game.
#' 
#' You play several games and record the information from each game (your
#' puzzle input). Each game is listed with its ID number (like the `11` in
#' `Game 11: ...`) followed by a semicolon-separated list of subsets of
#' cubes that were revealed from the bag (like `3 red, 5 green, 4 blue`).
#' 
#' For example, the record of a few games might look like this:
#' 
#'     Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
#'     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
#'     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
#'     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
#'     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
#' 
#' In game 1, three sets of cubes are revealed from the bag (and then put
#' back again). The first set is 3 blue cubes and 4 red cubes; the second
#' set is 1 red cube, 2 green cubes, and 6 blue cubes; the third set is
#' only 2 green cubes.
#' 
#' The Elf would first like to know which games would have been possible if
#' the bag contained *only 12 red cubes, 13 green cubes, and 14 blue
#' cubes*?
#' 
#' In the example above, games 1, 2, and 5 would have been *possible* if
#' the bag had been loaded with that configuration. However, game 3 would
#' have been *impossible* because at one point the Elf showed you 20 red
#' cubes at once; similarly, game 4 would also have been *impossible*
#' because the Elf showed you 15 blue cubes at once. If you add up the IDs
#' of the games that would have been possible, you get *`8`*.
#' 
#' Determine which games would have been possible if the bag had been
#' loaded with only 12 red cubes, 13 green cubes, and 14 blue cubes. *What
#' is the sum of the IDs of those games?*
#'
#' **Part Two**
#' 
#' The Elf says they\'ve stopped producing snow because they aren\'t
#' getting any *water*! He isn\'t sure why the water stopped; however, he
#' can show you how to get to the water source to check it out for
#' yourself. It\'s just up ahead!
#' 
#' As you continue your walk, the Elf poses a second question: in each game
#' you played, what is the *fewest number of cubes of each color* that
#' could have been in the bag to make the game possible?
#' 
#' Again consider the example games from earlier:
#' 
#'     Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green
#'     Game 2: 1 blue, 2 green; 3 green, 4 blue, 1 red; 1 green, 1 blue
#'     Game 3: 8 green, 6 blue, 20 red; 5 blue, 4 red, 13 green; 5 green, 1 red
#'     Game 4: 1 green, 3 red, 6 blue; 3 green, 6 red; 3 green, 15 blue, 14 red
#'     Game 5: 6 red, 1 blue, 3 green; 2 blue, 1 red, 2 green
#' 
#' -   In game 1, the game could have been played with as few as 4 red, 2
#'     green, and 6 blue cubes. If any color had even one fewer cube, the
#'     game would have been impossible.
#' -   Game 2 could have been played with a minimum of 1 red, 3 green, and
#'     4 blue cubes.
#' -   
#' -   Game 3 must have been played with at least 20 red, 13 green, and 6
#'     blue cubes.
#' -   Game 4 required at least 14 red, 3 green, and 15 blue cubes.
#' -   Game 5 needed no fewer than 6 red, 3 green, and 2 blue cubes in the
#'     bag.
#' 
#' The *power* of a set of cubes is equal to the numbers of red, green, and
#' blue cubes multiplied together. The power of the minimum set of cubes in
#' game 1 is `48`. In games 2-5 it was `12`, `1560`, `630`, and `36`,
#' respectively. Adding up these five powers produces the sum *`2286`*.
#' 
#' For each game, find the minimum set of cubes that must have been
#' present. *What is the sum of the power of these sets?*
#'
#' @param x some data
#' @return For Part One, `f02a(x)` returns .... For Part Two,
#'   `f02b(x)` returns ....
#' @export
#' @examples
#' f02a(example_data_02())
#' f02b()
f02a <- function(x) {
  checked <- sapply(x, f02_helper, USE.NAMES = FALSE)
  sum(which(checked[2, ] == 1))
}


#' @rdname day02
#' @export
f02b <- function(x) {
  sum(sapply(x, f02b_helper))
}


f02_helper <- function(x) {
  game <- as.integer(sub("Game ([0-9]*).*", "\\1", x))
  x <- sub(".*: ", "", x)
  sets <- unlist(sapply(strsplit(x, "; "), \(y) sapply(strsplit(y, ","), trimws), simplify = F))
  possible <- function(s) {
    vals <- strsplit(s, " ")[[1]]
    (vals[2] == "red" && as.integer(vals[1]) <= 12) ||
      (vals[2] == "green" && as.integer(vals[1]) <= 13) || 
        (vals[2] == "blue" && as.integer(vals[1]) <= 14)
  }
  c(game, all(sapply(sets, possible)))
}


f02b_helper <- function(x) {
  game <- as.integer(sub("Game ([0-9]*).*", "\\1", x))
  x <- sub(".*: ", "", x)
  sets <- sapply(strsplit(x, "; "), \(y) lapply(strsplit(y, ","), trimws))
  totals <- lapply(sets, \(z) unglue::unglue_data(z, "{n=\\d+} {c}"))
  check_max <- function(d1, d2) {
    r1 <- as.integer(d1[d1$c == "red", "n"])
    r2 <- as.integer(d2[d2$c == "red", "n"])
    b1 <- as.integer(d1[d1$c == "blue", "n"])
    b2 <- as.integer(d2[d2$c == "blue", "n"])
    g1 <- as.integer(d1[d1$c == "green", "n"])
    g2 <- as.integer(d2[d2$c == "green", "n"])
    
    maxr <- max(0, max(r1, r2))
    maxb <- max(0, max(b1, b2))
    maxg <- max(0, max(g1, g2))
    
    d3 <- data.frame(n = integer(), c = character())
    d3 <- rbind(d3, data.frame(n = maxr, c = "red"))
    d3 <- rbind(d3, data.frame(n = maxb, c = "blue"))
    d3 <- rbind(d3, data.frame(n = maxg, c = "green"))    
  }
  prod(Reduce(check_max, totals)$n)
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day02
#' @export
example_data_02 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
