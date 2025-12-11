#' Day 10: Factory
#'
#' [Factory](https://adventofcode.com/2025/day/10)
#'
#' @name day10
#' @rdname day10
#' @details
#'
#' **Part One**
#'
#' Just across the hall, you find a large factory. Fortunately, the Elves
#' here have plenty of time to decorate. Unfortunately, it\'s because the
#' factory machines are all offline, and none of the Elves can figure out
#' the initialization procedure.
#'
#' The Elves do have the manual for the machines, but the section detailing
#' the initialization procedure was eaten by a [Shiba
#' Inu](https://en.wikipedia.org/wiki/Shiba_Inu). All that remains of the
#' manual are some indicator light diagrams, button wiring schematics, and
#' [joltage](3) requirements for each machine.
#'
#' For example:
#'
#'     [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
#'     [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
#'     [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
#'
#' The manual describes one machine per line. Each line contains a single
#' indicator light diagram in `[`square brackets`]`, one or more button
#' wiring schematics in `(`parentheses`)`, and joltage requirements in
#' `{`curly braces`}`.
#'
#' To start a machine, its *indicator lights* must match those shown in the
#' diagram, where `.` means *off* and `#` means *on*. The machine has the
#' number of indicator lights shown, but its indicator lights are all
#' *initially off*.
#'
#' So, an indicator light diagram like `[.##.]` means that the machine has
#' four indicator lights which are initially off and that the goal is to
#' simultaneously configure the first light to be off, the second light to
#' be on, the third to be on, and the fourth to be off.
#'
#' You can *toggle* the state of indicator lights by pushing any of the
#' listed *buttons*. Each button lists which indicator lights it toggles,
#' where `0` means the first light, `1` means the second light, and so on.
#' When you push a button, each listed indicator light either turns on (if
#' it was off) or turns off (if it was on). You have to push each button an
#' integer number of times; there\'s no such thing as \"[0.5
#' presses]{title="But only because these aren't A buttons."}\" (nor can
#' you push a button a negative number of times).
#'
#' So, a button wiring schematic like `(0,3,4)` means that each time you
#' push that button, the first, fourth, and fifth indicator lights would
#' all toggle between on and off. If the indicator lights were `[#.....]`,
#' pushing the button would change them to be `[...##.]` instead.
#'
#' Because none of the machines are running, the joltage requirements are
#' irrelevant and can be safely ignored.
#'
#' You can push each button as many times as you like. However, to save on
#' time, you will need to determine the *fewest total presses* required to
#' correctly configure all indicator lights for all machines in your list.
#'
#' There are a few ways to correctly configure the first machine:
#'
#'     [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
#'
#' - You could press the first three buttons once each, a total of `3`
#'   button presses.
#' - You could press `(1,3)` once, `(2,3)` once, and `(0,1)` twice, a total
#'   of `4` button presses.
#' - You could press all of the buttons except `(1,3)` once each, a total
#'   of `5` button presses.
#'
#' However, the fewest button presses required is *`2`*. One way to do this
#' is by pressing the last two buttons (`(0,2)` and `(0,1)`) once each.
#'
#' The second machine can be configured with as few as *`3`* button
#' presses:
#'
#'     [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
#'
#' One way to achieve this is by pressing the last three buttons (`(0,4)`,
#' `(0,1,2)`, and `(1,2,3,4)`) once each.
#'
#' The third machine has a total of six indicator lights that need to be
#' configured correctly:
#'
#'     [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
#'
#' The fewest presses required to correctly configure it is *`2`*; one way
#' to do this is by pressing buttons `(0,3,4)` and `(0,1,2,4,5)` once each.
#'
#' So, the fewest button presses required to correctly configure the
#' indicator lights on all of the machines is `2` + `3` + `2` = *`7`*.
#'
#' Analyze each machine\'s indicator light diagram and button wiring
#' schematics. *What is the fewest button presses required to correctly
#' configure the indicator lights on all of the machines?*
#'
#' **Part Two**
#'
#' All of the machines are starting to come online! Now, it\'s time to
#' worry about the joltage requirements.
#'
#' Each machine needs to be configured to *exactly the specified joltage
#' levels* to function properly. Below the buttons on each machine is a big
#' lever that you can use to switch the buttons from configuring the
#' indicator lights to increasing the joltage levels. (Ignore the indicator
#' light diagrams.)
#'
#' The machines each have a set of *numeric counters* tracking its joltage
#' levels, one counter per joltage requirement. The counters are all
#' *initially set to zero*.
#'
#' So, joltage requirements like `{3,5,4,7}` mean that the machine has four
#' counters which are initially `0` and that the goal is to simultaneously
#' configure the first counter to be `3`, the second counter to be `5`, the
#' third to be `4`, and the fourth to be `7`.
#'
#' The button wiring schematics are still relevant: in this new joltage
#' configuration mode, each button now indicates which counters it affects,
#' where `0` means the first counter, `1` means the second counter, and so
#' on. When you push a button, each listed counter is *increased by `1`*.
#'
#' So, a button wiring schematic like `(1,3)` means that each time you push
#' that button, the second and fourth counters would each increase by `1`.
#' If the current joltage levels were `{0,1,2,3}`, pushing the button would
#' change them to be `{0,2,2,4}`.
#'
#' You can push each button as many times as you like. However, your finger
#' is getting sore from all the button pushing, and so you will need to
#' determine the *fewest total presses* required to correctly configure
#' each machine\'s joltage level counters to match the specified joltage
#' requirements.
#'
#' Consider again the example from before:
#'
#'     [.##.] (3) (1,3) (2) (2,3) (0,2) (0,1) {3,5,4,7}
#'     [...#.] (0,2,3,4) (2,3) (0,4) (0,1,2) (1,2,3,4) {7,5,12,7,2}
#'     [.###.#] (0,1,2,3,4) (0,3,4) (0,1,2,4,5) (1,2) {10,11,11,5,10,5}
#'
#' Configuring the first machine\'s counters requires a minimum of *`10`*
#' button presses. One way to do this is by pressing `(3)` once, `(1,3)`
#' three times, `(2,3)` three times, `(0,2)` once, and `(0,1)` twice.
#'
#' Configuring the second machine\'s counters requires a minimum of *`12`*
#' button presses. One way to do this is by pressing `(0,2,3,4)` twice,
#' `(2,3)` five times, and `(0,1,2)` five times.
#'
#' Configuring the third machine\'s counters requires a minimum of *`11`*
#' button presses. One way to do this is by pressing `(0,1,2,3,4)` five
#' times, `(0,1,2,4,5)` five times, and `(1,2)` once.
#'
#' So, the fewest button presses required to correctly configure the
#' joltage level counters on all of the machines is `10` + `12` + `11` =
#' *`33`*.
#'
#' Analyze each machine\'s joltage requirements and button wiring
#' schematics. *What is the fewest button presses required to correctly
#' configure the joltage level counters on all of the machines?*
#'
#' @param x some data
#' @return For Part One, `f10a(x)` returns .... For Part Two,
#'   `f10b(x)` returns ....
#' @export
#' @examples
#' f10a(example_data_10())
#' f10b()
f10a <- function(x) {
  d <- readLines("input.txt")
  d <- readLines("inst/input10.txt")
  acc <- 0
  for (i in seq_along(d)) {
    p <- parse(d[i])
    n <- p[[1]]
    lights <- p[[2]]
    buttons <- sapply(p[[3]], make_buttons, n)
    vals <- do.call(expand.grid, rep(list(c(0, 1)), length(buttons)))
    vals <- t(apply(vals, 1, \(x) x*buttons))
    res <- apply(vals, 1, \(x) Reduce(bitwXor, x, init = 0))
    nz <- apply(vals, 1, \(x) sum(x != 0))
    valres <- as.data.frame(cbind(vals, res, nz))
    acc <- acc + min(valres[which(valres$res == make_buttons(lights, n)), ]$nz)
  }
  acc
}

make_buttons <- function(x, len) {
  y <- rep(0, len)
  y[as.integer(x)+1] <- 1
  res <- 0
  for (i in (seq_along(y) - 1)) {
    if (y[i+1] == 1) res <- res + 2^(i+1)
  }
  res
}

parse <- function(x) {
  btns <- strsplit(gsub("\\(", "", sub("^.*?]", "", sub("\\{.*", "", x))), ")")[[1]]
  res <- lapply(btns, \(x) strsplit(trimws(x), ",")[[1]])

  lts <- sub("\\[", "", sub("\\].*", "", x))
  n <- nchar(lts)

  joltage <- as.integer(strsplit(sub("\\}", "", sub("^.*\\{", "", x)), ",")[[1]])

  list(n, which(strsplit(lts, "")[[1]] == "#")-1, res[lengths(res) > 0], joltage)
}

#' @rdname day10
#' @export
f10b <- function(x) {

  library(ROI.plugin.glpk)

  d <- readLines("input.txt")
  d <- readLines("inst/input10.txt")

  acc <- 0
  for (i in seq_along(d)) {
    p <- parse(d[i])
    n <- p[[1]]
    lights <- p[[2]]
    buttons <- lapply(p[[3]], as.integer)
    nb <- length(buttons)
    joltages <- p[[4]]
    nj <- length(joltages)

    A <- matrix(0, nrow = nj, ncol = nb)
    for (j in 1:nb) {
      for (idx in buttons[[j]]) {
        A[idx + 1, j] <- 1
      }
    }

    model <- ompr::MIPModel() |>
      ompr::add_variable(x[j], j = 1:nb, type = "integer", lb = 0) |>
      ompr::set_objective(sum_over(x[j], j = 1:nb), "min") |>
      ompr::add_constraint(sum_over(A[i, j] * x[j], j = 1:nb) == joltages[i], i = 1:nj)

    result <- ompr::solve_model(model, ompr.roi::with_ROI(solver = "glpk", verbose = FALSE))
    acc <- acc + result$objective_value
  }
  acc
}



f10_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day10
#' @export
example_data_10 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
