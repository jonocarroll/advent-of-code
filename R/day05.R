#' Day 05: Supply Stacks
#'
#' [Supply Stacks](https://adventofcode.com/2022/day/5)
#'
#' @name day05
#' @rdname day05
#' @details
#'
#' **Part One**
#'
#' The expedition can depart as soon as the final supplies have been
#' unloaded from the ships. Supplies are stored in stacks of marked
#' *crates*, but because the needed supplies are buried under many other
#' crates, the crates need to be rearranged.
#'
#' The ship has a *giant cargo crane* capable of moving crates between
#' stacks. To ensure none of the crates get crushed or fall over, the crane
#' operator will rearrange them in a series of carefully-planned steps.
#' After the crates are rearranged, the desired crates will be at the top
#' of each stack.
#'
#' The Elves don\'t want to interrupt the crane operator during this
#' delicate procedure, but they forgot to ask her *which* crate will end up
#' where, and they want to be ready to unload them as soon as possible so
#' they can embark.
#'
#' They do, however, have a drawing of the starting stacks of crates *and*
#' the rearrangement procedure (your puzzle input). For example:
#'
#'         [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#'     move 1 from 2 to 1
#'     move 3 from 1 to 3
#'     move 2 from 2 to 1
#'     move 1 from 1 to 2
#'
#' In this example, there are three stacks of crates. Stack 1 contains two
#' crates: crate `Z` is on the bottom, and crate `N` is on top. Stack 2
#' contains three crates; from bottom to top, they are crates `M`, `C`, and
#' `D`. Finally, stack 3 contains a single crate, `P`.
#'
#' Then, the rearrangement procedure is given. In each step of the
#' procedure, a quantity of crates is moved from one stack to a different
#' stack. In the first step of the above rearrangement procedure, one crate
#' is moved from stack 2 to stack 1, resulting in this configuration:
#'
#'     [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#' In the second step, three crates are moved from stack 1 to stack 3.
#' Crates are moved *one at a time*, so the first crate to be moved (`D`)
#' ends up below the second and third crates:
#'
#'             [Z]
#'             [N]
#'         [C] [D]
#'         [M] [P]
#'      1   2   3
#'
#' Then, both crates are moved from stack 2 to stack 1. Again, because
#' crates are moved *one at a time*, crate `C` ends up below crate `M`:
#'
#'             [Z]
#'             [N]
#'     [M]     [D]
#'     [C]     [P]
#'      1   2   3
#'
#' Finally, one crate is moved from stack 1 to stack 2:
#'
#'             [Z]
#'             [N]
#'             [D]
#'     [C] [M] [P]
#'      1   2   3
#'
#' The Elves just need to know *which crate will end up on top of each
#' stack*; in this example, the top crates are `C` in stack 1, `M` in stack
#' 2, and `Z` in stack 3, so you should combine these together and give the
#' Elves the message *`CMZ`*.
#'
#' *After the rearrangement procedure completes, what crate ends up on top
#' of each stack?*
#'
#' **Part Two**
#'
#' As you watch the crane operator expertly rearrange the crates, you
#' notice the process isn\'t following your prediction.
#'
#' Some mud was covering the writing on the side of the crane, and you
#' quickly wipe it away. The crane isn\'t a CrateMover 9000 - it\'s a
#' *[CrateMover
#' 9001]{title="It's way better than the old CrateMover 1006."}*.
#'
#' The CrateMover 9001 is notable for many new and exciting features: air
#' conditioning, leather seats, an extra cup holder, and *the ability to
#' pick up and move multiple crates at once*.
#'
#' Again considering the example above, the crates begin in the same
#' configuration:
#'
#'         [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#' Moving a single crate from stack 2 to stack 1 behaves the same as
#' before:
#'
#'     [D]
#'     [N] [C]
#'     [Z] [M] [P]
#'      1   2   3
#'
#' However, the action of moving three crates from stack 1 to stack 3 means
#' that those three moved crates *stay in the same order*, resulting in
#' this new configuration:
#'
#'             [D]
#'             [N]
#'         [C] [Z]
#'         [M] [P]
#'      1   2   3
#'
#' Next, as both crates are moved from stack 2 to stack 1, they *retain
#' their order* as well:
#'
#'             [D]
#'             [N]
#'     [C]     [Z]
#'     [M]     [P]
#'      1   2   3
#'
#' Finally, a single crate is still moved from stack 1 to stack 2, but now
#' it\'s crate `C` that gets moved:
#'
#'             [D]
#'             [N]
#'             [Z]
#'     [M] [C] [P]
#'      1   2   3
#'
#' In this example, the CrateMover 9001 has put the crates in a totally
#' different order: *`MCD`*.
#'
#' Before the rearrangement process finishes, update your simulation so
#' that the Elves know where they should stand to be ready to unload the
#' final supplies. *After the rearrangement procedure completes, what crate
#' ends up on top of each stack?*
#'
#' @param x some data
#' @return For Part One, `f05a(x)` returns .... For Part Two,
#'   `f05b(x)` returns ....
#' @export
#' @examples
#' f05a(example_data_05())
#' f05b()
f05a <- function(x, model = 9000) {
  stack <- get_stacks(x)
  instr <- get_instructions(x)
  res <- crane(stack, instr, model)
  paste(sapply(res, tail, 1), collapse = "")
}


#' @rdname day05
#' @export
f05b <- function(x) {

}


extract_stack <- function(x) {
  # split into stacks
  n <- seq(1, nc <- nchar(x), by = 4)
  stack <- substring(x, n, c(n[-1]-1, nc))
  stack <- trimws(sub("]", "", sub("[", "", stack, fixed = TRUE), fixed = TRUE))
  stack
}

get_stacks <- function(x) {
  x <- x[1:(grep("^$", x)-1)]
  y <- t(sapply(x, extract_stack, USE.NAMES = FALSE))
  stackno <- y[nrow(y), ]
  y <- y[-nrow(y), ]
  z <- as.list(as.data.frame(y))
  z <- lapply(z, rev)
  z <- lapply(z, \(w) w[w != ""])
  z
}

get_instruction <- function(x) {
  x <- sub("move ", "", x)
  n <- as.integer(sub("([0-9]+).*", "\\1", x))
  x <- sub("^.*?from ", "", x)
  from = as.integer(sub("([0-9]+).*", "\\1", x))
  to <- as.integer(sub("^.*?to ", "", x))
  data.frame(n, from, to)
}

get_instructions <- function(x) {
  x <- x[(grep("^$", x)+1):length(x)]
  y <- lapply(x, get_instruction)
  do.call(rbind, y)
}

crane <- function(stack, inst, model) {
  for (r in seq_len(nrow(inst))) {
    stack <- .crane(stack, inst[r, ], model)
  }
  stack
}

.crane <- function(stack, inst, model) {
  sfrom <- paste0("V", inst$from)
  sto <- paste0("V", inst$to)
  pick <- tail(stack[[sfrom]], inst$n)
  if (model == 9000) {
    pick <- rev(pick)
  }
  stack[[sfrom]] <- head(stack[[sfrom]], -inst$n)
  stack[[sto]] <- c(stack[[sto]], pick)
  stack
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day05
#' @export
example_data_05 <- function(example = 1) {
  l <- list(
    crates = c(
      "    [D]    ",
      "[N] [C]    ",
      "[Z] [M] [P]",
      " 1   2   3 ",
      "",
      "move 1 from 2 to 1",
      "move 3 from 1 to 3",
      "move 2 from 2 to 1",
      "move 1 from 1 to 2"
    )
  )
  l[[example]]
}
