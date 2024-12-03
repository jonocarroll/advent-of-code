#' Day 03: Mull It Over
#'
#' [Mull It Over](https://adventofcode.com/2024/day/3)
#'
#' @name day03
#' @rdname day03
#' @details
#'
#' **Part One**
#'
#' \"Our computers are having issues, so I have no idea if we have any
#' Chief Historians [in
#' stock]{title="There's a spot reserved for Chief Historians between the green toboggans and the red toboggans. They've never actually had any Chief Historians in stock, but it's best to be prepared."}!
#' You\'re welcome to check the warehouse, though,\" says the mildly
#' flustered shopkeeper at the [North Pole Toboggan Rental
#' Shop](/2020/day/2). The Historians head out to take a look.
#' 
#' The shopkeeper turns to you. \"Any chance you can see why our computers
#' are having issues again?\"
#' 
#' The computer appears to be trying to run a program, but its memory (your
#' puzzle input) is *corrupted*. All of the instructions have been jumbled
#' up!
#' 
#' It seems like the goal of the program is just to *multiply some
#' numbers*. It does that with instructions like `mul(X,Y)`, where `X` and
#' `Y` are each 1-3 digit numbers. For instance, `mul(44,46)` multiplies
#' `44` by `46` to get a result of `2024`. Similarly, `mul(123,4)` would
#' multiply `123` by `4`.
#' 
#' However, because the program\'s memory has been corrupted, there are
#' also many invalid characters that should be *ignored*, even if they look
#' like part of a `mul` instruction. Sequences like `mul(4*`, `mul(6,9!`,
#' `?(12,34)`, or `mul ( 2 , 4 )` do *nothing*.
#' 
#' For example, consider the following section of corrupted memory:
#' 
#'     xmul(2,4)%&mul[3,7]!@^do_not_mul(5,5)+mul(32,64]then(mul(11,8)mul(8,5))
#' 
#' Only the four highlighted sections are real `mul` instructions. Adding
#' up the result of each instruction produces *`161`*
#' (`2*4 + 5*5 + 11*8 + 8*5`).
#' 
#' Scan the corrupted memory for uncorrupted `mul` instructions. *What do
#' you get if you add up all of the results of the multiplications?*
#'
#' **Part Two**
#'
#' As you scan through the corrupted memory, you notice that some of the
#' conditional statements are also still intact. If you handle some of the
#' uncorrupted conditional statements in the program, you might be able to
#' get an even more accurate result.
#' 
#' There are two new instructions you\'ll need to handle:
#' 
#' -   The `do()` instruction *enables* future `mul` instructions.
#' -   The `don't()` instruction *disables* future `mul` instructions.
#' 
#' Only the *most recent* `do()` or `don't()` instruction applies. At the
#' beginning of the program, `mul` instructions are *enabled*.
#' 
#' For example:
#' 
#'     xmul(2,4)&mul[3,7]!^don't()_mul(5,5)+mul(32,64](mul(11,8)undo()?mul(8,5))
#' 
#' This corrupted memory is similar to the example from before, but this
#' time the `mul(5,5)` and `mul(11,8)` instructions are *disabled* because
#' there is a `don't()` instruction before them. The other `mul`
#' instructions function normally, including the one at the end that gets
#' re-*enabled* by a `do()` instruction.
#' 
#' This time, the sum of the results is *`48`* (`2*4 + 8*5`).
#' 
#' Handle the new instructions; *what do you get if you add up all of the
#' results of just the enabled multiplications?*
#'
#' @param x some data
#' @return For Part One, `f03a(x)` returns .... For Part Two,
#'   `f03b(x)` returns ....
#' @export
#' @examples
#' f03a(example_data_03())
#' f03b()
f03a <- function(x) {
  x <- readLines("inst/input03.txt")
  eq <- sapply(regmatches(x, gregexec("mul\\([0-9]{1,3},[0-9]{1,3}\\)", x)), \(z) paste(z, collapse = "+"))
  sum(sapply(eq, \(z) eval(parse(text = z))))
}

mul <- function(x, y) x*y 

#' @rdname day03
#' @export
f03b <- function(x) {
  x <- readLines("inst/input03.txt")
  ops <- regmatches(x, gregexec("mul\\([0-9]{1,3},[0-9]{1,3}\\)|don't\\(\\)|do\\(\\)", x))
  get_res(unlist(ops))
}

get_res <- function(z) {
  tot <- 0
  onoff <- 1
  for (op in z) {
    if (op == "do()") {
      onoff <- 1
    } else if (op == "don't()") {
      onoff <- 0
    } else {
      tot <- tot + onoff*eval(parse(text = op))
    }
  }
  tot
}

f03_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day03
#' @export
example_data_03 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
