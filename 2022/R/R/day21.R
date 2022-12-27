#' Day 21: Monkey Math
#'
#' [Monkey Math](https://adventofcode.com/2022/day/21)
#'
#' @name day21
#' @rdname day21
#' @details
#'
#' **Part One**
#'
#' The [monkeys](11) are back! You\'re worried they\'re going to try to
#' steal your stuff again, but it seems like they\'re just holding their
#' ground and making various monkey noises at you.
#'
#' Eventually, one of the elephants realizes you don\'t speak monkey and
#' comes over to interpret. As it turns out, they overheard you talking
#' about trying to find the grove; they can show you a shortcut if you
#' answer their *riddle*.
#'
#' Each monkey is given a *job*: either to *yell a specific number* or to
#' *yell the result of a math operation*. All of the number-yelling monkeys
#' know their number from the start; however, the math operation monkeys
#' need to wait for two other monkeys to yell a number, and those two other
#' monkeys might *also* be waiting on other monkeys.
#'
#' Your job is to *work out the number the monkey named `root` will yell*
#' before the monkeys figure it out themselves.
#'
#' For example:
#'
#'     root: pppw + sjmn
#'     dbpl: 5
#'     cczh: sllz + lgvd
#'     zczc: 2
#'     ptdq: humn - dvpt
#'     dvpt: 3
#'     lfqf: 4
#'     humn: 5
#'     ljgn: 2
#'     sjmn: drzm * dbpl
#'     sllz: 4
#'     pppw: cczh / lfqf
#'     lgvd: ljgn * ptdq
#'     drzm: hmdt - zczc
#'     hmdt: 32
#'
#' Each line contains the name of a monkey, a colon, and then the job of
#' that monkey:
#'
#' -   A lone number means the monkey\'s job is simply to yell that number.
#' -   A job like `aaaa + bbbb` means the monkey waits for monkeys `aaaa`
#'     and `bbbb` to yell each of their numbers; the monkey then yells the
#'     sum of those two numbers.
#' -   `aaaa - bbbb` means the monkey yells `aaaa`\'s number minus
#'     `bbbb`\'s number.
#' -   Job `aaaa * bbbb` will yell `aaaa`\'s number multiplied by `bbbb`\'s
#'     number.
#' -   Job `aaaa / bbbb` will yell `aaaa`\'s number divided by `bbbb`\'s
#'     number.
#'
#' So, in the above example, monkey `drzm` has to wait for monkeys `hmdt`
#' and `zczc` to yell their numbers. Fortunately, both `hmdt` and `zczc`
#' have jobs that involve simply yelling a single number, so they do this
#' immediately: `32` and `2`. Monkey `drzm` can then yell its number by
#' finding `32` minus `2`: *`30`*.
#'
#' Then, monkey `sjmn` has one of its numbers (`30`, from monkey `drzm`),
#' and already has its other number, `5`, from `dbpl`. This allows it to
#' yell its own number by finding `30` multiplied by `5`: *`150`*.
#'
#' This process continues until `root` yells a number: *`152`*.
#'
#' However, your actual situation involves [considerably more
#' monkeys]{title="Advent of Code 2022: Now With Considerably More Monkeys"}.
#' *What number will the monkey named `root` yell?*
#'
#' **Part Two**
#'
#' Due to some kind of monkey-elephant-human mistranslation, you seem to
#' have misunderstood a few key details about the riddle.
#'
#' First, you got the wrong job for the monkey named `root`; specifically,
#' you got the wrong math operation. The correct operation for monkey
#' `root` should be `=`, which means that it still listens for two numbers
#' (from the same two monkeys as before), but now checks that the two
#' numbers *match*.
#'
#' Second, you got the wrong monkey for the job starting with `humn:`. It
#' isn\'t a monkey - it\'s *you*. Actually, you got the job wrong, too: you
#' need to figure out *what number you need to yell* so that `root`\'s
#' equality check passes. (The number that appears after `humn:` in your
#' input is now irrelevant.)
#'
#' In the above example, the number you need to yell to pass `root`\'s
#' equality test is *`301`*. (This causes `root` to get the same number,
#' `150`, from both of its monkeys.)
#'
#' *What number do you yell to pass `root`\'s equality test?*
#'
#' @param x some data
#' @return For Part One, `f21a(x)` returns .... For Part Two,
#'   `f21b(x)` returns ....
#' @export
#' @examples
#' f21a(example_data_21())
#' f21b()
f21a <- function(x) {
  defs <- sapply(x, parseInput)
  for (d in defs) {
    eval(parse(text = d))
  }
  format(root(), scientific = FALSE)
}


#' @rdname day21
#' @export
f21b <- function(x) {
  x[grep("root:", x)] <- sub("+", "==", x[grep("root:", x)], fixed = TRUE)
  rootdef <- x[grep("root:", x)]
  parts <- strsplit(rootdef, " ")[[1]]
  f1 <- paste0(parts[2], "()")
  f2 <- paste0(parts[4], "()")
  defs <- sapply(x, parseInput)
  for (d in defs) {
    eval(parse(text = d))
  }
  humn <- function() 5
  f1_1 <- eval(parse(text = f1))
  f2_1 <- eval(parse(text = f2))
  humn <- function() 10
  f1_2 <- eval(parse(text = f1))
  f2_2 <- eval(parse(text = f2))

  ## if ONE of the sides doesn't depend on humn:
  if (f1_1 == f1_2) {
    message(f1, " is constant wrt humn")
    dynf <- f2
    target <- f1_1
  } else if (f2_1 == f2_2) {
    message(f2, " is constant wrt humn")
    dynf <- f1
    target <- f2_2
  }

  x <- y <- vector(mode = "integer", length = 21)
  for (i in 0:50) {
    x[i+1] <- 2^i
    humn <- function() {2^i}
    y[i+1] <- eval(parse(text = dynf))
  }

  lineq <- lm(y ~ x)
  coefs <- coef(lineq)
  # treat this as Ax + B = y
  # so (B - y) + Ax = 0
  format(Re(polyroot(c(coefs[1] - target, coefs[2]))), scientific = FALSE)

}


f21_helper <- function(x) {

}

parseInput <- function(x) {
  monkey <- sub("^(.*):.*", "\\1", x)
  ret <- sub(".*: (.*)$", "\\1", x)
  if (is.na(suppressWarnings(as.integer(ret)))) {
    ret <- strsplit(ret, " ")[[1]]
    v1 <- ret[1]
    op <- ret[2]
    v2 <- ret[3]
    def <- paste0(monkey, " <- function() { ", v1, "() ", op, " ", v2, "() }")
  } else {
    def <- paste0(monkey, " <- function() { ", ret, " }")
  }
}

#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day21
#' @export
example_data_21 <- function(example = 1) {
  l <- list(
    a = readLines("inst/example21.txt")
  )
  l[[example]]
}
