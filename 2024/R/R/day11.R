#' Day 11: Plutonian Pebbles
#'
#' [Plutonian Pebbles](https://adventofcode.com/2024/day/11)
#'
#' @name day11
#' @rdname day11
#' @details
#'
#' **Part One**
#'
#' The ancient civilization on [Pluto](/2019/day/20) was known for its
#' ability to manipulate spacetime, and while the Historians explore their
#' infinite corridors, you\'ve noticed a strange set of physics-defying
#' stones.
#' 
#' At first glance, they seem like normal stones: they\'re arranged in a
#' perfectly *straight line*, and each stone has a *number* engraved on it.
#' 
#' The strange part is that every time you
#' [blink]{title="No, they're not statues. Why do you ask?"}, the stones
#' *change*.
#' 
#' Sometimes, the number engraved on a stone changes. Other times, a stone
#' might *split in two*, causing all the other stones to shift over a bit
#' to make room in their perfectly straight line.
#' 
#' As you observe them for a while, you find that the stones have a
#' consistent behavior. Every time you blink, the stones each
#' *simultaneously* change according to the *first applicable rule* in this
#' list:
#' 
#' -   If the stone is engraved with the number `0`, it is replaced by a
#'     stone engraved with the number `1`.
#' -   If the stone is engraved with a number that has an *even* number of
#'     digits, it is replaced by *two stones*. The left half of the digits
#'     are engraved on the new left stone, and the right half of the digits
#'     are engraved on the new right stone. (The new numbers don\'t keep
#'     extra leading zeroes: `1000` would become stones `10` and `0`.)
#' -   If none of the other rules apply, the stone is replaced by a new
#'     stone; the old stone\'s number *multiplied by 2024* is engraved on
#'     the new stone.
#' 
#' No matter how the stones change, their *order is preserved*, and they
#' stay on their perfectly straight line.
#' 
#' How will the stones evolve if you keep blinking at them? You take a note
#' of the number engraved on each stone in the line (your puzzle input).
#' 
#' If you have an arrangement of five stones engraved with the numbers
#' `0 1 10 99 999` and you blink once, the stones transform as follows:
#' 
#' -   The first stone, `0`, becomes a stone marked `1`.
#' -   The second stone, `1`, is multiplied by 2024 to become `2024`.
#' -   The third stone, `10`, is split into a stone marked `1` followed by
#'     a stone marked `0`.
#' -   The fourth stone, `99`, is split into two stones marked `9`.
#' -   The fifth stone, `999`, is replaced by a stone marked `2021976`.
#' 
#' So, after blinking once, your five stones would become an arrangement of
#' seven stones engraved with the numbers `1 2024 1 0 9 9 2021976`.
#' 
#' Here is a longer example:
#' 
#'     Initial arrangement:
#'     125 17
#' 
#'     After 1 blink:
#'     253000 1 7
#' 
#'     After 2 blinks:
#'     253 0 2024 14168
#' 
#'     After 3 blinks:
#'     512072 1 20 24 28676032
#' 
#'     After 4 blinks:
#'     512 72 2024 2 0 2 4 2867 6032
#' 
#'     After 5 blinks:
#'     1036288 7 2 20 24 4048 1 4048 8096 28 67 60 32
#' 
#'     After 6 blinks:
#'     2097446912 14168 4048 2 0 2 4 40 48 2024 40 48 80 96 2 8 6 7 6 0 3 2
#' 
#' In this example, after blinking six times, you would have `22` stones.
#' After blinking 25 times, you would have *`55312`* stones!
#' 
#' Consider the arrangement of stones in front of you. *How many stones
#' will you have after blinking 25 times?*
#'
#' **Part Two**
#' 
#' The Historians sure are taking a long time. To be fair, the infinite
#' corridors *are* very large.
#' 
#' *How many stones would you have after blinking a total of 75 times?*
#'
#' @param x some data
#' @return For Part One, `f11a(x)` returns .... For Part Two,
#'   `f11b(x)` returns ....
#' @export
#' @examples
#' f11a(example_data_11())
#' f11b()
f11a <- function(x) {
  d <- readLines("inst/input11.txt")
  stones <- as.numeric(strsplit(d, " ")[[1]])
  count_em(stones, 25)
}

blink_rec <- memoise::memoise(function(x, n) {
  if (n == 0) return(1)
  if (x == 0) {
    blink_rec(1, n-1)
  } else if ((digits <- nchar(x)) %% 2 == 0) {
    vals <- as.numeric(c(substring(x, 1, digits/2), substring(x, (digits/2)+1)))
    blink_rec(vals[1], n-1) + blink_rec(vals[2], n-1)
  } else {
    blink_rec(x*2024, n-1)
  }
})

count_em <- function(stones, n) {
  tot <- 0
  for (i in seq_along(stones)) {
    tot <- tot + blink_rec(stones[i], n)
  }
  print(tot, digits = 22)
  invisible(tot)
}

#' @rdname day11
#' @export
f11b <- function(x) {
  d <- readLines("inst/input11.txt")
  stones <- as.numeric(strsplit(d, " ")[[1]])
  count_em(stones, 75)
}


f11_helper <- function(x) {

}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day11
#' @export
example_data_11 <- function(example = 1) {
  l <- list(
    a = c(


    )
  )
  l[[example]]
}
