#' Day 13: Distress Signal
#'
#' [Distress Signal](https://adventofcode.com/2022/day/13)
#'
#' @name day13
#' @rdname day13
#' @details
#'
#' **Part One**
#'
#' You climb the hill and again try contacting the Elves. However, you
#' instead receive a signal you weren\'t expecting: a *distress signal*.
#'
#' Your handheld device must still not be working properly; the packets
#' from the distress signal got decoded *out of order*. You\'ll need to
#' re-order the list of received packets (your puzzle input) to decode the
#' message.
#'
#' Your list consists of pairs of packets; pairs are separated by a blank
#' line. You need to identify *how many pairs of packets are in the right
#' order*.
#'
#' For example:
#'
#'     [1,1,3,1,1]
#'     [1,1,5,1,1]
#'
#'     [[1],[2,3,4]]
#'     [[1],4]
#'
#'     [9]
#'     [[8,7,6]]
#'
#'     [[4,4],4,4]
#'     [[4,4],4,4,4]
#'
#'     [7,7,7,7]
#'     [7,7,7]
#'
#'     []
#'     [3]
#'
#'     [[[]]]
#'     [[]]
#'
#'     [1,[2,[3,[4,[5,6,7]]]],8,9]
#'     [1,[2,[3,[4,[5,6,0]]]],8,9]
#'
#' [Packet data consists of lists and
#' integers.]{title="The snailfish called. They want their distress signal back."}
#' Each list starts with `[`, ends with `]`, and contains zero or more
#' comma-separated values (either integers or other lists). Each packet is
#' always a list and appears on its own line.
#'
#' When comparing two values, the first value is called *left* and the
#' second value is called *right*. Then:
#'
#' -   If *both values are integers*, the *lower integer* should come
#'     first. If the left integer is lower than the right integer, the
#'     inputs are in the right order. If the left integer is higher than
#'     the right integer, the inputs are not in the right order. Otherwise,
#'     the inputs are the same integer; continue checking the next part of
#'     the input.
#' -   If *both values are lists*, compare the first value of each list,
#'     then the second value, and so on. If the left list runs out of items
#'     first, the inputs are in the right order. If the right list runs out
#'     of items first, the inputs are not in the right order. If the lists
#'     are the same length and no comparison makes a decision about the
#'     order, continue checking the next part of the input.
#' -   If *exactly one value is an integer*, convert the integer to a list
#'     which contains that integer as its only value, then retry the
#'     comparison. For example, if comparing `[0,0,0]` and `2`, convert the
#'     right value to `[2]` (a list containing `2`); the result is then
#'     found by instead comparing `[0,0,0]` and `[2]`.
#'
#' Using these rules, you can determine which of the pairs in the example
#' are in the right order:
#'
#'     == Pair 1 ==
#'     - Compare [1,1,3,1,1] vs [1,1,5,1,1]
#'       - Compare 1 vs 1
#'       - Compare 1 vs 1
#'       - Compare 3 vs 5
#'         - Left side is smaller, so inputs are in the right order
#'
#'     == Pair 2 ==
#'     - Compare [[1],[2,3,4]] vs [[1],4]
#'       - Compare [1] vs [1]
#'         - Compare 1 vs 1
#'       - Compare [2,3,4] vs 4
#'         - Mixed types; convert right to [4] and retry comparison
#'         - Compare [2,3,4] vs [4]
#'           - Compare 2 vs 4
#'             - Left side is smaller, so inputs are in the right order
#'
#'     == Pair 3 ==
#'     - Compare [9] vs [[8,7,6]]
#'       - Compare 9 vs [8,7,6]
#'         - Mixed types; convert left to [9] and retry comparison
#'         - Compare [9] vs [8,7,6]
#'           - Compare 9 vs 8
#'             - Right side is smaller, so inputs are not in the right order
#'
#'     == Pair 4 ==
#'     - Compare [[4,4],4,4] vs [[4,4],4,4,4]
#'       - Compare [4,4] vs [4,4]
#'         - Compare 4 vs 4
#'         - Compare 4 vs 4
#'       - Compare 4 vs 4
#'       - Compare 4 vs 4
#'       - Left side ran out of items, so inputs are in the right order
#'
#'     == Pair 5 ==
#'     - Compare [7,7,7,7] vs [7,7,7]
#'       - Compare 7 vs 7
#'       - Compare 7 vs 7
#'       - Compare 7 vs 7
#'       - Right side ran out of items, so inputs are not in the right order
#'
#'     == Pair 6 ==
#'     - Compare [] vs [3]
#'       - Left side ran out of items, so inputs are in the right order
#'
#'     == Pair 7 ==
#'     - Compare [[[]]] vs [[]]
#'       - Compare [[]] vs []
#'         - Right side ran out of items, so inputs are not in the right order
#'
#'     == Pair 8 ==
#'     - Compare [1,[2,[3,[4,[5,6,7]]]],8,9] vs [1,[2,[3,[4,[5,6,0]]]],8,9]
#'       - Compare 1 vs 1
#'       - Compare [2,[3,[4,[5,6,7]]]] vs [2,[3,[4,[5,6,0]]]]
#'         - Compare 2 vs 2
#'         - Compare [3,[4,[5,6,7]]] vs [3,[4,[5,6,0]]]
#'           - Compare 3 vs 3
#'           - Compare [4,[5,6,7]] vs [4,[5,6,0]]
#'             - Compare 4 vs 4
#'             - Compare [5,6,7] vs [5,6,0]
#'               - Compare 5 vs 5
#'               - Compare 6 vs 6
#'               - Compare 7 vs 0
#'                 - Right side is smaller, so inputs are not in the right order
#'
#' What are the indices of the pairs that are already *in the right order*?
#' (The first pair has index 1, the second pair has index 2, and so on.) In
#' the above example, the pairs in the right order are 1, 2, 4, and 6; the
#' sum of these indices is *`13`*.
#'
#' Determine which pairs of packets are already in the right order. *What
#' is the sum of the indices of those pairs?*
#'
#' **Part Two**
#'
#' Now, you just need to put *all* of the packets in the right order.
#' Disregard the blank lines in your list of received packets.
#'
#' The distress signal protocol also requires that you include two
#' additional *divider packets*:
#'
#'     [[2]]
#'     [[6]]
#'
#' Using the same rules as before, organize all packets - the ones in your
#' list of received packets as well as the two divider packets - into the
#' correct order.
#'
#' For the example above, the result of putting the packets in the correct
#' order is:
#'
#'     []
#'     [[]]
#'     [[[]]]
#'     [1,1,3,1,1]
#'     [1,1,5,1,1]
#'     [[1],[2,3,4]]
#'     [1,[2,[3,[4,[5,6,0]]]],8,9]
#'     [1,[2,[3,[4,[5,6,7]]]],8,9]
#'     [[1],4]
#'     [[2]]
#'     [3]
#'     [[4,4],4,4]
#'     [[4,4],4,4,4]
#'     [[6]]
#'     [7,7,7]
#'     [7,7,7,7]
#'     [[8,7,6]]
#'     [9]
#'
#' Afterward, locate the divider packets. To find the *decoder key* for
#' this distress signal, you need to determine the indices of the two
#' divider packets and multiply them together. (The first packet is at
#' index 1, the second packet is at index 2, and so on.) In this example,
#' the divider packets are *10th* and *14th*, and so the decoder key is
#' *`140`*.
#'
#' Organize all of the packets into the correct order. *What is the decoder
#' key for the distress signal?*
#'
#' @param x some data
#' @return For Part One, `f13a(x)` returns .... For Part Two,
#'   `f13b(x)` returns ....
#' @export
#' @examples
#' f13a(example_data_13())
#' f13b()
f13a <- function(x) {
  pairs <- strsplit(paste(x, collapse = "\n"), "\\n\\n")[[1]]
  packets <- strsplit(pairs, "\n")
  sum(which(sapply(packets, \(y) compare_packets(y[1], y[2]))))
}

convert_to_list <- function(x) {
  if (!is.character(x)) return(x)
  sapply(x, \(y) eval(parse(text = gsub("]", ")", gsub("[", "list(", y, fixed = TRUE), fixed = TRUE))), simplify = FALSE)
}

#' @rdname day13
#' @export
f13b <- function(x) {
  pairs <- strsplit(paste(x, collapse = "\n"), "\\n\\n")[[1]]
  packets <- strsplit(pairs, "\n")
  dividers <- c("[[2]]", "[[6]]")
  packets <- c(do.call(c, packets), dividers)
  sorted <- rev(bubble_sort(packets))
  prod(which(sorted %in% dividers))
}


compare_packets <- function(l, r) {
  left <- convert_to_list(l)
  right <- convert_to_list(r)
  res <- 0
  for (i in seq_along(left)) {
    if (i > length(right)) return(FALSE)
    if (length(left[[i]]) == 0 && length(right[[i]]) != 0) return(TRUE)
    if (length(left[[i]]) != 0 && length(right[[i]]) == 0) return(FALSE)
    if (length(left[[i]]) == 0 && length(right[[i]]) == 0) next
    if (!is.list(left[[i]]) && !is.list(right[[i]])) {
      if (length(left[[i]]) == 1 && length(right[[i]]) == 1) {
        if (left[[i]][[1]] == right[[i]][[1]]) next
        return(left[[i]][[1]] < right[[i]][[1]])
      } else {
        if (is.null(right[[i]]) && !is.null(left[[i]])) return(FALSE)
        if (is.null(left[[i]]) && !is.null(right[[i]])) return(TRUE)
        if (is.null(left[[i]]) && is.null(right[[i]])) next
        for (j in seq_along(left[[i]])) {
          if (j > length(right[[i]])) return(FALSE)
          res <- compare_packets(left[[i]][[j]], right[[i]][[j]])
          if (isTRUE(res) || isFALSE(res)) return(res)
        }
        if (length(right[[i]]) > j) return(FALSE)
      }
    }
    if (!is.list(left[[i]]) && length(left[[i]]) == 1) left[[i]] <- list(left[[i]])
    if (!is.list(right[[i]]) && length(right[[i]]) == 1) right[[i]] <- list(right[[i]])
    if (is.list(left[[i]]) && is.list(right[[i]])) {
      res <- compare_packets(left[[i]], right[[i]])
      if (isTRUE(res) || isFALSE(res)) return(res)
    }
    if (isTRUE(res) | isFALSE(res)) return(res)
  }
  if (length(right) > i) return(TRUE)
}

# adapted from https://www.codertime.org/r-how-to-write-bubble-sort/
bubble_sort <- function(x) {
  swap_performed <- TRUE
  # Repeat the algorithm until no more swaps are performed
  while (swap_performed) {
    swap_performed <- FALSE
    # Check if any swaps are necessary
    for (i in 1:(length(x) - 1)) {
      if (compare_packets(x[i], x[i+1])) {
      # if (x[i] > x[i + 1]) {
        # Swap elements that are not in increasing order
        tmp <- x[i]
        x[i] <- x[i + 1]
        x[i + 1] <- tmp
        # Now record that a swap was performed
        # This requests another pass through the while loop
        swap_performed <- TRUE
      }
    }
  }
  # Output: the vector sorted in increasing order
  return(x)
}


#' @param example Which example data to use (by position or name). Defaults to
#'   1.
#' @rdname day13
#' @export
example_data_13 <- function(example = 1) {
  l <- list(
    a = c(
          "[1,1,3,1,1]",
          "[1,1,5,1,1]",
          "",
          "[[1],[2,3,4]]",
          "[[1],4]",
          "",
          "[9]",
          "[[8,7,6]]",
          "",
          "[[4,4],4,4]",
          "[[4,4],4,4,4]",
          "",
          "[7,7,7,7]",
          "[7,7,7]",
          "",
          "[]",
          "[3]",
          "",
          "[[[]]]",
          "[[]]",
          "",
          "[1,[2,[3,[4,[5,6,7]]]],8,9]",
          "[1,[2,[3,[4,[5,6,0]]]],8,9]"
    )
  )
  l[[example]]
}
