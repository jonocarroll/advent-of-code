#' @export
ex <- function(n) {
  # for i in {1..25}; do touch "inst/example${(l(2)(0))i}.txt"; done
  # for i in {1..25}; do touch "inst/example${(l(2)(0))i}b.txt"; done
  day <- if (grepl("b", n, fixed = TRUE)) {
    n <- sub("b", "", n)
    paste0(sprintf("%02d", as.integer(n)), "b")
  } else {
    sprintf("%02d", as.integer(n))
  }
  readLines(paste0("inst/example", day,".txt"))
}

#' @export
input <- function(n) {
  day <- sprintf("%02d", as.integer(n))
  readLines(paste0("inst/input", day,".txt"))
}
