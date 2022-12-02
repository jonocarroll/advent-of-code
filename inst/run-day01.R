library(adventofcode22)
x <- readLines("./inst/input01.txt")
x <- paste(x, collapse = "\n")

p1 <- f01a(x)
p2 <- f01b(x)

stopifnot(p1 == aoc_solutions$day01a)
stopifnot(p2 == aoc_solutions$day01b)
