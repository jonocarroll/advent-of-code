library(adventofcode22)
x <- readLines("./inst/input17.txt")

p1 <- f17a(x)
p2 <- f17b(x)

stopifnot(p1 == aoc_solutions$day17a)
stopifnot(p2 == aoc_solutions$day17b)
