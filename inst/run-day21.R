library(adventofcode22)
x <- readLines("./inst/input21.txt")

p1 <- f21a(x)
p2 <- f21b(x)

stopifnot(p1 == aoc_solutions$day21a)
stopifnot(p2 == aoc_solutions$day21b)
