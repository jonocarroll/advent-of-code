library(adventofcode22)
x <- readLines("./inst/input06.txt")

p1 <- f06a(x)
p2 <- f06b(x)

stopifnot(p1 == aoc_solutions$day06a)
stopifnot(p2 == aoc_solutions$day06b)
