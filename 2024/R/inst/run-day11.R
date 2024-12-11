library(adventofcode24)
x <- readLines("./inst/input11.txt")

p1 <- f11a(x)
p2 <- f11b(x)

stopifnot(p1 == aoc_solutions$day11a)
stopifnot(p2 == aoc_solutions$day11b)
