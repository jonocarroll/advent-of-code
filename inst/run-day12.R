library(adventofcode22)
x <- readLines("./inst/input12.txt")

p1 <- f12a(x)
p2 <- f12b(x)

stopifnot(p1 == aoc_solutions$day12a)
stopifnot(p2 == aoc_solutions$day12b)
