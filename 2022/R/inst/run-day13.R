library(adventofcode22)
x <- readLines("./inst/input13.txt")

p1 <- f13a(x)
p2 <- f13b(x)

stopifnot(p1 == aoc_solutions$day13a)
stopifnot(p2 == aoc_solutions$day13b)
