library(adventofcode22)
x <- readLines("./inst/input19.txt")

p1 <- f19a(x)
p2 <- f19b(x)

stopifnot(p1 == aoc_solutions$day19a)
stopifnot(p2 == aoc_solutions$day19b)
