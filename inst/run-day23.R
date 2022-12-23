library(adventofcode22)
x <- readLines("./inst/input23.txt")

p1 <- f23a(x)
p2 <- f23b(x)

stopifnot(p1 == aoc_solutions$day23a)
stopifnot(p2 == aoc_solutions$day23b)
