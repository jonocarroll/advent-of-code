library(adventofcode24)
x <- readLines("./inst/input14.txt")

p1 <- f14a(x)
p2 <- f14b(x)

stopifnot(p1 == aoc_solutions$day14a)
stopifnot(p2 == aoc_solutions$day14b)
