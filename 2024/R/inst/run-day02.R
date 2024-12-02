library(adventofcode24)
x <- readLines("./inst/input02.txt")

p1 <- f02a(x)
p2 <- f02b(x)

stopifnot(p1 == aoc_solutions$day02a)
stopifnot(p2 == aoc_solutions$day02b)
