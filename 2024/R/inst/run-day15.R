library(adventofcode24)
x <- readLines("./inst/input15.txt")

p1 <- f15a(x)
p2 <- f15b(x)

stopifnot(p1 == aoc_solutions$day15a)
stopifnot(p2 == aoc_solutions$day15b)
