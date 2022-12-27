library(adventofcode22)
x <- readLines("./inst/input08.txt")

p1 <- f08a(x)
p2 <- f08b(x)

stopifnot(p1 == aoc_solutions$day08a)
stopifnot(p2 == aoc_solutions$day08b)
