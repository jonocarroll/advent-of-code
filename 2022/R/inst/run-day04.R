library(adventofcode22)
x <- readLines("./inst/input04.txt")

p1 <- f04a(x)
p2 <- f04b(x)

stopifnot(p1 == aoc_solutions$day04a)
stopifnot(p2 == aoc_solutions$day04b)
