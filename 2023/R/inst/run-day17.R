library(adventofcode2023)
x <- readLines("./inst/input17.txt")

p1 <- f17a(x, 1:3)
p2 <- f17b(x, 4:10)

stopifnot(p1 == aoc_solutions$day17a)
stopifnot(p2 == aoc_solutions$day17b)
