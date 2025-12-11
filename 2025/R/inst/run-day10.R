library(adventofcode2025)
x <- readLines("./inst/input10.txt")

p1 <- f10a(x)
p2 <- f10b(x)

stopifnot(p1 == aoc_solutions$day10a)
stopifnot(p2 == aoc_solutions$day10b)
