library(adventofcode2023)
x <- readLines("./inst/input24.txt")

p1 <- f24a(x)
p2 <- f24b(x)

stopifnot(p1 == aoc_solutions$day24a)
stopifnot(p2 == aoc_solutions$day24b)
