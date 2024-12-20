library(adventofcode24)
x <- readLines("./inst/input20.txt")

p1 <- f20a(x)
p2 <- f20b(x)

stopifnot(p1 == aoc_solutions$day20a)
stopifnot(p2 == aoc_solutions$day20b)
