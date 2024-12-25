library(adventofcode24)
x <- readLines("./inst/input25.txt")

p1 <- f25a(x)
p2 <- f25b(x)

stopifnot(p1 == aoc_solutions$day25a)
stopifnot(p2 == aoc_solutions$day25b)
