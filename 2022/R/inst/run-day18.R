library(adventofcode22)
x <- as.matrix(read.csv("./inst/input18.txt", header = FALSE))

p1 <- f18a(x)
p2 <- f18b(x)

stopifnot(p1 == aoc_solutions$day18a)
stopifnot(p2 == aoc_solutions$day18b)
