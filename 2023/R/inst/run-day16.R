library(adventofcode2023)
x <- readLines("./inst/input16.txt")

p1 <- f16a(x)
p2 <- f16b(x)

stopifnot(p1 == aoc_solutions$day16a)
stopifnot(p2 == aoc_solutions$day16b)
