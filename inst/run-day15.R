library(adventofcode22)
x <- readLines("./inst/input15.txt")

p1 <- f15a(x, at = 2e6)
p2 <- f15b(x, maxsize = 4e6)

# ret = 3293021; i = 3230812

stopifnot(p1 == aoc_solutions$day15a)
stopifnot(p2 == aoc_solutions$day15b)
